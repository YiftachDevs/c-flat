use core::panic;
use std::{any::Any, collections::HashMap};

use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, errors::CompilerError, function_lowerer, parser::{ExprNode, ExprNodeEnum, Function, Literal, PostfixOpr, Scope, Span, Statement, Struct}};

pub struct IRExprValueResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: Option<BasicValueEnum<'ctx>>
}

pub enum IRExprResult<'ctx> {
    Value(IRExprValueResult<'ctx>),
    CommaSeperated(Box<ExprNode>, Box<ExprNode>),
    Type(IRTypeId),
    Function(IRFunctionId),
    FunctionName(IRScopeId, String),
    StructName(IRScopeId, String),
    EnumName(IRScopeId, String),
    ModuleScope(IRScopeId),
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn r_value(&mut self, expr_result: &IRExprValueResult<'ctx>) -> Option<BasicValueEnum<'ctx>> { // might cause bugs
        if let Some(expr_value) = expr_result.llvm_value {
            Some(if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.ir_type(expr_result.type_id).llvm_type.unwrap();
                self.build_load(llvm_type, ptr_value)
            } else {
                expr_value
            })
        } else {
            None
        }
    }

    pub fn build_load(&self, llvm_type: BasicTypeEnum<'ctx>, ptr_value: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        let load_value = self.builder.build_load(llvm_type, ptr_value, "tmp_load").unwrap();
        load_value
    }

    pub fn lower_scope(&mut self, ir_context: &mut IRContext<'ctx>, scope: &Scope, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        for statement in scope.statements.iter() {
            match statement {
                Statement::VarDeclaration(var) => {
                    let ir_var = if let Some(init_expr) = var.init_expr.as_ref() {
                        let ctx_type = if let Some(var_type) = var.var_type.as_ref() { Some(self.get_type(ir_context, var_type, context_type)?) } else { None };
                        let expr_result = self.lower_expr(ir_context, init_expr, ctx_type)?;
                        let expr_value_result = self.ensure_expr_result_value(expr_result, init_expr.span, ctx_type)?;
                        let var_type = expr_value_result.type_id;
                        if let Some(expected_type) = ctx_type && expected_type != var_type {
                            let expected_type_string: String = self.format_type(expected_type);
                            let received_type_string: String = self.format_type(var_type);
                            let err_description: String = format!("Type mismatch during var declaration, expected: {}, received: {}", expected_type_string, received_type_string);
                            return Err(self.error("Type mismatch", Some(err_description), Some(var.span)));
                        }
                        let llvm_value = if self.is_type_zero_sized(var_type)? { None } else {
                            let llvm_value = expr_value_result.llvm_value.unwrap();
                            Some(llvm_value) 
                        };
                        IRVariable { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut, llvm_value: llvm_value }
                    } else {
                        let var_type = self.get_type(ir_context, var.var_type.as_ref().unwrap(), context_type)?;
                        let llvm_ptr = self.get_alloca(var_type, &var.name).into();
                        IRVariable { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut, llvm_value: Some(llvm_ptr) }
                    };
                    ir_context.into_fun_context().vars.push(ir_var);
                },
                Statement::Expression { expr, is_final_value } => {
                    let expr_result = self.lower_expr(ir_context, expr, context_type)?;
                    let expr_result_value = self.ensure_expr_result_value(expr_result, expr.span, context_type)?;
                    if *is_final_value {
                        return Ok(expr_result_value);
                    }
                }
                _ => { todo!() }
            }
        }

        let scope_type = self.primitive_type(PrimitiveType::Void)?;
        Ok(IRExprValueResult { type_id: scope_type, llvm_value: None })
    }

    pub fn ensure_expr_result_value(&mut self, expr_result: IRExprResult<'ctx>, span: Span, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value) = expr_result {
            if let Some(ctx_t) = context_type {
                if ctx_t != expr_value.type_id {
                    let err_description: String = format!("Expected '{}', received '{}'", self.format_type(ctx_t), self.format_type(expr_value.type_id));
                    return Err(self.error("Type mismatch", Some(err_description), Some(span)));
                }
            }
            return Ok(expr_value);
        }
        return Err(self.error("Expression does not have a value", None, Some(span)));
    }

    pub fn ensure_expr_result_type(&mut self, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRTypeId, CompilerError> {
        if let IRExprResult::Type(type_id) = expr_result {
            return Ok(type_id);
        }
        return Err(self.error("Expression is not a type", None, Some(span)));
    }

    pub fn get_type(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRTypeId, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, context_type)?;
        self.ensure_expr_result_type(expr_result, expr.span)
    }

    pub fn get_value(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, context_type)?;
        self.ensure_expr_result_value(expr_result, expr.span, context_type)
    }

    pub fn lower_expr(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let result = match &expr.value {
            ExprNodeEnum::Literal(literal) => {
                IRExprResult::Value(self.lower_expr_literal(literal, context_type, expr.span)?)
            },
            ExprNodeEnum::Scope(scope) => {
                IRExprResult::Value(self.lower_scope(ir_context, scope, context_type)?)
            },
            ExprNodeEnum::Name(name, _) => {
                self.lower_expr_first_name(ir_context, name, expr.span, context_type)?
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                self.lower_postfix_opr(ir_context, *opr, left_expr, right_expr, context_type)?
            },
            ExprNodeEnum::InfixOpr(opr, left_expr, right_expr) => {
                // TEMPORARY just for commas testing,
                IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone())
            }
            _ => todo!("lower_expr 2")
        };
        Ok(result)
    }


    fn lower_postfix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PostfixOpr, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        match opr {
            PostfixOpr::Mem => {
                self.lower_postfix_opr_member(ir_context, left_expr, right_expr, context_type)
            },
            PostfixOpr::Inv => {
                self.lower_postfix_opr_invoke(ir_context, left_expr, right_expr, context_type)
            },
            PostfixOpr::Con => {
                self.lower_postfix_opr_constructor(ir_context, left_expr, right_expr, context_type)
            },
            PostfixOpr::Tmp => {
                self.lower_postfix_opr_templates(ir_context, left_expr, right_expr, context_type)
            }
            _ => todo!("lower_postix_opr 6")
        }
    }

    fn merge_templates_keys_values(&self, templates_keys: &Vec<IRTemplateKey>, templates_values: &Vec<IRTemplateValue>, values_span: Span) -> Result<IRTemplatesMap, CompilerError> {
        if templates_keys.len() != templates_values.len() {
            return Err(self.error(format!("Expected {} templates", templates_keys.len()).as_str(), None, Some(values_span)));
        }
        let mut result = IRTemplatesMap::new();
        for i in 0..templates_keys.len() {
            result.insert(templates_keys[i].clone(), templates_values[i].clone());
        }
        Ok(result)
    }

    fn lower_postfix_opr_templates(&mut self, ir_context: &mut IRContext<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_context, left_expr, context_type)?;
        let right_expr = right_expr.as_ref().unwrap();
        let templates_values = self.lower_args_types(ir_context, right_expr)?;
        if let IRExprResult::FunctionName(parent_scope, fun_name) = left_expr_result {
            let fun_def = self.find_fun_def_in_scope(parent_scope, fun_name.as_str()).unwrap().1;
            let templates_keys = self.get_templates_keys_from(&fun_def.templates)?;
            let templates_map = self.merge_templates_keys_values(&templates_keys, &templates_values, right_expr.span)?;
            let fun_id = self.lower_fun(parent_scope, fun_name.as_str(), templates_map, Some(left_expr.span))?;
            Ok(IRExprResult::Function(fun_id))
        } else if let IRExprResult::StructName(parent_scope, struct_name) = left_expr_result {
            let struct_def = self.find_struct_def_in_scope(parent_scope, struct_name.as_str()).unwrap().1;
            let templates_keys = self.get_templates_keys_from(&struct_def.templates)?;
            let templates_map = self.merge_templates_keys_values(&templates_keys, &templates_values, right_expr.span)?;
            let struct_id = self.lower_struct(parent_scope, struct_name.as_str(), templates_map, Some(left_expr.span))?;
            Ok(IRExprResult::Type(struct_id))
        } else {
            return Err(self.error("Expected a struct / enum / function", None, Some(left_expr.span)));
        }
    }

    fn lower_postfix_opr_member(&mut self, ir_context: &mut IRContext<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_context, left_expr, context_type)?;
        let right_expr = right_expr.as_ref().unwrap();
        let right_expr_name  = if let ExprNodeEnum::Name(name, _) = &right_expr.value { name.clone() } else {
            return Err(self.error("Expected a name", None, Some(right_expr.span)));
        };
        match left_expr_result {
            IRExprResult::ModuleScope(scope) => {
                if let Some(new_scope) = self.get_module_scope_in_scope(scope, right_expr_name.as_str()) {
                    Ok(IRExprResult::ModuleScope(new_scope))
                } else if let Some((fun_scope, fun_def)) = self.find_fun_def_in_scope(scope, right_expr_name.as_str()) {
                    return Ok(self.lower_fun_name(fun_scope, fun_def, right_expr.span)?);
                } else if let Some((struct_scope, struct_def)) = self.find_struct_def_in_scope(scope, right_expr_name.as_str()) {
                    return Ok(self.lower_struct_name(struct_scope, struct_def, right_expr.span)?);
                } else {
                    return Err(self.error("Missing module / function / type", None, Some(right_expr.span)));
                }
            },
            IRExprResult::Value(expr_value_result) => {
                let ir_type = self.ir_type(expr_value_result.type_id);
                match &ir_type.type_enum {
                    IRTypeEnum::Struct { parent_scope, scope, templates_map, args, def, vars_built } => {
                        if let Some((i, arg)) = args.iter().enumerate().find(|(_, arg)| arg.name == right_expr_name) {
                            let llvm_value = expr_value_result.llvm_value.unwrap();
                            let arg_name = format!("{}.{}", llvm_value.get_name().to_string_lossy(), arg.name);
                            let arg_value = match llvm_value {
                                BasicValueEnum::PointerValue(ptr_value) => {
                                    self.builder.build_struct_gep(ir_type.llvm_type.unwrap(), ptr_value, i as u32, arg_name.as_str()).unwrap().into()
                                },
                                BasicValueEnum::StructValue(struct_value) => {
                                    self.builder.build_extract_value(struct_value, i as u32, arg_name.as_str()).unwrap()
                                },
                                _ => panic!("lower_postfix_opr_member 2")
                            };
                
                            return Ok(IRExprResult::Value(IRExprValueResult { type_id: arg.type_id, llvm_value: Some(arg_value) }));
                        } else {
                            return Err(self.error("Missing struct member / function", None, Some(right_expr.span)));
                        }
                    },
                    _ => todo!("lower_postfix_opr_member 3")
                }
            }
            _ => todo!("lower_postfix_opr_member 4")
        }
    }

    fn lower_args_values(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Option<Box<ExprNode>>, context_types: &Vec<IRTypeId>) -> Result<Vec<BasicValueEnum<'ctx>>, CompilerError> {
        let args_len = context_types.len();
        let mut llvm_args: Vec<BasicValueEnum> = Vec::new();
        let mut i = 0;
        if let Some(expr) = args_expr {
            let mut cur_expr = expr.clone();
            while i < args_len {
                let context_type = Some(context_types[i]);
                let expr_result = self.lower_expr(ir_context, cur_expr.as_ref(), context_type)?;
                i += 1;
                if let IRExprResult::CommaSeperated(left_expr, right_expr) = expr_result {
                    let left_expr_result = self.lower_expr(ir_context, left_expr.as_ref(), context_type)?;
                    let value_result = self.ensure_expr_result_value(left_expr_result, left_expr.span, context_type)?;
                    let llvm_value = self.r_value(&value_result).unwrap(); // does not support void / never args.
                    llvm_args.push(llvm_value);
                    cur_expr = right_expr;
                } else if let IRExprResult::Value(_) = expr_result {
                    let final_arg_value = self.ensure_expr_result_value(expr_result, cur_expr.span, context_type)?;
                    let llvm_value = self.r_value(&final_arg_value).unwrap(); // does not support void / never args.
                    llvm_args.push(llvm_value);

                    if i < args_len {
                        return Err(self.error("Missing arguments", None, Some(cur_expr.span)));
                    }
                } else {
                    return Err(self.error("Expression does not have a value", None, Some(cur_expr.span)));
                }
            }
            if i > args_len {
                return Err(self.error("Too many arguments", None, Some(cur_expr.span)));
            }
        }
        Ok(llvm_args)
    }

    fn lower_args_types(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>) -> Result<Vec<IRTypeId>, CompilerError> {
        let mut ir_types: Vec<IRTypeId> = Vec::new();
        let mut cur_expr = args_expr.clone();
        loop {
            let expr_result = self.lower_expr(ir_context, cur_expr.as_ref(), None)?;
            if let IRExprResult::CommaSeperated(left_expr, right_expr) = expr_result {
                let left_expr_result = self.lower_expr(ir_context, left_expr.as_ref(), None)?;
                let type_result = self.ensure_expr_result_type(left_expr_result, cur_expr.span)?;
                ir_types.push(type_result);
                cur_expr = right_expr;
            } else if let IRExprResult::Type(type_result) = expr_result {
                ir_types.push(type_result);
                break;
            } else {
                return Err(self.error("Expression is not a type", None, Some(cur_expr.span)));
            }
        }
        Ok(ir_types)
    }

    fn lower_postfix_opr_invoke(&mut self, ir_context: &mut IRContext<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_context, left_expr, context_type)?;
        if let IRExprResult::Function(fun_id) = left_expr_result {
            let args_context_types = self.ir_function(fun_id).args.iter().map(|arg| arg.type_id).collect::<Vec<IRTypeId>>();
            let llvm_args = self.lower_args_values(ir_context, right_expr, &args_context_types)?;
            let llvm_args = llvm_args.iter().map(|llvm_arg| (*llvm_arg).into()).collect::<Vec<BasicMetadataValueEnum>>();
            let ir_fun = if let IRExprResult::Function(fun_id) = left_expr_result { self.ir_function(fun_id) } else { panic!("lower_postix_opr 4") };
            let fun_call = self.builder.build_call(ir_fun.llvm_value, &llvm_args, "fun_call_tmp").unwrap();
            let ret_value = fun_call.try_as_basic_value().basic();
            Ok(IRExprResult::Value(IRExprValueResult { type_id: ir_fun.return_type, llvm_value: ret_value }))
        } else {
            todo!("lower_postix_opr 5")
        }
    }

    fn lower_postfix_opr_constructor(&mut self, ir_context: &mut IRContext<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result: IRExprResult<'_> = self.lower_expr(ir_context, left_expr, context_type)?;
        if let IRExprResult::Type(type_id) = left_expr_result {
            match &self.ir_type(type_id).type_enum {
                IRTypeEnum::Struct { parent_scope, scope, templates_map, args, def, vars_built } => {
                    let context_types = args.iter().map(|arg| arg.type_id).collect::<Vec<IRTypeId>>();
                    let def_args = def.vars.as_ref().unwrap();
                    let llvm_args = self.lower_args_values(ir_context, right_expr, &context_types)?;
                    let mut llvm_value: inkwell::values::AggregateValueEnum<'_> = self.ir_type(type_id).llvm_type.unwrap().into_struct_type().get_undef().into();
                    let basic_value = llvm_value.as_basic_value_enum();
                    for (i, llvm_arg) in llvm_args.iter().enumerate() {
                        let arg_name: String = format!("{}.{}", basic_value.get_name().to_string_lossy(), def_args.variables[i].name);
                        llvm_value = self.builder.build_insert_value(llvm_value, *llvm_arg, i as u32, arg_name.as_str()).unwrap();
                    }
                    Ok(IRExprResult::Value(IRExprValueResult { type_id, llvm_value: Some(llvm_value.as_basic_value_enum()) }))
                },
                _ => todo!("lower_postfix_opr_constructor")
            }
            
        } else {
            return Err(self.error("Expected a constructable type", None, Some(left_expr.span)));
        }
    }

    fn lower_expr_first_name(&mut self, ir_context: &mut IRContext<'ctx>, name: &str, span: Span, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContext::FunContext(fun_context) = ir_context {
            if let Some(var) = fun_context.vars.iter().find(|var| var.name == name) {
                return Ok(IRExprResult::Value(IRExprValueResult{ type_id: var.type_id, llvm_value: var.llvm_value }));
            }
            let fun_scope = self.ir_function(fun_context.fun).parent_scope;
            if let Some((actual_scope, fun_def)) = self.find_fun_def_in_scope(fun_scope, name) {
                return Ok(self.lower_fun_name(actual_scope, fun_def, span)?);
            }
        }
        let scope = self.get_context_scope(ir_context);
        if let Some((actual_scope, struct_def)) = self.find_struct_def_in_scope(scope, name) {
            return Ok(self.lower_struct_name(actual_scope, struct_def, span)?);
        }
        if let Some(module) = self.get_module_scope_in_scope(scope, name) {
            return Ok(IRExprResult::ModuleScope(module));
        }
        if let Some(prim_t) = self.primitive_type_from(name) {
            return Ok(IRExprResult::Type(self.primitive_type(prim_t)?));
        }
        if let Some((_, v)) = self.ir_scope(scope).templates_map.iter().find(|(k, v)| k.name == name) {
            return Ok(IRExprResult::Type(*v));
        }
        if let IRContext::ImplDefContext(parent_scope, matches, templates_map) = ir_context {
            for template_match in matches.iter() {
                if template_match.name == name {
                    todo!("lower_expr_first_name")
                }
            }
        }
        return Err(self.error("Unrecognized name", None, Some(span)));
    }

    fn lower_fun_name(&mut self, scope: IRScopeId, fun_def: &Function, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let Some(_) = fun_def.templates {
            return Ok(IRExprResult::FunctionName(scope, fun_def.name.clone()));
        } else {
            return Ok(IRExprResult::Function(self.lower_fun(scope, fun_def.name.as_str(), IndexMap::new(), Some(span))?));
        }
    }

    fn lower_struct_name(&mut self, scope: IRScopeId, struct_def: &Struct, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let Some(_) = struct_def.templates {
            return Ok(IRExprResult::StructName(scope, struct_def.name.clone()));
        } else {
            return Ok(IRExprResult::Type(self.lower_struct(scope, struct_def.name.as_str(), IndexMap::new(), Some(span))?));
        }
    }

    fn lower_expr_literal(&mut self, literal: &Literal, context_type: Option<IRTypeId>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let result = match &literal {
            Literal::Bool(v) => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Bool)?, llvm_value: Some(self.llvm_context.bool_type().const_int(*v as u64, false).into()) }
            },
            Literal::Char(ch) => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Char)?, llvm_value: Some(self.llvm_context.i32_type().const_int(*ch as u64, false).into())}
            },
            Literal::UnresolvedInteger(int) => {
                let default = (self.primitive_type(PrimitiveType::I32)?, self.llvm_context.i32_type());
                let (int_type_id, llvm_int_type) = if let Some(ctx_t) = context_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(ctx_t).type_enum && (prim.is_int() | prim.is_uint()) {
                        (self.primitive_type(prim)?, self.ir_type(ctx_t).llvm_type.unwrap().into_int_type())
                    } else {
                        default
                    }
                } else {
                    default
                };
                IRExprValueResult{ type_id: int_type_id, llvm_value: Some(llvm_int_type.const_int(*int as u64, false).into()) }
            },
            Literal::Float(float) => {
                let default = (self.primitive_type(PrimitiveType::F32)?, self.llvm_context.f32_type());
                let (float_type_id, llvm_float_type) = if let Some(ctx_t) = context_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(ctx_t).type_enum && prim.is_float() {
                        (self.primitive_type(prim)?, self.ir_type(ctx_t).llvm_type.unwrap().into_float_type())
                    } else {
                        default
                    }
                } else {
                    default
                };                  
                IRExprValueResult{ type_id: float_type_id, llvm_value: Some(llvm_float_type.const_float(*float).into()) }
            },
            Literal::Void => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Void)?, llvm_value: None }
            }
            _ => todo!()
        };
        Ok(result)
    }
}