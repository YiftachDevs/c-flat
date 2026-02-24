use core::panic;
use std::{any::Any, collections::HashMap};

use inkwell::{types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, errors::CompilerError, function_lowerer::IRFunScope, parser::{ExprNode, ExprNodeEnum, Function, Literal, PostfixOpr, PrimitiveType, Scope, Span, Statement, Struct, VarType}};

pub struct IRExprValueResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: Option<BasicValueEnum<'ctx>>
}

pub enum IRExprResult<'ctx> {
    Value(IRExprValueResult<'ctx>),
    CommaSeperated(Box<IRExprResult<'ctx>>, Box<ExprNode>),
    Type(IRTypeId),
    Function(IRFunctionId),
    FunctionName(IRScopeId, String),
    StructName(IRScopeId, String),
    EnumName(IRScopeId, String),
    ModuleScope(IRScopeId)
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn r_value(&mut self, expr_result: &IRExprValueResult<'ctx>) -> Option<BasicValueEnum<'ctx>> { // might cause bugs
        if let Some(expr_value) = expr_result.llvm_value {
            Some(if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.ir_type(expr_result.type_id).llvm_type.unwrap();
                self.builder.build_load(llvm_type, ptr_value, "side_load").unwrap()
            } else {
                expr_value
            })
        } else {
            None
        }
    }

    pub fn get_type_from_expr(&self, ctx_scope: IRScopeId, expr: &ExprNode) -> Result<IRTypeId, CompilerError> {
        let ir_ctx_scope: &IRScope<'_> = self.ir_scope(ctx_scope);
        match &expr.value {
            ExprNodeEnum::Name(name, _) => { // could be either template, module, struct, enum. (also fun name if after comes a struct / enum or recusively another fun name)
                for template_key in ir_ctx_scope.templates_values.keys() {
                    if let IRTemplateKey::Type(type_template_key_name) = template_key {
                        if type_template_key_name == name && let IRTemplateValue::Type(type_id) = ir_ctx_scope.templates_values[template_key] {
                            return Ok(type_id);
                        }
                    }
                }
                todo!("CodeLowerer::get_type_from_expr 1")
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                todo!("CodeLowerer::get_type_from_expr 2")

            },
            _ => todo!("CodeLowerer::get_type_from_expr 3")
        }
    }

    pub fn lower_scope(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, scope: &Scope, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let ctx_scope = self.ir_function(ir_fun_scope.fun).scope;

        for statement in scope.statements.iter() {
            match statement {
                Statement::VarDeclaration(var) => {
                    let ir_var = if let Some(init_expr) = var.init_expr.as_ref() {
                        let ctx_type = if var.var_type == VarType::UnresolvedInitExpr { None } else { Some(self.get_type_from(ctx_scope, &var.var_type)?) };
                        let expr_result = self.lower_expr(ir_fun_scope, init_expr, ctx_type)?;
                        let expr_value_result = self.ensure_expr_result_value(expr_result, init_expr.span)?;
                        let var_type = expr_value_result.type_id;
                        if let Some(expected_type) = ctx_type && expected_type != var_type {
                            let expected_type_string: String = self.format_type(expected_type);
                            let received_type_string: String = self.format_type(var_type);
                            let err_description: String = format!("Type mismatch during var declaration, expected: {}, received: {}", expected_type_string, received_type_string);
                            return Err(self.error("Type mismatch", Some(err_description), Some(var.span)));
                        }
                        let llvm_ptr = if self.is_type_zero_sized(var_type)? { None } else {
                            if let BasicValueEnum::PointerValue(ptr_value) = expr_value_result.llvm_value.unwrap()  {
                                Some(ptr_value)
                            } else {
                                let llvm_ptr = self.get_alloca(var_type, &var.name);
                                self.builder.build_store(llvm_ptr, expr_value_result.llvm_value.unwrap()).unwrap();
                                Some(llvm_ptr)
                            }
                        };
                        IRVariable { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut, llvm_ptr: llvm_ptr }
                    } else {
                        let var_type = self.get_type_from(ctx_scope, &var.var_type)?;
                        let llvm_ptr = self.get_alloca(var_type, &var.name);
                        IRVariable { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut, llvm_ptr: Some(llvm_ptr) }
                    };
                    ir_fun_scope.vars.push(ir_var);
                },
                Statement::Expression { expr, is_final_value } => {
                    let expr_result = self.lower_expr(ir_fun_scope, expr, context_type)?;
                    let expr_result_value = self.ensure_expr_result_value(expr_result, expr.span)?;
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


    /*pub fn lower_expr_value(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value_result) = self.lower_expr(ir_fun_scope, expr, context_type)? {
            return Ok(expr_value_result);
        }
        return Err(self.error("Expression does not have a value", None, Some(expr.span)));
    }*/

    pub fn ensure_expr_result_value(&mut self, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value_result) = expr_result {
            return Ok(expr_value_result);
        }
        return Err(self.error("Expression does not have a value", None, Some(span)));
    }

    pub fn lower_expr(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        match &expr.value {
            ExprNodeEnum::Literal(literal) => {
                Ok(IRExprResult::Value(self.lower_expr_literal(literal, context_type)?))
            },
            ExprNodeEnum::Scope(scope) => {
                Ok(IRExprResult::Value(self.lower_scope(ir_fun_scope, scope, context_type)?))
            },
            ExprNodeEnum::Name(name, _) => {
                self.lower_expr_name(ir_fun_scope, name, expr.span)
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                self.lower_postfix_opr(ir_fun_scope, *opr, left_expr, right_expr, context_type)
            },
            ExprNodeEnum::InfixOpr(opr, left_expr, right_expr) => {
                // TEMPORARY just for commas testing,
                Ok(IRExprResult::CommaSeperated(Box::new(self.lower_expr(ir_fun_scope, left_expr, context_type)?), right_expr.clone()))
            }
            _ => todo!("lower_expr 2")
        }
    }

    /*fn lower_exprs(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let expr = self.lower_expr(ir_fun_scope, expr, context_type)?;
        if let IRExprResult::Results(_) = expr {
            return Ok(expr);
        }
        return Ok(IRExprResult::Results(vec![expr]));
    }

    fn lower_exprs_values(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<Vec<IRExprValueResult<'ctx>>, CompilerError> {
        let mut values = Vec::new();
        if let IRExprResult::Results(results) = self.lower_exprs(ir_fun_scope, expr, context_type)? {
            for result in results {
                if let IRExprResult::Value(value) = result {
                    values.push(value);
                } else {
                    return Err(self.error("Expression does not have a value", None, Some(expr.span)));
                }
            }
        }
        return Ok(values);
    }*/

    fn lower_postfix_opr(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, opr: PostfixOpr, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        match opr {
            PostfixOpr::Mem => {
                self.lower_postfix_opr_member(ir_fun_scope, left_expr, right_expr, context_type)
            },
            PostfixOpr::Inv => {
                self.lower_postfix_opr_invoke(ir_fun_scope, left_expr, right_expr, context_type)
            },
            PostfixOpr::Con => {
                self.lower_postfix_opr_constructor(ir_fun_scope, left_expr, right_expr, context_type)
            }
            /*PostfixOpr::Tmp => { // redo this whole thing
                let right_expr = right_expr.as_ref().unwrap();
                if let IRExprResult::Results(templates_results) = self.lower_exprs(ir_fun_scope, right_expr, None)? {
                    let mut templates_values = HashMap::new();
                    let (parent_scope, name, templates_keys) = match left_expr_result {
                        IRExprResult::FunctionName(parent_scope, fun_name) => {
                            let fun_def = self.find_fun_def_in_scope(parent_scope, fun_name.as_str()).unwrap();
                            (parent_scope, fun_name, self.get_templates_keys_from(parent_scope, fun_def.templates.as_ref().unwrap())?) // does not support generics relying on each other eg: fun foo<T, const size: T> needs to be fixed
                        },
                        IRExprResult::TypeName(parent_scope, type_name) => {
                            todo!("lower_postfix_opr 1")
                        },
                        _ => return Err(self.error("Expected a type or a function", None, Some(left_expr.span)))
                    };
                    for (i, template_result) in templates_results.iter().enumerate() {
                        if let IRExprResult::Type(type_id) = template_result {
                            templates_values.insert(templates_keys[i].clone(), IRTemplateValue::Type(*type_id));
                        } else {
                            todo!("lower_postfix_opr 2")
                        }
                    }
                    let fun_id = self.lower_fun(parent_scope, name.as_str(), templates_values, Some(right_expr.span))?;
                    Ok(IRExprResult::Function(fun_id))
                } else { panic!("lower_postix_opr 3") }
            },*/
            _ => todo!("lower_postix_opr 6")
        }
    }

    fn lower_postfix_opr_member(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_fun_scope, left_expr, context_type)?;
        let right_expr = right_expr.as_ref().unwrap();
        let right_expr_name  = if let ExprNodeEnum::Name(name, _) = &right_expr.value { name.clone() } else {
            return Err(self.error("Expected a name", None, Some(right_expr.span)));
        };
        match left_expr_result {
            IRExprResult::ModuleScope(scope) => {
                if let Some(new_scope) = self.get_module_scope_in_scope(scope, right_expr_name.as_str()) {
                    Ok(IRExprResult::ModuleScope(new_scope))
                } else if let Some(fun_def) = self.find_fun_def_in_scope(scope, right_expr_name.as_str()) {
                    return Ok(self.lower_fun_name(scope, fun_def, right_expr.span)?);
                } else if let Some(struct_def) = self.find_struct_def_in_scope(scope, right_expr_name.as_str()) {
                    return Ok(self.lower_struct_name(scope, struct_def, right_expr.span)?);
                } else {
                    return Err(self.error("Missing module / function / type", None, Some(right_expr.span)));
                }
            },
            _ => todo!()
        }
    }

    fn lower_args_values(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, args_expr: &Option<Box<ExprNode>>, context_types: &Vec<IRTypeId>) -> Result<Vec<BasicValueEnum<'ctx>>, CompilerError> {
        let args_len = context_types.len();
        let mut llvm_args: Vec<BasicValueEnum> = Vec::new();
        let mut i = 0;
        if let Some(expr) = args_expr {
            let mut cur_expr = expr.clone();
            while i < args_len {
                let expr_result = self.lower_expr(ir_fun_scope, cur_expr.as_ref(), Some(context_types[i]))?;
                i += 1;
                if let IRExprResult::CommaSeperated(left_result, right_expr) = expr_result {
                    let value_result = self.ensure_expr_result_value(*left_result, cur_expr.span)?;
                    let llvm_value = self.r_value(&value_result).unwrap(); // does not support void / never args.
                    llvm_args.push(llvm_value);
                    cur_expr = right_expr;
                } else if let IRExprResult::Value(final_arg_value) = expr_result {
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

    fn lower_postfix_opr_invoke(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_fun_scope, left_expr, context_type)?;
        if let IRExprResult::Function(fun_id) = left_expr_result {
            let args_context_types = self.ir_function(fun_id).args.iter().map(|arg| arg.type_id).collect::<Vec<IRTypeId>>();
            let llvm_args = self.lower_args_values(ir_fun_scope, right_expr, &args_context_types)?;
            let llvm_args = llvm_args.iter().map(|llvm_arg| (*llvm_arg).into()).collect::<Vec<BasicMetadataValueEnum>>();
            let ir_fun = if let IRExprResult::Function(fun_id) = left_expr_result { self.ir_function(fun_id) } else { panic!("lower_postix_opr 4") };
            let fun_call = self.builder.build_call(ir_fun.llvm_value, &llvm_args, "fun_call_tmp").unwrap();
            let ret_value = fun_call.try_as_basic_value().basic();
            Ok(IRExprResult::Value(IRExprValueResult { type_id: ir_fun.return_type, llvm_value: ret_value }))
        } else {
            todo!("lower_postix_opr 5")
        }
    }

    fn lower_postfix_opr_constructor(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, left_expr: &Box<ExprNode>, right_expr: &Option<Box<ExprNode>>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result: IRExprResult<'_> = self.lower_expr(ir_fun_scope, left_expr, context_type)?;
        if let IRExprResult::Type(type_id) = left_expr_result {
            match &self.ir_type(type_id).type_enum {
                IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built } => {
                    let context_types = args.iter().map(|arg| arg.type_id).collect::<Vec<IRTypeId>>();
                    let llvm_args = self.lower_args_values(ir_fun_scope, right_expr, &context_types)?;
                    let llvm_ptr = self.get_alloca(type_id, "tmp_struct_constructor_rename_me");
                    for (i, llvm_arg) in llvm_args.iter().enumerate() {
                        let arg_ptr = self.builder.build_struct_gep(self.ir_type(type_id).llvm_type.unwrap(), llvm_ptr, i as u32, "struct_arg_rename_me").unwrap();
                        self.builder.build_store(arg_ptr, *llvm_arg).unwrap();
                    }
                    Ok(IRExprResult::Value(IRExprValueResult { type_id, llvm_value: Some(llvm_ptr.into()) }))
                },
                _ => todo!("lower_postfix_opr_constructor")
            }
            
        } else {
            return Err(self.error("Expected a constructable type", None, Some(left_expr.span)));
        }
    }

    fn lower_expr_name(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, name: &str, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let Some(var) = ir_fun_scope.vars.iter().find(|var| var.name == name) {
            let llvm_value: Option<BasicValueEnum<'_>> = if let Some(ptr_value) = var.llvm_ptr { Some(ptr_value.into()) } else { None };
            return Ok(IRExprResult::Value(IRExprValueResult{ type_id: var.type_id, llvm_value: llvm_value }));
        }
        let global_scope = self.get_global_scope();
        if let Some(fun_def) = self.find_fun_def_in_scope(global_scope, name) {
            return Ok(self.lower_fun_name(global_scope, fun_def, span)?);
        }
        if let Some(struct_def) = self.find_struct_def_in_scope(global_scope, name) {
            return Ok(self.lower_struct_name(global_scope, struct_def, span)?);
        }
        if let Some(module) = self.get_module_scope_in_scope(global_scope, name) {
            return Ok(IRExprResult::ModuleScope(module));
        }
        return Err(self.error("Unrecognized name", None, Some(span)));
    }

    fn lower_fun_name(&mut self, scope: IRScopeId, fun_def: &Function, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let Some(_) = fun_def.templates {
            return Ok(IRExprResult::FunctionName(scope, fun_def.name.clone()));
        } else {
            return Ok(IRExprResult::Function(self.lower_fun(scope, fun_def.name.as_str(), HashMap::new(), Some(span))?));
        }
    }

    fn lower_struct_name(&mut self, scope: IRScopeId, struct_def: &Struct, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let Some(_) = struct_def.templates {
            return Ok(IRExprResult::StructName(scope, struct_def.name.clone()));
        } else {
            return Ok(IRExprResult::Type(self.lower_struct(scope, struct_def.name.as_str(), HashMap::new(), Some(span))?));
        }
    }

    fn lower_expr_literal(&mut self, literal: &Literal, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        match &literal {
            Literal::Bool(v) => {
                Ok(IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Bool)?, llvm_value: Some(self.llvm_context.bool_type().const_int(*v as u64, false).into()) })
            },
            Literal::Char(ch) => {
                Ok(IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Char)?, llvm_value: Some(self.llvm_context.i32_type().const_int(*ch as u64, false).into())})
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
                Ok(IRExprValueResult{ type_id: int_type_id, llvm_value: Some(llvm_int_type.const_int(*int as u64, false).into()) })
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
                Ok(IRExprValueResult{ type_id: float_type_id, llvm_value: Some(llvm_float_type.const_float(*float).into()) })
            },
            Literal::Void => {
                Ok(IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Void)?, llvm_value: None }) 
            }
            _ => todo!()
        }
    }
}