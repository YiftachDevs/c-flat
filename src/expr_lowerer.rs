use core::panic;
use std::{any::Any, collections::HashMap};

use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, errors::CompilerError, function_lowerer::IRFunScope, parser::{ExprNode, ExprNodeEnum, Function, Literal, PostfixOpr, PrimitiveType, Scope, Span, Statement, VarType}};

pub struct IRExprValueResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: Option<BasicValueEnum<'ctx>>
}

pub enum IRExprResult<'ctx> {
    Value(IRExprValueResult<'ctx>),
    Results(Vec<IRExprResult<'ctx>>),
    Type(IRTypeId),
    Function(IRFunctionId),
    FunctionName(IRScopeId, String),
    TypeName(IRScopeId, String),
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
            ExprNodeEnum::Name(name) => { // could be either template, module, struct, enum. (also fun name if after comes a struct / enum or recusively another fun name)
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
                        let expr_result = self.lower_expr_value(ir_fun_scope, init_expr, ctx_type)?;
                        let var_type = expr_result.type_id;
                        if let Some(expected_type) = ctx_type && expected_type != var_type {
                            let expected_type_string: String = self.format_type(expected_type);
                            let received_type_string: String = self.format_type(var_type);
                            let err_description: String = format!("Type mismatch during var declaration, expected: {}, received: {}", expected_type_string, received_type_string);
                            return Err(self.error("Type mismatch", Some(err_description), Some(var.span)));
                        }
                        let llvm_ptr = if self.is_type_zero_sized(var_type)? { None } else {
                            let llvm_ptr = self.get_alloca(var_type, &var.name);
                            self.builder.build_store(llvm_ptr, expr_result.llvm_value.unwrap()).unwrap();
                            Some(llvm_ptr)
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
                    let expr_result = self.lower_expr_value(ir_fun_scope, expr, context_type)?;
                    if *is_final_value {
                        return Ok(expr_result);
                    }
                }
                _ => { todo!() }
            }
        }

        let scope_type = self.primitive_type(PrimitiveType::Void)?;
        Ok(IRExprValueResult { type_id: scope_type, llvm_value: None })
    }


    pub fn lower_expr_value(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value_result) = self.lower_expr(ir_fun_scope, expr, context_type)? {
            return Ok(expr_value_result);
        }
        return Err(self.error("Expression does not have a value", None, Some(expr.span)));
    }

    pub fn lower_expr(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        match &expr.value {
            ExprNodeEnum::Literal(literal) => {
                Ok(IRExprResult::Value(self.lower_expr_literal(literal, context_type)?))
            },
            ExprNodeEnum::Scope(scope) => {
                Ok(IRExprResult::Value(self.lower_scope(ir_fun_scope, scope, context_type)?))
            },
            ExprNodeEnum::Name(name) => {
                self.lower_expr_name(ir_fun_scope, name, expr.span)
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                self.lower_postfix_opr(ir_fun_scope, *opr, left_expr, right_expr, context_type)
            }
            _ => todo!("lower_expr 2")
        }
    }

    fn lower_exprs(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, expr: &ExprNode, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
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
    }

    fn lower_postfix_opr(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>, opr: PostfixOpr, left_expr: &ExprNode, right_expr: &Option<ExprNode>, context_type: Option<IRTypeId>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_fun_scope, left_expr, context_type)?;

        match opr {
            PostfixOpr::Mem => {
                let right_expr = right_expr.as_ref().unwrap();
                let right_expr_name  = if let ExprNodeEnum::Name(name) = &right_expr.value { name.clone() } else {
                    return Err(self.error("Expected a name", None, Some(right_expr.span)));
                };
                match left_expr_result {
                    IRExprResult::ModuleScope(scope) => {
                        if let Some(new_scope) = self.get_module_scope_in_scope(scope, right_expr_name.as_str()) {
                            Ok(IRExprResult::ModuleScope(new_scope))
                        } else if let Some(fun_def) = self.find_fun_def_in_scope(scope, right_expr_name.as_str()) {
                            return Ok(self.lower_fun_name(scope, fun_def, right_expr.span)?);
                        } else {
                            return Err(self.error("Missing module / function / type", None, Some(right_expr.span)));
                        }
                    },
                    _ => todo!()
                }
            },
            PostfixOpr::Tmp => { // redo this whole thing
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
            },
            PostfixOpr::Inv => {
                let mut llvm_args = Vec::new();
                for value_result in self.lower_exprs_values(ir_fun_scope, right_expr.as_ref().unwrap(), context_type)? {
                    llvm_args.push(value_result.llvm_value.unwrap().into());
                }
                let ir_fun = if let IRExprResult::Function(fun_id) = left_expr_result { self.ir_function(fun_id) } else { panic!("lower_postix_opr 4") };
                let fun_call = self.builder.build_call(ir_fun.llvm_value, &llvm_args, "fun_call_tmp").unwrap();
                let ret_value = fun_call.try_as_basic_value().basic();
                Ok(IRExprResult::Value(IRExprValueResult { type_id: ir_fun.return_type, llvm_value: ret_value }))
            }
            _ => todo!("lower_postix_opr 5")
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