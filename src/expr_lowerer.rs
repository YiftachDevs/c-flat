use core::panic;
use std::{any::Any, collections::HashMap};

use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, conditional_lowerer, errors::{CompilerError, SemanticError}, function_lowerer, parser::{ExprNode, ExprNodeEnum, Function, Literal, PostfixOpr, Scope, Span, Statement, Struct, Templates, Trait}};

#[derive(Copy, Clone, PartialEq)]
pub struct IRExprValueResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: BasicValueEnum<'ctx>
}

#[derive(Clone, PartialEq)]
pub struct IRExprPlaceResult<'ctx> {
    pub type_id: IRTypeId,
    pub ptr_value: PointerValue<'ctx>,
    pub is_mut: bool,
    pub var: String
}

#[derive(PartialEq)]
pub enum IRExprResult<'ctx> {
    Empty,
    Void,
    Place(IRExprPlaceResult<'ctx>),
    Value(IRExprValueResult<'ctx>),
    CommaSeperated(Box<ExprNode>, Box<ExprNode>),
    Type(IRTypeId),
    Trait(IRTraitId),
    Function(IRFunctionId, Option<Box<IRExprResult<'ctx>>>),
    FunctionName(IRScopeId, String, Option<Box<IRExprResult<'ctx>>>, Option<IRImplId>),
    StructName(IRScopeId, String),
    TraitName(IRScopeId, String, IRTypeId),
    EnumName(IRScopeId, String),
    ModuleScope(IRScopeId),
    NoImplMatch
}

impl<'ctx> IRExprResult<'ctx> {
    pub fn to_string_temp(&self) -> &'static str {
        match self {
            IRExprResult::Empty => "Empty",
            IRExprResult::Void => "Void",
            IRExprResult::Var(_) => "Var",
            IRExprResult::Value(_) => "Value",
            IRExprResult::CommaSeperated(_, _) => "CommaSeperated",
            IRExprResult::Type(_) => "Type",
            IRExprResult::Trait(_) => "Trait",
            IRExprResult::Function(_, _) => "Function",
            IRExprResult::FunctionName(_, _, _, _) => "FunctionName",
            IRExprResult::StructName(_, _) => "StructName",
            IRExprResult::TraitName(_, _, _) => "TraitName",
            IRExprResult::EnumName(_, _) => "EnumName",
            IRExprResult::ModuleScope(_) => "ModuleScope",
            IRExprResult::NoImplMatch => "NoImplMatch"
        }
    }
}

impl<'ctx> CodeLowerer<'ctx> {
    /*pub fn r_value(&mut self, expr_result: &IRExprValueResult<'ctx>) -> IRExprValueResult<'ctx> { // might cause bugs
        if let Some(expr_value) = expr_result.llvm_value {
            if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.ir_type(expr_result.type_id).llvm_type.unwrap();
                IRExprValueResult { type_id: expr_result.type_id, llvm_value: Some(self.build_load(llvm_type, ptr_value))}
            } else {
                expr_result.clone()
            }
        } else {
            expr_result.clone()
        }
    }*/

    /*pub fn build_load(&self, llvm_type: BasicTypeEnum<'ctx>, ptr_value: PointerValue<'ctx>) -> BasicValueEnum<'ctx> {
        let load_value = self.builder.build_load(llvm_type, ptr_value, "tmp_load").unwrap();
        load_value
    }*/

    pub fn lower_scope(&mut self, ir_context: &mut IRContext<'ctx>, scope: &Scope, context_type: &IRContextType) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let prev_vars_len = ir_context.into_fun_context().vars.len();
    
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let mut scope_result = self.get_type_zero(void_type);

        for (i, statement) in scope.statements.iter().enumerate() {
            let is_final_statement = i == scope.statements.len() - 1;
            let ctx_t = if is_final_statement { context_type } else { &IRContextType::Any }; 
            match statement {
                Statement::VarDeclaration(var) => {
                    let ir_var = if let Some(init_expr) = var.init_expr.as_ref() {
                        let ctx_type = if let Some(var_type) = var.var_type.as_ref() { IRContextType::Type(self.get_type(ir_context, var_type, &IRContextType::Any)?) } else { IRContextType::Any };
                        let expr_result = self.get_value(ir_context, init_expr, &ctx_type, true)?;
                        let var_dec = IRVarDeclaration { name: var.name.clone(), type_id: expr_result.type_id, is_mut: var.is_mut };
                        let place = self.alloc_place(&var_dec);
                        self.builder.build_store(place.ptr_value, expr_result.llvm_value).unwrap();
                        IRVariable { name: var.name.clone(), place, moved: false }
                    } else {
                        let var_type = self.get_type(ir_context, var.var_type.as_ref().unwrap(), &IRContextType::Any)?;
                        let var_dec = IRVarDeclaration { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut };
                        let place = self.alloc_place(&var_dec).into();
                        IRVariable { name: var.name.clone(), place, moved: false }
                    };
                    ir_context.into_fun_context().vars.push(ir_var);
                },
                Statement::Expression { expr, is_final_value } => {
                    if *is_final_value {
                        scope_result = self.get_value(ir_context, expr, context_type, true)?;
                        break;
                    } else {
                        let expr_result = self.get_value(ir_context, expr, &IRContextType::Any, false)?;
                        if expr_result.type_id == self.primitive_type(PrimitiveType::Never)? {
                            scope_result = expr_result;
                            break;
                        }
                    }
                },
                Statement::ConditionalChain(conditional_chain) => {
                    let expr_result = self.lower_conditional_chain(ir_context, conditional_chain, ctx_t)?;
                    if is_final_statement || expr_result.type_id == self.primitive_type(PrimitiveType::Never)? {
                        scope_result = expr_result;
                        break;
                    }
                },
                Statement::ControlFlow(control_flow) => {
                    scope_result = self.lower_control_flow(ir_context, control_flow, ctx_t)?;
                    break;
                }
            }
        }

        ir_context.into_fun_context().vars.drain(prev_vars_len..);

        self.ensure_type_matches(scope_result.type_id, context_type, Some(scope.span))?;
        Ok(scope_result)
    }

    pub fn ensure_expr_result_value(&mut self, expr_result: &IRExprResult<'ctx>, ensure_type: bool, span: Span, context_type: &IRContextType) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value) = expr_result {
            if ensure_type {
                self.ensure_type_matches(expr_value.type_id, context_type, Some(span))?;
            }
            return Ok(*expr_value );
        }
        if let IRExprResult::Void = expr_result {
            let void_type =  self.primitive_type(PrimitiveType::Void)?;
            let void_value = self.get_type_zero(void_type);
            return self.ensure_expr_result_value(&IRExprResult::Value(void_value), ensure_type, span, context_type);
        }
        return Err(self.error(SemanticError::ExpectedValueExpr, Some(span)));
    }

    pub fn ensure_expr_result_type(&mut self, expr_result: &IRExprResult<'ctx>, span: Span, context_type: &IRContextType) -> Result<IRTypeId, CompilerError> {
        if let IRExprResult::Type(type_id) = expr_result {
            self.ensure_type_matches(*type_id, context_type, Some(span))?;
            return Ok(*type_id);
        }
        if let IRExprResult::Void = expr_result {
            return Ok(self.primitive_type(PrimitiveType::Void)?);
        }
        return Err(self.error(SemanticError::ExpectedTypeExpr, Some(span)));
    }

    pub fn ensure_expr_result_trait(&mut self, expr_result: &IRExprResult<'ctx>, span: Span, context_type: &IRContextType) -> Result<IRTraitId, CompilerError> {
        if let IRExprResult::Trait(trait_id) = expr_result {
            let ir_trait = self.ir_trait(*trait_id);
            self.ensure_type_matches(ir_trait.self_type, context_type, Some(span))?;
            return Ok(*trait_id);
        }
        return Err(self.error(SemanticError::ExpectedTrait, Some(span)));
    }

    pub fn get_type(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: &IRContextType) -> Result<IRTypeId, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, context_type)?;
        self.ensure_expr_result_type(&expr_result, expr.span, context_type)
    }

    pub fn get_value(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: &IRContextType, ensure_type: bool) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, context_type)?;
        self.ensure_expr_result_value(&expr_result, ensure_type, expr.span, context_type)
    }

    pub fn get_trait(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: &IRContextType) -> Result<IRTraitId, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, context_type)?;
        self.ensure_expr_result_trait(&expr_result, expr.span, context_type)
    }

    pub fn lower_expr(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
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
                self.lower_infix_opr(ir_context, *opr, left_expr, right_expr, expr.span, context_type)?
            },
            ExprNodeEnum::PrefixOpr(opr, right_expr) => {
                self.lower_prefix_opr(ir_context, *opr, right_expr, expr.span, context_type)?
            },
            ExprNodeEnum::ConditionalChain(conditional_chain) => {
                IRExprResult::Value(self.lower_conditional_chain(ir_context, conditional_chain, context_type)?)
            },
            ExprNodeEnum::Void => {
                if let IRContext::FunContext(_) = ir_context {
                    IRExprResult::Void
                } else {
                    IRExprResult::Type(self.ensure_expr_result_type(&IRExprResult::Void, expr.span, &IRContextType::Any)?)
                }
            },
            ExprNodeEnum::Empty => IRExprResult::Empty,
            _ => todo!("lower_expr 2")
        };
        Ok(result)
    }

    pub fn merge_templates_keys_values(&self, templates_keys: &[IRTemplateKey], templates_values: &[IRTemplateValue], values_span: Option<Span>) -> Result<IRTemplatesMap, CompilerError> {
        if templates_keys.len() != templates_values.len() {
            return Err(self.error(SemanticError::UnmatchedTemplateCount { expected: templates_keys.len() }, values_span));
        }
        let mut result: IndexMap<String, usize> = IRTemplatesMap::new();
        for i in 0..templates_keys.len() {
            result.insert(templates_keys[i].clone(), templates_values[i].clone());
        }
        Ok(result)
    }

    fn lower_expr_first_name(&mut self, ir_context: &mut IRContext<'ctx>, name: &str, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContext::FunContext(fun_context) = ir_context {
            if let Some(var) = fun_context.vars.iter().find(|var| var.name == name) {
                if var.moved {
                    return Err(self.error(SemanticError::MovedValue, Some(span)));
                }
                return Ok(IRExprResult::Var(var.name.clone()));
            }
            let search_scope = self.ir_function(fun_context.fun).parent_scope;
            if let Some(fun_result) = self.lower_fun_name(search_scope, name, span)? {
                return Ok(fun_result);
            }
        } else if let IRContextType::Impl(ctx_t) = context_type && let IRContext::ImplDefContext(parent_scope, matches, templates_map) = ir_context {
            for template_match in matches.iter() {
                if template_match == name {
                    templates_map.insert(template_match.clone(), *ctx_t);
                    return Ok(IRExprResult::Type(*ctx_t));
                }
            }
        }
        let scope = self.get_context_scope(ir_context);
        if let Some(result) = self.lower_struct_name(scope, name, span)? {
            return Ok(result);
        }
        if let IRContextType::Type(self_type) = context_type && let Some(result) = self.lower_trait_name(scope, name, *self_type, span)? {
            return Ok(result);
        }
        if let Some(module) = self.get_module_scope_in_scope(scope, name) {
            return Ok(IRExprResult::ModuleScope(module));
        }
        if let Some(prim_t) = self.primitive_type_from(name) {
            return Ok(IRExprResult::Type(self.primitive_type(prim_t)?));
        }
        if let Some((_, v)) = self.ir_scope(scope).templates_map.iter().find(|(key, v)| **key == name) {
            return Ok(IRExprResult::Type(*v));
        }
        return Err(self.error(SemanticError::UnrecognizedName, Some(span)));
    }

    pub fn lower_fun_name(&mut self, search_scope: IRScopeId, fun_name: &str, span: Span) -> Result<Option<IRExprResult<'ctx>>, CompilerError> {
        if let Some((fun_scope, fun_def)) = self.find_fun_def_in_scope(search_scope, fun_name, Some(span))? {
            if !fun_def.templates.templates.is_empty() {
                return Ok(Some(IRExprResult::FunctionName(fun_scope, fun_name.to_string(), None, None)));
            } else {
                return Ok(Some(IRExprResult::Function(self.lower_fun(fun_scope, fun_name, &Vec::new(), Some(span))?, None)));
            }
        }
        Ok(None)
    }

    pub fn lower_impl_fun_name(&mut self, impl_id: IRImplId, fun_name: &str, opt_self_value: Option<IRExprValueResult<'ctx>>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let ir_impl = self.ir_impl(impl_id);
        let fun_def = ir_impl.ast_def.scope.as_ref().unwrap().functions.iter().find(|fun| fun.name == fun_name).unwrap();
        if !fun_def.templates.templates.is_empty() {
            return Ok(IRExprResult::FunctionName(ir_impl.scope, fun_name.to_string(), opt_self_value,  Some(impl_id)));
        } else {
            let fun = self.lower_fun(ir_impl.scope, fun_def.name.as_str(), &Vec::new(), Some(span))?;
            self.ensure_trait_fun_valid(impl_id, fun, Some(span))?;
            return Ok(IRExprResult::Function(fun, opt_self_value));
        }
    }

    pub fn lower_struct_name(&mut self, search_scope: IRScopeId, struct_name: &str, span: Span) -> Result<Option<IRExprResult<'ctx>>, CompilerError> {
        if let Some((struct_scope, struct_def)) = self.find_struct_def_in_scope(search_scope, struct_name, Some(span))? {
            if !struct_def.templates.templates.is_empty() {
                return Ok(Some(IRExprResult::StructName(struct_scope, struct_name.to_string())));
            } else {
                return Ok(Some(IRExprResult::Type(self.lower_struct(struct_scope, struct_name, &Vec::new(), Some(span))?)));
            }
        }
        Ok(None)
    }

    pub fn lower_trait_name(&mut self, search_scope: IRScopeId, trait_name: &str, self_type: IRTypeId, span: Span) -> Result<Option<IRExprResult<'ctx>>, CompilerError> {
        if let Some((trait_scope, trait_def)) = self.find_trait_def_in_scope(search_scope, trait_name, Some(span))? {
            if !trait_def.templates.templates.is_empty() {
                return Ok(Some(IRExprResult::TraitName(trait_scope, trait_name.to_string(), self_type)));
            } else {
                return Ok(Some(IRExprResult::Trait(self.lower_trait(trait_scope, trait_name, &Vec::new(), self_type, Some(span))?)));
            }
        }
        Ok(None)
    }

    fn lower_expr_literal(&mut self, literal: &Literal, context_type: &IRContextType, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let result = match &literal {
            Literal::Bool(v) => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Bool)?, llvm_value: self.llvm_context.bool_type().const_int(*v as u64, false).into() }
            },
            Literal::Char(ch) => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Char)?, llvm_value: self.llvm_context.i32_type().const_int(*ch as u64, false).into() }
            },
            Literal::UnresolvedInteger(int) => {
                let default = (self.primitive_type(PrimitiveType::I32)?, self.llvm_context.i32_type());
                let (int_type_id, llvm_int_type) = if let IRContextType::Type(ctx_t) = context_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(*ctx_t).type_enum && (prim.is_int() | prim.is_uint()) {
                        (self.primitive_type(prim)?, self.ir_type(*ctx_t).llvm_type.into_int_type())
                    } else {
                        default
                    }
                } else {
                    default
                };
                IRExprValueResult{ type_id: int_type_id, llvm_value: llvm_int_type.const_int(*int as u64, false).into() }
            },
            Literal::Float(float) => {
                let default = (self.primitive_type(PrimitiveType::F32)?, self.llvm_context.f32_type());
                let (float_type_id, llvm_float_type) = if let IRContextType::Type(ctx_t) = context_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(*ctx_t).type_enum && prim.is_float() {
                        (self.primitive_type(prim)?, self.ir_type(*ctx_t).llvm_type.into_float_type())
                    } else {
                        default
                    }
                } else {
                    default
                };                  
                IRExprValueResult{ type_id: float_type_id, llvm_value: llvm_float_type.const_float(*float).into() }
            },
            Literal::Void => {
                let void_type = self.primitive_type(PrimitiveType::Void)?;
                self.get_type_zero(void_type)
            }
            _ => todo!()
        };
        Ok(result)
    }

    pub fn lower_args(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, context_types: &[IRContextType]) -> Result<Vec<(IRExprResult<'ctx>, Span)>, CompilerError> {
        let args_len = context_types.len();
        let mut args: Vec<(IRExprResult<'ctx>, Span)> = Vec::new();
        let mut i: usize = 0;
        let mut cur_expr = args_expr.clone();
        while i < args_len {
            let context_type = context_types.get(i).unwrap_or(&context_types[0]);
            let expr_result = self.lower_expr(ir_context, cur_expr.as_ref(), context_type)?;
            if let IRExprResult::Empty = expr_result {
                break;
            } else if let IRExprResult::CommaSeperated(left_expr, right_expr) = expr_result {
                let arg_result = self.lower_expr(ir_context, left_expr.as_ref(), context_type)?;
                args.push((arg_result, left_expr.span));
                if i + 1 >= args_len {
                    i += 2;
                    break;
                }
                cur_expr = right_expr;
            } else {
                args.push((expr_result, cur_expr.span));
                i += 1;
                break;
            }
            i += 1;
        }
        if i != args_len {
            return Err(self.error(SemanticError::UnmatchedArgCount { expected: args_len }, Some(cur_expr.span)));
        }
        Ok(args)
    }

    pub fn load_var(&mut self, ir_var: &'ctx mut IRVariable<'ctx>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        ir_var.moved = true;
    }

    pub fn lower_args_values(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, context_types: &[IRContextType]) -> Result<Vec<IRExprValueResult<'ctx>>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, context_types)?;
        let mut args_values = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            args_values.push(self.ensure_expr_result_value(expr_result, true, *span, &context_types[i])?);
        }
        Ok(args_values)
    }

    pub fn lower_args_types(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, context_types: &[IRContextType]) -> Result<Vec<IRTypeId>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, context_types)?;
        let mut args_types = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            args_types.push(self.ensure_expr_result_type(expr_result, *span, &context_types[i])?);
        }
        Ok(args_types)
    }

    pub fn lower_args_traits(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, context_types: &[IRContextType]) -> Result<Vec<IRTraitId>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, context_types)?;
        let mut args_types = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            args_types.push(self.ensure_expr_result_trait(expr_result, *span, &context_types[i])?);
        }
        Ok(args_types)
    }
}