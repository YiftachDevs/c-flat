use core::panic;
use std::{any::{Any, TypeId}, collections::HashMap};

use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, conditional_lowerer, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, function_lowerer, parser::{Const, ExprNode, ExprNodeEnum, Function, Literal, PostfixOpr, Scope, Span, Statement, Struct, Template, Templates, Trait}};

#[derive(Copy, Clone, PartialEq)]
pub struct IRExprValueResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: BasicValueEnum<'ctx>
}

#[derive(Clone, PartialEq)]
pub struct IRExprPlaceResult<'ctx> {
    pub type_id: IRTypeId,
    pub ptr_value: BasicValueEnum<'ctx>,
    pub is_mut: bool,
    pub owner: Option<IRVarId>
}

#[derive(Clone, PartialEq)]
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
    ModuleScope(IRScopeId),
    NoImplMatch,
    NoTypeConstraintMatch,
    TemplatesValues(Vec<IRTemplateValue<'ctx>>)
}

impl<'ctx> IRExprResult<'ctx> {
    pub fn to_string_temp(&self) -> &'static str {
        match self {
            IRExprResult::Empty => "Empty",
            IRExprResult::Void => "Void",
            IRExprResult::Place(_) => "Place",
            IRExprResult::Value(_) => "Value",
            IRExprResult::CommaSeperated(_, _) => "CommaSeperated",
            IRExprResult::Type(_) => "Type",
            IRExprResult::Trait(_) => "Trait",
            IRExprResult::Function(_, _) => "Function",
            IRExprResult::FunctionName(_, _, _, _) => "FunctionName",
            IRExprResult::StructName(_, _) => "StructName",
            IRExprResult::TraitName(_, _, _) => "TraitName",
            IRExprResult::ModuleScope(_) => "ModuleScope",
            IRExprResult::NoImplMatch => "NoImplMatch",
            IRExprResult::NoTypeConstraintMatch => "NoTypeConstraintMatch",
            IRExprResult::TemplatesValues(_) => "TemplatesValues"
        }
    }

    pub fn is_place_or_value(&self) -> bool {
        match self {
            IRExprResult::Place(_) => true,
            IRExprResult::Value(_) => true,
            _ => false
        }
    }

    pub fn get_type_id(&self) -> IRTypeId {
        match &self { IRExprResult::Value(value) => value.type_id, IRExprResult::Place(place) => place.type_id, _ => panic!() }
    }
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_scope(&mut self, ir_context: &mut IRContext<'ctx>, scope: &Scope, expr_context: &IRExprContext<'ctx>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let prev_vars_len = ir_context.into_fun_context().vars.len();
    
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let mut scope_result = self.get_type_zero(void_type);

        for (i, statement) in scope.statements.iter().enumerate() {
            let is_final_statement = i == scope.statements.len() - 1;
            let ctx_t = if is_final_statement { expr_context } else { &IRExprContext::Value(None) }; 
            match statement {
                Statement::VarDeclaration(var) => {
                    if let Some(init_expr) = var.init_expr.as_ref() {
                        let ctx_type = if let Some(var_type) = var.var_type.as_ref() { IRExprContext::Value(Some(self.get_type(ir_context, var_type, &IRExprContext::Type)?)) } else { IRExprContext::Value(None) };
                        let expr_result = self.get_value(ir_context, init_expr, &ctx_type, true)?;
                        let var_dec = IRVarDeclaration { name: var.name.clone(), type_id: expr_result.type_id, is_mut: var.is_mut, is_static: var.is_static };
                        self.alloc_var(ir_context.into_fun_context(), &var_dec, Some(expr_result.llvm_value), Some(var.span))?;
                    } else {
                        let var_type = self.get_type(ir_context, var.var_type.as_ref().unwrap(), &IRExprContext::Type)?;
                        let var_dec = IRVarDeclaration { name: var.name.clone(), type_id: var_type, is_mut: var.is_mut, is_static: var.is_static };
                        self.alloc_var(ir_context.into_fun_context(), &var_dec, None, Some(var.span))?;
                    }
                },
                Statement::Expression { expr, is_final_value } => {
                    if *is_final_value {
                        scope_result = self.get_value(ir_context, expr, expr_context, true)?;
                        break;
                    } else {
                        let expr_result = self.get_value(ir_context, expr, &IRExprContext::Value(None), false)?;
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
                    scope_result = self.lower_control_flow(ir_context, control_flow,)?;
                    break;
                }
                Statement::Const(const_def) => {
                    self.lower_constant(ir_context, const_def)?;
                }
            }
        }

        if scope_result.type_id != self.primitive_type(PrimitiveType::Never)? {
            self.drop_vars(ir_context, prev_vars_len, scope.span)?;
        }

        if let IRExprContext::Value(expected_type) = expr_context {
            scope_result.type_id = self.ensure_type_matches(scope_result.type_id, *expected_type, Some(scope.span), true)?;
        }
        Ok(scope_result)
    }

    pub fn lower_constant(&mut self, ir_context: &mut IRContext<'ctx>, const_def: &Const) -> Result<(), CompilerError> {
        let scope_id = self.get_context_scope(ir_context);

        let const_type = self.get_type(ir_context, &const_def.var_type, &IRExprContext::Type)?;

        if let None = const_def.init_expr {
            return Err(self.error(SemanticError::MissingInitExpr, Some(const_def.span)));
        }

        let const_value = self.get_constant(ir_context, const_def.init_expr.as_ref().unwrap(), &IRExprContext::Value(Some(const_type)))?;
        let ir_constant = IRConstant { name: const_def.name.clone(), value: const_value };

        self.scopes_table[scope_id].constants.push(ir_constant);

        Ok(())
    }

    pub fn lower_scope_constraint(&mut self, ir_context: &mut IRContext<'ctx>, scope: &Scope, expr_context: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRExprContext::TypeConstraint(type_id) = expr_context {
            let inner_expr = match &scope.statements[0] { Statement::Expression { expr, is_final_value } => expr, _ => panic!() };
            if let IRTypeEnum::Struct(ir_struct) = self.ir_type(*type_id).type_enum.clone() {
                for arg_dec in ir_struct.args.iter() {
                    if let IRExprResult::NoTypeConstraintMatch = self.lower_expr(ir_context, inner_expr, &IRExprContext::TypeConstraint(arg_dec.type_id))? {
                        return Ok(IRExprResult::NoTypeConstraintMatch);
                    }
                }
                return Ok(IRExprResult::Type(*type_id));
            } else { return Ok(IRExprResult::NoTypeConstraintMatch); }
        } else { panic!() }
    }

    pub fn drop_vars(&mut self, ir_context: &mut IRContext<'ctx>, prev_len: usize, span: Span) -> Result<(), CompilerError> {
        for i in (prev_len..ir_context.into_fun_context().vars.len()).rev() {
            let var = &ir_context.into_fun_context().vars[i];
            if var.moved {
                continue;
            }
            let place = var.place.clone();
            let core_scope = self.get_core_scope()?;
            let drop_trait = self.lower_trait(core_scope, "Drop", &Vec::new(), place.type_id, None)?;
            if self.type_impls_trait(place.type_id, drop_trait)? {
                self.call_core_trait(ir_context, IRExprResult::Place(place), None, &CoreTraitFun::Drop, span)?;
            }
        }
        ir_context.into_fun_context().vars.drain(prev_len..);
        Ok(())
    }
 
    pub fn ensure_expr_result_value(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: &IRExprResult<'ctx>, ensure_type: bool, span: Span, expr_context: &IRExprContext) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Value(expr_value) = expr_result.clone() {
            if ensure_type && let IRExprContext::Value(expected_type) = expr_context {
                self.ensure_type_matches(expr_value.type_id, *expected_type, Some(span), true)?;
            }
            return Ok(expr_value);
        }
        if let IRExprResult::Place(place) = expr_result.clone() {
            let mut value = self.load_place(ir_context, place, span)?;
            if ensure_type && let IRExprContext::Value(expected_type) = expr_context {
                value.type_id = self.ensure_type_matches(value.type_id, *expected_type, Some(span), true)?;
            }
            return Ok(value);
        }
        if let IRExprResult::Void = expr_result {
            let void_type =  self.primitive_type(PrimitiveType::Void)?;
            let void_value = self.get_type_zero(void_type);
            return self.ensure_expr_result_value(ir_context, &IRExprResult::Value(void_value), ensure_type, span, expr_context);
        }
        return Err(self.error(SemanticError::ExpectedValueExpr, Some(span)));
    }

    pub fn ensure_expr_result_place(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRExprPlaceResult<'ctx>, CompilerError> {
        if let IRExprResult::Place(place) = expr_result {
            Ok(place)
        } else {
            Err(self.error(SemanticError::ExpectedPlace, Some(span)))
        }
    }

    pub fn ensure_expr_result_type(&mut self, expr_result: &IRExprResult<'ctx>, span: Span) -> Result<IRTypeId, CompilerError> {
        if let IRExprResult::Type(type_id) = expr_result {
            return Ok(*type_id);
        }
        if let IRExprResult::Void = expr_result {
            return Ok(self.primitive_type(PrimitiveType::Void)?);
        }
        return Err(self.error(SemanticError::ExpectedTypeExpr, Some(span)));
    }

    pub fn ensure_expr_result_trait(&mut self, expr_result: &IRExprResult<'ctx>, span: Span, expr_context: &IRExprContext<'ctx>) -> Result<IRTraitId, CompilerError> {
        if let IRExprResult::Trait(trait_id) = expr_result {
            let ir_trait = self.ir_trait(*trait_id);
            if let IRExprContext::Trait(self_type) = expr_context {
                self.ensure_type_matches(ir_trait.self_type, Some(*self_type), Some(span), false)?;
            }
            return Ok(*trait_id);
        }
        return Err(self.error(SemanticError::ExpectedTrait, Some(span)));
    }

    pub fn ensure_expr_result_template_value(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: &IRExprResult<'ctx>, span: Span, expr_context: &IRExprContext<'ctx>) -> Result<IRTemplateValue<'ctx>, CompilerError> {
        if let IRExprContext::Template(template) = expr_context {
            match template {
                IRTemplate::Const(const_type) => {
                    let value = self.ensure_expr_result_value(ir_context, expr_result, true, span, &IRExprContext::Value(Some(*const_type)))?;
                    self.ensure_expr_value_result_constant(value, span)?;
                    Ok(IRTemplateValue::Const(value))
                },
                IRTemplate::Type => {
                    let type_id = self.ensure_expr_result_type(expr_result, span)?;
                    Ok(IRTemplateValue::Type(type_id))
                }
            }
        } else if let IRExprContext::Impl(template_value) = expr_context {
            Ok(template_value.clone())
        } else { panic!() }
    }

    pub fn ensure_expr_result_template_values(&mut self, expr_result: IRExprResult<'ctx>) -> Vec<IRTemplateValue<'ctx>> {
        if let IRExprResult::TemplatesValues(values) = expr_result {
            values
        } else { panic!() }
    }

    pub fn ensure_expr_value_result_constant(&mut self, value: IRExprValueResult<'ctx>, span: Span) -> Result<(), CompilerError> {
        if let IRTypeEnum::Primitive(_) = self.ir_type(value.type_id).type_enum {
            if value.llvm_value.is_const() {
                Ok(())
            } else {
                Err(self.error(SemanticError::ExpectdConstant, Some(span)))
            }
        } else {
            Err(self.error(SemanticError::NonPrimConstant, Some(span)))
        }
    }

    pub fn get_type(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, ir_expr_context: &IRExprContext<'ctx>) -> Result<IRTypeId, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, ir_expr_context)?;
        self.ensure_expr_result_type(&expr_result, expr.span)
    }

    pub fn get_value(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>, ensure_type: bool) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, expr_context)?;
        self.ensure_expr_result_value(ir_context, &expr_result, ensure_type, expr.span, expr_context)
    }

    pub fn get_place(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>) -> Result<IRExprPlaceResult<'ctx>, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, expr_context)?;
        self.ensure_expr_result_place(ir_context, expr_result, expr.span)
    }


    pub fn get_trait(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>) -> Result<IRTraitId, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, expr_context)?;
        self.ensure_expr_result_trait(&expr_result, expr.span, expr_context)
    }
    
    pub fn get_template_value(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>) -> Result<IRTemplateValue<'ctx>, CompilerError> {
        let expr_result = self.lower_expr(ir_context, expr, expr_context)?;
        self.ensure_expr_result_template_value(ir_context, &expr_result, expr.span, expr_context)
    }


    pub fn get_constant(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let value = self.get_value(ir_context, expr, expr_context, true)?;
        self.ensure_expr_value_result_constant(value, expr.span)?;
        Ok(value)
    }

    pub fn lower_expr(&mut self, ir_context: &mut IRContext<'ctx>, expr: &ExprNode, expr_context: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let result = match &expr.value {
            ExprNodeEnum::Literal(literal) => {
                IRExprResult::Value(self.lower_expr_literal(literal, expr_context)?)
            },
            ExprNodeEnum::Scope(scope) => {
                if let IRContext::ImplConstraintContext(cur_impl_id) = ir_context {
                    self.lower_scope_constraint(ir_context, scope, expr_context)?
                } else {
                    IRExprResult::Value(self.lower_scope(ir_context, scope, expr_context)?)
                }
            },
            ExprNodeEnum::Array(arr_expr, opt_size) => {
                self.lower_array(ir_context, arr_expr, opt_size, expr.span, expr_context)?
            },
            ExprNodeEnum::Name(name) => {
                self.lower_expr_first_name(ir_context, name, expr.span, expr_context)?
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                self.lower_postfix_opr(ir_context, *opr, left_expr, right_expr, expr.span, expr_context)?
            },
            ExprNodeEnum::InfixOpr(opr, left_expr, right_expr) => {
                self.lower_infix_opr(ir_context, *opr, left_expr, right_expr, expr.span, expr_context)?
            },
            ExprNodeEnum::PrefixOpr(opr, right_expr) => {
                self.lower_prefix_opr(ir_context, *opr, right_expr, expr.span, expr_context)?
            },
            ExprNodeEnum::ConditionalChain(conditional_chain) => {
                IRExprResult::Value(self.lower_conditional_chain(ir_context, conditional_chain, expr_context)?)
            },
            ExprNodeEnum::Void => {
                if let IRExprContext::Value(_) = expr_context {
                    let void_type = self.primitive_type(PrimitiveType::Void)?;
                    IRExprResult::Value(self.get_type_zero(void_type))
                } else {
                    IRExprResult::Type(self.primitive_type(PrimitiveType::Void)?)
                }
            },
            ExprNodeEnum::Brackets(expr) => self.lower_expr(ir_context, expr, expr_context)?,
            ExprNodeEnum::Empty => IRExprResult::Empty,
            _ => todo!("lower_expr 2")
        };
        if let IRExprContext::TypeConstraint(type_id) = expr_context && let IRExprResult::Trait(trait_id) = result && let IRContext::ImplConstraintContext(cur_impl_id) = ir_context {
            let mut type_impls_trait = false;
            self.lower_impls(*type_id)?;
            for impl_id in self.ir_type(*type_id).lowered_impls.as_ref().unwrap() {
                if impl_id == cur_impl_id {
                    continue;
                }
                if let Some(cur_trait) = self.ir_impl(*impl_id).trait_id && cur_trait == trait_id {
                    type_impls_trait = true;
                    break;
                }
            }
            if !type_impls_trait {
                return Ok(IRExprResult::NoTypeConstraintMatch);
            }
        }
        Ok(result)
    }

    pub fn merge_templates_keys_values(&self, templates_keys: &[IRTemplateKey], templates_values: &[IRTemplateValue<'ctx>], values_span: Option<Span>) -> Result<IRTemplatesMap<'ctx>, CompilerError> {
        if templates_keys.len() != templates_values.len() {
            return Err(self.error(SemanticError::UnmatchedTemplateCount { expected: templates_keys.len() }, values_span));
        }
        let mut result = IRTemplatesMap::new();
        for i in 0..templates_keys.len() {
            result.insert(templates_keys[i].clone(), templates_values[i].clone());
        }
        Ok(result)
    }

    fn lower_expr_first_name(&mut self, ir_context: &mut IRContext<'ctx>, name: &str, span: Span, expr_context: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContext::FunContext(fun_context) = ir_context {
            if let IRExprContext::Value(_) = expr_context && let Some(var) = fun_context.vars.iter().find(|var| var.name == name) {
                if var.moved {
                    return Err(self.error(SemanticError::MovedValue, Some(span)));
                }
                return Ok(IRExprResult::Place(var.place.clone()));
            }
            let search_scope = self.ir_function(fun_context.fun).parent_scope;
            if let IRExprContext::Function = expr_context {
                return if let Some(fun_result) = self.lower_fun_name(search_scope, name, span)? {
                    Ok(fun_result)
                } else {
                    Err(self.error(SemanticError::ExpectedFunction, Some(span)))
                };
            }
        } else if let IRExprContext::Impl(ctx_template_value) = expr_context && let IRContext::ImplDefContext(parent_scope, matches_map, templates_map) = ir_context {
            for template_match in matches_map.keys() {
                if template_match == name {
                    templates_map.insert(template_match.clone(), ctx_template_value.clone());
                    match ctx_template_value {
                        IRTemplateValue::Type(type_id) => {
                            return Ok(IRExprResult::Type(*type_id));
                        },
                        IRTemplateValue::Const(const_value) => {
                            let expected_type = match matches_map[template_match] { IRTemplate::Const(type_id) => type_id, _ => panic!() };
                            self.ensure_type_matches(const_value.type_id, Some(expected_type), Some(span), false)?;
                            return Ok(IRExprResult::Value(*const_value));
                        }
                    }
                }
            }
        }
        let scope = self.get_context_scope(ir_context);
        if let Some(ir_const) = self.find_constant_in_scope(scope, name) {
            return Ok(IRExprResult::Value(ir_const.value));
        }
        if let Some(result) = self.lower_struct_name(scope, name, span)? {
            return Ok(result);
        }
        if let Some(result) = self.lower_type_def_name(scope, name, span)? {
            return Ok(result);
        }
        if let IRExprContext::Trait(self_type) = expr_context && let Some(result) = self.lower_trait_name(scope, name, *self_type, span)? {
            return Ok(result);
        }
        if let IRExprContext::TypeConstraint(self_type) = expr_context && let Some(result) = self.lower_trait_name(scope, name, *self_type, span)? {
            return Ok(result);
        }
        if let Some(module) = self.get_module_scope_in_scope(scope, name)? {
            return Ok(IRExprResult::ModuleScope(module));
        }
        if let Some(prim_t) = self.primitive_type_from(name) {
            return Ok(IRExprResult::Type(self.primitive_type(prim_t)?));
        }
        if let Some((_, template_value)) = self.ir_scope(scope).templates_map.iter().find(|(key, _)| **key == name) {
            return match template_value {
                IRTemplateValue::Type(type_id) => Ok(IRExprResult::Type(*type_id)),
                IRTemplateValue::Const(const_value) => Ok(IRExprResult::Value(*const_value))
            }
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

    pub fn lower_impl_fun_name(&mut self, impl_id: IRImplId, fun_name: &str, opt_self_value: Option<Box<IRExprResult<'ctx>>>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let fun_def = self.ir_impl(impl_id).ast_def.scope.as_ref().unwrap().functions.iter().find(|fun| fun.name == fun_name).unwrap();
        if !fun_def.templates.templates.is_empty() {
            return Ok(IRExprResult::FunctionName(self.ir_impl(impl_id).scope, fun_name.to_string(), opt_self_value,  Some(impl_id)));
        } else {
            let fun = self.lower_impl_fun(impl_id, fun_def.name.as_str(), &Vec::new(), Some(span))?;
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

    pub fn lower_type_def_name(&mut self, search_scope: IRScopeId, type_def_name: &str, span: Span) -> Result<Option<IRExprResult<'ctx>>, CompilerError> {
        if let Some((type_def_scope, type_def)) = self.find_type_def_in_scope(search_scope, type_def_name, Some(span))? {
            let ir_context = &mut IRContext::ScopeContext(type_def_scope);
            let type_id = self.get_type(ir_context, &type_def.expr, &IRExprContext::Type)?;
            return Ok(Some(IRExprResult::Type(type_id)));
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

    pub fn lower_expr_literal(&mut self, literal: &Literal, expr_context: &IRExprContext<'ctx>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let ctx_type = if let IRExprContext::Value(ctx_t) = expr_context { *ctx_t } else { panic!() };
        let result = match &literal {
            Literal::Bool(v) => {
                let llvm_value = if *v { self.llvm_context.bool_type().const_all_ones() } else { self.llvm_context.bool_type().const_zero() }.into();
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Bool)?, llvm_value }
            },
            Literal::Char(ch) => {
                IRExprValueResult{ type_id: self.primitive_type(PrimitiveType::Char)?, llvm_value: self.llvm_context.i8_type().const_int(*ch as u64, false).into() }
            },
            Literal::UnresolvedInteger(int) => {
                let default = (self.primitive_type(PrimitiveType::U64)?, self.llvm_context.i64_type());
                let (int_type_id, llvm_int_type) = if let Some(ctx_t) = ctx_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(ctx_t).type_enum && (prim.is_int() | prim.is_uint()) {
                        (self.primitive_type(prim)?, self.ir_type(ctx_t).llvm_type.into_int_type())
                    } else {
                        default
                    }
                } else {
                    default
                };
                IRExprValueResult{ type_id: int_type_id, llvm_value: llvm_int_type.const_int(*int as u64, false).into() }
            },
            Literal::Float(float) => {
                let default = (self.primitive_type(PrimitiveType::F64)?, self.llvm_context.f64_type());
                let (float_type_id, llvm_float_type) = if let Some(ctx_t) = ctx_type {
                    if let IRTypeEnum::Primitive(prim) = self.ir_type(ctx_t).type_enum && prim.is_float() {
                        (self.primitive_type(prim)?, self.ir_type(ctx_t).llvm_type.into_float_type())
                    } else {
                        default
                    }
                } else {
                    default
                };                  
                IRExprValueResult{ type_id: float_type_id, llvm_value: llvm_float_type.const_float(*float).into() }
            },
            Literal::String(string) => {
                let ptr_value = self.builder.build_global_string_ptr(string, "global_string").unwrap();
                let char_type = self.primitive_type(PrimitiveType::Char)?;
                let slice_type = self.slice_type(char_type)?;
                let unsized_ref_type = self.unsized_ref_type(slice_type, false)?;
                IRExprValueResult { type_id: unsized_ref_type, llvm_value: self.unsized_ref_value(unsized_ref_type, ptr_value.as_pointer_value(), string.len() as u64)? }
            },
            Literal::Void => {
                let void_type = self.primitive_type(PrimitiveType::Void)?;
                self.get_type_zero(void_type)
            }
            _ => todo!()
        };
        Ok(result)
    }

    pub fn lower_args(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, expr_contexts: &[IRExprContext<'ctx>], any_size: bool) -> Result<Vec<(IRExprResult<'ctx>, Span)>, CompilerError> {
        let args_len = expr_contexts.len();
        let mut args: Vec<(IRExprResult<'ctx>, Span)> = Vec::new();
        let mut i: usize = 0;
        let mut cur_expr = args_expr.clone();
        while i < args_len || any_size {
            let expr_context = expr_contexts.get(i).unwrap_or(&expr_contexts[0]);
            let expr_context = if let IRExprContext::Template(template) = expr_context { 
                match template {
                    IRTemplate::Const(type_id) => &IRExprContext::Value(Some(*type_id)),
                    IRTemplate::Type => &IRExprContext::Type
                }
            } else { expr_context };
            let expr_result = self.lower_expr(ir_context, cur_expr.as_ref(), expr_context)?;
            if let IRExprResult::Empty = expr_result {
                break;
            } else if let IRExprResult::CommaSeperated(left_expr, right_expr) = expr_result {
                let arg_result = self.lower_expr(ir_context, left_expr.as_ref(), expr_context)?;
                args.push((arg_result, left_expr.span));
                if i + 1 >= args_len && !any_size {
                    i += 2;
                    break;
                }
                cur_expr = right_expr;
            } else {
                args.push((expr_result, cur_expr.span));
                cur_expr.value = ExprNodeEnum::Empty;
                i += 1;
                break;
            }
            i += 1;
        }
        if i != args_len && !any_size || cur_expr.value != ExprNodeEnum::Empty {
            return Err(self.error(SemanticError::UnmatchedArgCount { expected: args_len }, Some(cur_expr.span)));
        }
        Ok(args)
    }

    pub fn lower_args_values(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, expr_contexts: &[IRExprContext<'ctx>], any_size: bool) -> Result<Vec<IRExprValueResult<'ctx>>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, expr_contexts, any_size)?;
        let mut args_values = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            let ctx_t = expr_contexts.get(i).unwrap_or(&expr_contexts[0]);
            args_values.push(self.ensure_expr_result_value(ir_context, expr_result, true, *span, ctx_t)?);
        }
        Ok(args_values)
    }

    pub fn lower_args_types(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, expr_contexts: &[IRExprContext<'ctx>]) -> Result<Vec<IRTypeId>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, expr_contexts, false)?;
        let mut args_types = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            args_types.push(self.ensure_expr_result_type(expr_result, *span)?);
        }
        Ok(args_types)
    }

    pub fn lower_args_traits(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, expr_contexts: &[IRExprContext<'ctx>]) -> Result<Vec<IRTraitId>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, expr_contexts, false)?;
        let mut args_types = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            args_types.push(self.ensure_expr_result_trait(expr_result, *span, &expr_contexts[i])?);
        }
        Ok(args_types)
    }

    pub fn get_ir_templates(&mut self, ast_templates: &Templates) -> Result<Vec<IRTemplate>, CompilerError> {
        let mut templates = Vec::new();
        for ast_template in ast_templates.templates.iter() {
            match ast_template {
                Template::Const(const_value) => {
                    let ir_context = &mut IRContext::ScopeContext(self.get_global_scope()?);
                    let const_type = self.get_type(ir_context, &const_value.var_type, &IRExprContext::Type)?;
                    templates.push(IRTemplate::Const(const_type));
                },
                Template::VarType(_, _) => {
                    templates.push(IRTemplate::Type);
                }
            }
        }
        Ok(templates)
    }

    pub fn lower_args_templates(&mut self, ir_context: &mut IRContext<'ctx>, args_expr: &Box<ExprNode>, expr_contexts: &[IRExprContext<'ctx>]) -> Result<IRExprResult<'ctx>, CompilerError> {
        let args = self.lower_args(ir_context, args_expr, &expr_contexts, false)?;
        let mut result = Vec::new();
        for (i, (expr_result, span)) in args.iter().enumerate() {
            if let IRExprResult::NoImplMatch = expr_result {
                return Ok(IRExprResult::NoImplMatch);
            }
            result.push(self.ensure_expr_result_template_value(ir_context, expr_result, *span, &expr_contexts[i])?);
        }
        Ok(IRExprResult::TemplatesValues(result))
    }

    pub fn load_place(&mut self, ir_context: &mut IRContext<'ctx>, place: IRExprPlaceResult<'ctx>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if self.is_type_unsized(place.type_id)? {
            return Err(self.error(SemanticError::LoadingUnsized, Some(span)));
        }
        if let Some(owner) = place.owner {
            let var = &mut ir_context.into_fun_context().vars[owner];
            if let None = self.find_impl_of_core_trait(place.type_id, &CoreTraitFun::Copy)? {
                if var.moved {
                    return Err(self.error(SemanticError::MovedValue, Some(span)));
                }
                var.moved = true;
            }
        }
        let ir_type = self.ir_type(place.type_id);
        let llvm_value = self.builder.build_load(ir_type.llvm_type, place.ptr_value.into_pointer_value(), "tmp").unwrap();
        Ok(IRExprValueResult { type_id: place.type_id, llvm_value })
    }

    pub fn load_if_place(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        match expr_result { IRExprResult::Value(value) => Ok(value), IRExprResult::Place(place) => self.load_place(ir_context, place, span), _ => panic!() }
    }
}