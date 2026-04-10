use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRExprContext, IRTemplateValue, IRTypeEnum, IRTypeId, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_postfix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PostfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let err = self.error(SemanticError::InvalidOpr(opr.to_string()), Some(span));
        if opr == PostfixOpr::Sep {
            let left_expr_result = self.lower_expr(ir_context, left_expr, &IRExprContext::Type)?;
            return self.lower_postfix_opr_seperator(ir_context, left_expr_result, right_expr, context_type, left_expr.span);
        }
        if opr == PostfixOpr::Tmp {
            let left_expr_result = self.lower_expr(ir_context, left_expr, context_type)?;
            return self.lower_postfix_opr_templates(ir_context, left_expr_result, right_expr, context_type, left_expr.span);
        }
        if let IRContext::FunContext(_) = ir_context { } else { return Err(err); }
        match opr {
            PostfixOpr::Mem => {
                let left_expr_result = self.lower_expr(ir_context, left_expr, &IRExprContext::Value(None))?;
                self.lower_postfix_opr_member(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            },
            PostfixOpr::Inv => {
                let left_expr_result = self.lower_expr(ir_context, left_expr, &IRExprContext::Function)?;
                Ok(IRExprResult::Value(self.lower_postfix_opr_invoke(ir_context, left_expr_result, right_expr, left_expr.span)?))
            },
            PostfixOpr::Con => {
                let left_expr_result = self.lower_expr(ir_context, left_expr, &IRExprContext::Type)?;
                Ok(IRExprResult::Value(self.lower_postfix_opr_constructor(ir_context, left_expr_result, right_expr, context_type, left_expr.span)?))
            },
            PostfixOpr::Idx => {
                let left_expr_result = self.lower_expr(ir_context, left_expr, &IRExprContext::Value(None))?;
                Ok(self.lower_postfix_opr_index(ir_context, left_expr_result, right_expr, left_expr.span)?)
            },
            _ => panic!()
        }
    }

    fn lower_postfix_opr_index(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let type_id = left_expr_result.get_type_id();
        if let Some(_) = self.find_impl_of_core_trait(type_id, &CoreTraitFun::Index)? {
            let index_result = if let Ok(index_result) = self.call_core_trait(ir_context, left_expr_result.clone(), Some(right_expr), &CoreTraitFun::IndexMut, span) {
                index_result
            } else { self.call_core_trait(ir_context, left_expr_result, Some(right_expr), &CoreTraitFun::Index, span)? };
            self.deref(ir_context, IRExprResult::Value(index_result), span)
        } else if self.is_derefable(type_id)? {
            let deref_result = self.deref(ir_context, left_expr_result, span)?;
            self.lower_postfix_opr_index(ir_context, deref_result, right_expr, span)
        } else {
            Ok(IRExprResult::Value(self.call_core_trait(ir_context, left_expr_result, Some(right_expr), &CoreTraitFun::Index, span)?))
        }
    }

    fn lower_postfix_opr_templates(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRExprContext<'ctx>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRExprResult::FunctionName(parent_scope, fun_name, opt_self_value, opt_impl_id) = left_expr_result {
            let fun_def = self.find_fun_def_in_scope(parent_scope, fun_name.as_str(), Some(span))?.unwrap().1;
            let contexts_types = self.get_ir_templates(&fun_def.templates)?.iter().map(|t| IRExprContext::Template(t.clone())).collect::<Vec<IRExprContext>>();
            let templates_values = self.lower_args_templates(ir_context, right_expr, &contexts_types)?;
            let templates_values = self.ensure_expr_result_template_values(templates_values);
            let fun_id = self.lower_fun(parent_scope, fun_name.as_str(), &templates_values, Some(span))?;
            if let Some(impl_id) = opt_impl_id {
                self.ensure_trait_fun_valid(impl_id, fun_id, Some(span))?;
            }
            Ok(IRExprResult::Function(fun_id, opt_self_value))
        } else if let IRExprResult::StructName(parent_scope, struct_name) = left_expr_result {
            let struct_def = self.find_struct_def_in_scope(parent_scope, struct_name.as_str(), Some(span))?.unwrap().1;
            let context_types = if let IRExprContext::Impl(template_value) = context_type {
                if let IRTemplateValue::Type(struct_type) = template_value {
                    if let IRTypeEnum::Struct(ir_struct) = &self.ir_type(*struct_type).type_enum && ir_struct.def == struct_def {
                        ir_struct.templates_map.values().map(|v| IRExprContext::Impl(v.clone())).collect::<Vec<IRExprContext>>()
                    } else {
                        return Ok(IRExprResult::NoImplMatch);
                    }
                } else {
                    return Ok(IRExprResult::NoImplMatch);
                }
            } else {
                self.get_ir_templates(&struct_def.templates)?.iter().map(|t| IRExprContext::Template(t.clone())).collect::<Vec<IRExprContext>>()
            };
            let templates_values = self.lower_args_templates(ir_context, right_expr, &context_types)?;
            if let IRExprResult::NoImplMatch = templates_values {
                return Ok(IRExprResult::NoImplMatch);
            }
            let templates_values = self.ensure_expr_result_template_values(templates_values);
            let struct_id = self.lower_struct(parent_scope, struct_name.as_str(), &templates_values, Some(span))?;
            Ok(IRExprResult::Type(struct_id))
        } else if let IRExprResult::TraitName(parent_scope, trait_name, trait_type_id) = left_expr_result {
            let trait_def = self.find_trait_def_in_scope(parent_scope, trait_name.as_str(), Some(span))?.unwrap().1;
            let contexts_types = self.get_ir_templates(&trait_def.templates)?.iter().map(|t| IRExprContext::Template(t.clone())).collect::<Vec<IRExprContext>>();
            let templates_values = self.lower_args_templates(ir_context, right_expr, &contexts_types)?;
            let templates_values = self.ensure_expr_result_template_values(templates_values);
            let trait_id = self.lower_trait(parent_scope, trait_name.as_str(), &templates_values, trait_type_id, Some(span))?;
            Ok(IRExprResult::Trait(trait_id))
        }
        else {
            return Err(self.error(SemanticError::UnrecognizedName, Some(span)));
        }
    }

    fn lower_postfix_opr_seperator(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRExprContext<'ctx>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let name  = if let ExprNodeEnum::Name(name) = &right_expr.value { name.clone() } else {
            return Err(self.error(SemanticError::ExpectedName, Some(right_expr.span)));
        };
        match left_expr_result {
            IRExprResult::ModuleScope(scope) => {
                if let Some(new_scope) = self.get_module_scope_in_scope(scope, name.as_str()) {
                    Ok(IRExprResult::ModuleScope(new_scope))
                } else if let Some(fun_result) = self.lower_fun_name(scope, name.as_str(), right_expr.span)? {
                    return Ok(fun_result);
                } else if let Some(struct_result) = self.lower_struct_name(scope, name.as_str(), right_expr.span)? {
                    return Ok(struct_result);
                } else if let IRExprContext::Trait(self_type) = context_type && let Some(trait_result) = self.lower_trait_name(scope, name.as_str(), *self_type, right_expr.span)? {
                    return Ok(trait_result);
                } else {
                    return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
                }
            },
            IRExprResult::Type(type_id) => {
                if let Some(impl_id) = self.find_impl_of_fun(type_id, name.as_str(), None)? {
                    return Ok(self.lower_impl_fun_name(impl_id, name.as_str(), None, right_expr.span)?);
                }
                return Err(self.error(SemanticError::ExpectedFunction, Some(right_expr.span)));
            },
            _ => panic!()
        }
    }

    fn lower_postfix_opr_member(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRExprContext<'ctx>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let name  = if let ExprNodeEnum::Name(name) = &right_expr.value { name.clone() } else {
            return Err(self.error(SemanticError::ExpectedName, Some(right_expr.span)));
        };
        match left_expr_result {
            IRExprResult::Place(place) => {
                if let IRExprContext::Function = context_type && let Some(impl_id) = self.find_impl_of_fun(place.type_id, name.as_str(), Some(span))? {
                    return Ok(self.lower_impl_fun_name(impl_id, name.as_str(), Some(Box::new(IRExprResult::Place(place))), right_expr.span)?);
                } else if let IRExprContext::Value(_) = context_type {
                    let ir_type = self.ir_type(place.type_id);
                    if let IRTypeEnum::Struct(_struct) = &ir_type.type_enum {
                        if let Some((i, arg)) = _struct.args.iter().enumerate().find(|(_, arg)| arg.name == name) {
                            let llvm_ptr_value = place.ptr_value;
                            let arg_name = format!("{}.{}", llvm_ptr_value.get_name().to_string_lossy(), arg.name);
                            let arg_ptr_value = self.builder.build_struct_gep(ir_type.llvm_type, llvm_ptr_value.into_pointer_value(), i as u32, arg_name.as_str()).unwrap().into();
                            return Ok(IRExprResult::Place(IRExprPlaceResult { type_id: arg.type_id, ptr_value: arg_ptr_value, is_mut: place.is_mut, owner: place.owner }));
                        }
                    }
                }
                if self.is_derefable(place.type_id)? {
                    let deref_result = self.deref(ir_context, IRExprResult::Place(place), span)?;
                    return self.lower_postfix_opr_member(ir_context, deref_result, right_expr, context_type, span);
                }
                return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
            }
            IRExprResult::Value(expr_value_result) => {
                if let IRExprContext::Function = context_type && let Some(impl_id) = self.find_impl_of_fun(expr_value_result.type_id, name.as_str(), Some(span))? {
                    return Ok(self.lower_impl_fun_name(impl_id, name.as_str(), Some(Box::new(IRExprResult::Value(expr_value_result))), right_expr.span)?);
                }  else if let IRExprContext::Value(_) = context_type {
                    let ir_type = self.ir_type(expr_value_result.type_id);
                    if let IRTypeEnum::Struct(_struct) = &ir_type.type_enum {
                        if let Some((i, arg)) = _struct.args.iter().enumerate().find(|(_, arg)| arg.name == name) {
                            let llvm_value = expr_value_result.llvm_value.into_struct_value();
                            let arg_name = format!("{}.{}", llvm_value.get_name().to_string_lossy(), arg.name);
                            return Ok(IRExprResult::Value(IRExprValueResult { type_id: arg.type_id, llvm_value: self.builder.build_extract_value(llvm_value, i as u32, arg_name.as_str()).unwrap() }));
                        }
                    }
                }
                if self.is_derefable(expr_value_result.type_id)? {
                    let deref_result = self.deref(ir_context, IRExprResult::Value(expr_value_result), span)?;
                    return self.lower_postfix_opr_member(ir_context, deref_result, right_expr, context_type, span);
                }
                return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
            }
            _ => panic!()
        }
    }

    pub fn lower_postfix_opr_invoke(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Function(fun_id, opt_self_value) = left_expr_result {
            let prev_vars_len = ir_context.into_fun_context().vars.len();
            let mut args_context_types = self.ir_function(fun_id).args.iter().map(|arg| IRExprContext::Value(Some(arg.type_id))).collect::<Vec<IRExprContext<'ctx>>>();
            let llvm_args = if let Some(self_value) = opt_self_value {
                let mut res = if let IRExprContext::Value(Some(ctx_self)) = args_context_types.remove(0) {
                    println!("{}", right_expr.to_string());
                    println!("a");
                    let auto_ref_result = self.auto_reference(ir_context, *self_value, ctx_self, span)?;
                    println!("a"); 
                    let e = vec![self.load_if_place(ir_context, auto_ref_result, span)?];
                    println!("a");
                    e
                } else { panic!() };
                res.extend(self.lower_args_values(ir_context, right_expr, &args_context_types, false)?);
                res
            } else {
                self.lower_args_values(ir_context, right_expr, &args_context_types, false)?
            }.iter().map(|llvm_arg| llvm_arg.llvm_value.into()).collect::<Vec<BasicMetadataValueEnum>>();
            if !self.ir_function(fun_id).has_body {
                return Err(self.error(SemanticError::FunctionMissingBody { fun_str: self.format_scope_path(self.ir_function(fun_id).scope) }, Some(span)));
            }
            let fun_call = self.builder.build_call(self.ir_function(fun_id).llvm_value, &llvm_args, "fun_call_tmp").unwrap();
            self.drop_vars(ir_context, prev_vars_len, span)?;
            let ret_value = fun_call.try_as_basic_value().unwrap_basic();
            Ok(IRExprValueResult { type_id: self.ir_function(fun_id).return_type, llvm_value: ret_value })
        } else {
            return Err(self.error(SemanticError::ExpectedFunction, Some(span)));
        }
    }

    fn lower_postfix_opr_constructor(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRExprContext<'ctx>, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Type(type_id) = left_expr_result {
            match &self.ir_type(type_id).type_enum {
                IRTypeEnum::Struct(_struct) => {
                    let context_types = _struct.args.iter().map(|arg| IRExprContext::Value(Some(arg.type_id))).collect::<Vec<IRExprContext<'ctx>>>();
                    let def_args = &_struct.def.vars;
                    let args_values = self.lower_args_values(ir_context, right_expr, &context_types, false)?;
                    let mut llvm_value: inkwell::values::AggregateValueEnum<'_> = self.ir_type(type_id).llvm_type.into_struct_type().get_undef().into();
                    let basic_value = llvm_value.as_basic_value_enum();
                    for i in 0..args_values.len() {
                        llvm_value.set_name("tmp_agg");
                        let arg_name: String = format!("{}.{}", basic_value.get_name().to_string_lossy(), def_args.variables[i].name);
                        llvm_value = self.builder.build_insert_value(llvm_value, args_values[i].llvm_value, i as u32, arg_name.as_str()).unwrap();
                    }
                    llvm_value.set_name("tmp_constructor");
                    Ok(IRExprValueResult { type_id, llvm_value: llvm_value.as_basic_value_enum() })
                },
                _ => return Err(self.error(SemanticError::ExpectedStruct, Some(span)))
            }
        } else {
            return Err(self.error(SemanticError::ExpectedStruct, Some(span)));
        }
    }
}