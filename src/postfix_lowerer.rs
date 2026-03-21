use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRTypeEnum, IRTypeId, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_postfix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PostfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        let left_expr_result = self.lower_expr(ir_context, left_expr, context_type)?;
        match opr {
            PostfixOpr::Mem => {
                self.lower_postfix_opr_member(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            },
            PostfixOpr::Inv => {
                Ok(IRExprResult::Value(self.lower_postfix_opr_invoke(ir_context, left_expr_result, right_expr, context_type, left_expr.span)?))
            },
            PostfixOpr::Con => {
                Ok(IRExprResult::Value(self.lower_postfix_opr_constructor(ir_context, left_expr_result, right_expr, context_type, left_expr.span)?))
            },
            PostfixOpr::Tmp => {
                self.lower_postfix_opr_templates(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            }
            _ => todo!("lower_postix_opr 6")
        }
    }

    fn coerce_self(&mut self, ir_context: &mut IRContext<'ctx>, self_value: IRExprResult<'ctx>, target_type: IRTypeId) -> Result<IRExprValueResult<'ctx>, CompilerError> {

    }

    fn lower_postfix_opr_templates(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRExprResult::FunctionName(parent_scope, fun_name, opt_self_value, opt_impl_id) = left_expr_result {
            let fun_def = self.find_fun_def_in_scope(parent_scope, fun_name.as_str(), Some(span))?.unwrap().1;
            let templates_len = fun_def.templates.templates.len();
            let templates_values = self.lower_args_types(ir_context, right_expr, &vec![IRContextType::Any; templates_len])?;
            let fun_id = self.lower_fun(parent_scope, fun_name.as_str(), &templates_values, Some(span))?;
            if let Some(impl_id) = opt_impl_id {
                self.ensure_trait_fun_valid(impl_id, fun_id, Some(span))?;
            }
            Ok(IRExprResult::Function(fun_id, opt_self_value))
        } else if let IRExprResult::StructName(parent_scope, struct_name) = left_expr_result {
            let struct_def = self.find_struct_def_in_scope(parent_scope, struct_name.as_str(), Some(span))?.unwrap().1;
            let templates_len = struct_def.templates.templates.len();
            let context_types = if let IRContextType::Impl(ctx_type) = context_type {
                let templates = if let IRTypeEnum::Struct(ir_struct) = &self.ir_type(*ctx_type).type_enum {
                    ir_struct.templates_map.iter().map(|(_, value)| IRContextType::Impl(*value)).collect::<Vec<IRContextType>>()
                } else {
                    Vec::new()
                };
                if templates.len() != templates_len {
                    return Ok(IRExprResult::NoImplMatch);
                }
                templates
            } else {
                vec![IRContextType::Any; templates_len]
            };
            let templates_values = self.lower_args_types(ir_context, right_expr, &context_types)?;
            let struct_id = self.lower_struct(parent_scope, struct_name.as_str(), &templates_values, Some(span))?;
            Ok(IRExprResult::Type(struct_id))
        } else if let IRExprResult::TraitName(parent_scope, trait_name, trait_type_id) = left_expr_result {
            let trait_def = self.find_trait_def_in_scope(parent_scope, trait_name.as_str(), Some(span))?.unwrap().1;
            let templates_len = trait_def.templates.templates.len();
            let templates_values = self.lower_args_types(ir_context, right_expr, &vec![IRContextType::Any; templates_len])?;
            let trait_id = self.lower_trait(parent_scope, trait_name.as_str(), &templates_values, trait_type_id, Some(span))?;
            Ok(IRExprResult::Trait(trait_id))
       
        }
        else {
            return Err(self.error(SemanticError::UnrecognizedName, Some(span)));
        }
    }

    fn lower_postfix_opr_member(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let name  = if let ExprNodeEnum::Name(name, _) = &right_expr.value { name.clone() } else {
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
                } else if let IRContextType::Type(self_type) = context_type && let Some(trait_result) = self.lower_trait_name(scope, name.as_str(), *self_type, right_expr.span)? {
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
            IRExprResult::Var(var_name) => {
                let ir_var = ir_context.into_fun_context().vars.iter_mut().find(|var| var.name == name).unwrap();
                let place = &ir_var.place;
                if let Some(impl_id) = self.find_impl_of_fun(place.type_id, name.as_str(), Some(span))? {
                    let value_result = self.load_var(&mut ir_var)?;
                    return Ok(self.lower_impl_fun_name(value_result.type_id, name.as_str(), Some(value_result), right_expr.span)?);
                }
                let ir_type = self.ir_type(place.type_id);
                if let IRTypeEnum::Reference { ptr_type_id } = &ir_type.type_enum {
                    let deref_expr_result = IRExprValueResult { type_id: *ptr_type_id, llvm_value: expr_value_result.llvm_value };
                    return self.lower_postfix_opr_member(ir_context, IRExprResult::Value(deref_expr_result), right_expr, context_type, span);
                }
                return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
            }
            IRExprResult::Value(expr_value_result) => {
                if let Some(impl_id) = self.find_impl_of_fun(expr_value_result.type_id, name.as_str(), Some(span))? {
                    return Ok(self.lower_impl_fun_name(expr_value_result.type_id, name.as_str(), Some(expr_value_result), right_expr.span)?);
                }
                let ir_type = self.ir_type(expr_value_result.type_id);
                if let IRTypeEnum::Reference { ptr_type_id } = &ir_type.type_enum {
                    let deref_expr_result = IRExprValueResult { type_id: *ptr_type_id, llvm_value: expr_value_result.llvm_value };
                    return self.lower_postfix_opr_member(ir_context, IRExprResult::Value(deref_expr_result), right_expr, context_type, span);
                }
                if let IRTypeEnum::Struct(_struct) = &ir_type.type_enum {
                    if let Some((i, arg)) = _struct.args.iter().enumerate().find(|(_, arg)| arg.name == name) {
                        let llvm_value = expr_value_result.llvm_value;
                        let arg_name = format!("{}.{}", llvm_value.get_name().to_string_lossy(), arg.name);
                        let arg_value = match llvm_value {
                            BasicValueEnum::PointerValue(ptr_value) => {
                                let arg_llvm_value = self.builder.build_struct_gep(ir_type.llvm_type, ptr_value, i as u32, arg_name.as_str()).unwrap().into();
                                let arg_ref_type = self.reference_type(arg.type_id)?;
                                IRExprValueResult { type_id: arg_ref_type, llvm_value: Some(arg_llvm_value) }
                            },
                            BasicValueEnum::StructValue(struct_value) => {
                                IRExprValueResult { type_id: arg.type_id, llvm_value: Some(self.builder.build_extract_value(struct_value, i as u32, arg_name.as_str()).unwrap()) }
                            },
                            _ => panic!("lower_postfix_opr_member 2")
                        };
                        return Ok(IRExprResult::Value(arg_value));
                    }
                }
                if let Some(deref_expr_result) = self.deref(ir_context, expr_value_result)? {
                    return self.lower_postfix_opr_member(ir_context, IRExprResult::Value(deref_expr_result), right_expr, context_type, span);
                }
                return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
            }
            _ => todo!("lower_postfix_opr_member 4")
        }
    }

    pub fn lower_postfix_opr_invoke(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Function(fun_id, opt_self_value) = left_expr_result {
            let mut args_context_types = self.ir_function(fun_id).args.iter().map(|arg| IRContextType::Type(arg.type_id)).collect::<Vec<IRContextType>>();
            let llvm_args = if let Some(self_value) = opt_self_value {
                let mut res = if let IRContextType::Type(ctx_self) = args_context_types.remove(0) {
                    vec![self.auto_reference(self_value, ctx_self, span)?]
                } else { panic!() };
                res.extend(self.lower_args_values(ir_context, right_expr, &args_context_types)?);
                res
            } else {
                self.lower_args_values(ir_context, right_expr, &args_context_types)?
            }.into_iter().filter_map(|res| res.llvm_value).map(|llvm_arg| llvm_arg.into()).collect::<Vec<BasicMetadataValueEnum>>();
            let ir_fun = self.ir_function(fun_id);
            if !ir_fun.has_body {
                return Err(self.error(SemanticError::FunctionMissingBody { fun_str: self.format_scope_path(ir_fun.scope) }, Some(span)));
            }
            let fun_call = self.builder.build_call(ir_fun.llvm_value, &llvm_args, "fun_call_tmp").unwrap();
            let ret_value = fun_call.try_as_basic_value().basic();
            Ok(IRExprValueResult { type_id: ir_fun.return_type, llvm_value: ret_value })
        } else {
            return Err(self.error(SemanticError::ExpectedFunction, Some(span)));
        }
    }

    fn lower_postfix_opr_constructor(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        if let IRExprResult::Type(type_id) = left_expr_result {
            match &self.ir_type(type_id).type_enum {
                IRTypeEnum::Struct(_struct) => {
                    let context_types = _struct.args.iter().map(|arg| IRContextType::Type(arg.type_id)).collect::<Vec<IRContextType>>();
                    let def_args = &_struct.def.vars;
                    let args_values = self.lower_args_values(ir_context, right_expr, &context_types)?;
                    let mut llvm_value: inkwell::values::AggregateValueEnum<'_> = self.ir_type(type_id).llvm_type.unwrap().into_struct_type().get_undef().into();
                    let basic_value = llvm_value.as_basic_value_enum();
                    for i in 0..args_values.len() {
                        llvm_value.set_name("tmp_agg");
                        if let Some(arg_value) = args_values[i].llvm_value {
                            let arg_name: String = format!("{}.{}", basic_value.get_name().to_string_lossy(), def_args.variables[i].name);
                            llvm_value = self.builder.build_insert_value(llvm_value, arg_value, i as u32, arg_name.as_str()).unwrap();
                        }
                    }
                    llvm_value.set_name("tmp_constructor");
                    Ok(IRExprValueResult { type_id, llvm_value: Some(llvm_value.as_basic_value_enum()) })
                },
                _ => return Err(self.error(SemanticError::ExpectedStruct, Some(span)))
            }
        } else {
            return Err(self.error(SemanticError::ExpectedStruct, Some(span)));
        }
    }
}