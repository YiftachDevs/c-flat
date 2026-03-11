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
                self.lower_postfix_opr_invoke(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            },
            PostfixOpr::Con => {
                self.lower_postfix_opr_constructor(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            },
            PostfixOpr::Tmp => {
                self.lower_postfix_opr_templates(ir_context, left_expr_result, right_expr, context_type, left_expr.span)
            }
            _ => todo!("lower_postix_opr 6")
        }
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
                if let IRTypeEnum::Struct(ir_struct) = &self.ir_type(*ctx_type).type_enum {
                    ir_struct.templates_map.iter().map(|(_, value)| IRContextType::Impl(*value)).collect::<Vec<IRContextType>>()
                } else {
                    return Ok(IRExprResult::NoImplMatch);
                }
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
        let right_expr = right_expr.as_ref();
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
                if let Some(fun_result) = self.lower_impl_fun_name(type_id, name.as_str(), None, right_expr.span)? {
                    return Ok(fun_result);
                }
                return Err(self.error(SemanticError::ExpectedFunction, Some(right_expr.span)));
            },
            IRExprResult::Value(expr_value_result) => {
                if let Some(fun_result) = self.lower_impl_fun_name(expr_value_result.type_id, name.as_str(), Some(expr_value_result), right_expr.span)? {
                    return Ok(fun_result);
                }
                let void_id = self.primitive_type(PrimitiveType::Void)?;
                let ir_type = self.ir_type(expr_value_result.type_id);
                match &ir_type.type_enum {
                    IRTypeEnum::Struct(_struct) => {
                        if let Some((i, arg)) = _struct.args.iter().enumerate().find(|(_, arg)| arg.name == name) {
                            let arg_value = if arg.type_id != void_id {
                                let llvm_value = expr_value_result.llvm_value.unwrap();
                                let arg_name = format!("{}.{}", llvm_value.get_name().to_string_lossy(), arg.name);
                                match llvm_value {
                                    BasicValueEnum::PointerValue(ptr_value) => {
                                        Some(self.builder.build_struct_gep(ir_type.llvm_type.unwrap(), ptr_value, i as u32, arg_name.as_str()).unwrap().into())
                                    },
                                    BasicValueEnum::StructValue(struct_value) => {
                                        Some(self.builder.build_extract_value(struct_value, i as u32, arg_name.as_str()).unwrap())
                                    },
                                    _ => panic!("lower_postfix_opr_member 2")
                                }
                            } else { None };

                            return Ok(IRExprResult::Value(IRExprValueResult { type_id: arg.type_id, llvm_value: arg_value }));
                        }
                        else {
                            return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)));
                        }
                    },
                    _ => return Err(self.error(SemanticError::UnrecognizedName, Some(right_expr.span)))
                }
            }
            _ => todo!("lower_postfix_opr_member 4")
        }
    }

    pub fn lower_postfix_opr_invoke(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRExprResult::Function(fun_id, opt_self_value) = left_expr_result {
            let mut args_context_types = self.ir_function(fun_id).args.iter().map(|arg| IRContextType::Type(arg.type_id)).collect::<Vec<IRContextType>>();
            let llvm_args = if let Some(self_value) = opt_self_value {
                if let IRContextType::Type(ctx_self) = args_context_types.remove(0) && self_value.type_id != ctx_self {
                    return Err(self.error(SemanticError::TypeMismatch { expected: self.format_type(ctx_self), got: self.format_type(self_value.type_id) }, Some(span)));
                }
                let mut res = self.lower_args_values(ir_context, right_expr, &args_context_types)?.into_iter().filter_map(|res| res.llvm_value).collect::<Vec<BasicValueEnum<'ctx>>>();
                if let Some(self_llvm_value) = self_value.llvm_value {
                    res.insert(0, self_llvm_value);
                }
                res
            } else {
                self.lower_args_values(ir_context, right_expr, &args_context_types)?.into_iter().filter_map(|res| res.llvm_value).collect::<Vec<BasicValueEnum<'ctx>>>()
            };
            let llvm_args = llvm_args.iter().map(|llvm_arg| (*llvm_arg).into()).collect::<Vec<BasicMetadataValueEnum>>();
            let ir_fun = self.ir_function(fun_id);
            if !ir_fun.has_body {
                return Err(self.error(SemanticError::FunctionMissingBody { fun_str: self.format_scope_path(ir_fun.scope) }, Some(span)));
            }
            let fun_call = self.builder.build_call(ir_fun.llvm_value, &llvm_args, "fun_call_tmp").unwrap();
            let ret_value = fun_call.try_as_basic_value().basic();
            Ok(IRExprResult::Value(IRExprValueResult { type_id: ir_fun.return_type, llvm_value: ret_value }))
        } else {
            return Err(self.error(SemanticError::ExpectedFunction, Some(span)));
        }
    }

    fn lower_postfix_opr_constructor(&mut self, ir_context: &mut IRContext<'ctx>, left_expr_result: IRExprResult<'ctx>, right_expr: &Box<ExprNode>, context_type: &IRContextType, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
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
                    Ok(IRExprResult::Value(IRExprValueResult { type_id, llvm_value: Some(llvm_value.as_basic_value_enum()) }))
                },
                _ => return Err(self.error(SemanticError::ExpectedStruct, Some(span)))
            }
        } else {
            return Err(self.error(SemanticError::ExpectedStruct, Some(span)));
        }
    }
}