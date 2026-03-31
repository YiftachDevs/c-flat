use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRTemplateValue, IRTypeEnum, IRTypeId, IRVarDeclaration, IRVariable, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprRefResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, PrefixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_prefix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PrefixOpr, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        match opr {
            PrefixOpr::Addr { is_mut } => {
                self.lower_prefix_opr_addr(ir_context, is_mut, right_expr, span, context_type)
            },
            PrefixOpr::Deref => {
                self.lower_prefix_opr_deref(ir_context, right_expr, span, context_type)
            }
            _ => panic!()
        }
    }

    pub fn lower_prefix_opr_addr(&mut self, ir_context: &mut IRContext<'ctx>, is_ref_mut: bool, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let new_context_type = if let IRContextType::Value(Some(type_id)) = context_type && let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(*type_id).type_enum && (!is_mut || is_ref_mut) {
            &IRContextType::Value(Some(ptr_type_id))
        } else if let IRContextType::Impl(template_value) = context_type && let IRTemplateValue::Type(type_id) = template_value {
            if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(*type_id).type_enum && is_mut == is_ref_mut {
                &IRContextType::Impl(IRTemplateValue::Type(ptr_type_id))
            } else if let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = self.ir_type(*type_id).type_enum && is_mut == is_ref_mut {
                &IRContextType::Impl(IRTemplateValue::Type(unsized_type))
            } else {
                return Ok(IRExprResult::NoImplMatch);
            }
        } else { context_type };
        let expr_result = self.lower_expr(ir_context, right_expr, new_context_type)?;
        match expr_result {
            IRExprResult::Type(type_id) => {
                let ptr_type = if self.is_type_unsized(type_id)? {
                    self.unsized_ref_type(type_id, is_ref_mut)?
                } else {
                    self.reference_type(type_id, is_ref_mut)?
                };
                Ok(IRExprResult::Type(ptr_type))
            },
            IRExprResult::NoImplMatch => Ok(IRExprResult::NoImplMatch),
            _ => Ok(IRExprResult::Value(self.address(ir_context, expr_result, is_ref_mut, right_expr.span)?))
        }
    }

    pub fn address(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, is_mut_ref: bool, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        match expr_result {
            IRExprResult::Value(value) => {
                let ir_fun_context = ir_context.into_fun_context();
                let temp_name = format!("tmp{}", ir_fun_context.vars.len());
                let var_dec = IRVarDeclaration { name: temp_name, type_id: value.type_id, is_mut: is_mut_ref };
                let var_id = self.alloc_var(ir_fun_context, &var_dec, Some(value.llvm_value));
                let var = &mut ir_fun_context.vars[var_id];
                var.moved = true;
                let value_result = IRExprValueResult { type_id: self.reference_type(var.place.type_id, is_mut_ref)?, llvm_value: var.place.ptr_value.into() };
                Ok(value_result)
            },
            IRExprResult::Place(place) => {
                let ir_fun_context = ir_context.into_fun_context();
                if let Some(owner) = place.owner {
                    let var = &mut ir_fun_context.vars[owner];
                    if !var.is_mut && is_mut_ref {
                        return Err(self.error(SemanticError::ExpectedMutable, Some(span)));
                    }
                    if !var.moved {
                        var.moved = true;
                    } else {
                        // return Err(self.error(SemanticError::MovedValue, Some(span)));
                    }
                }
                let value_result = if self.is_type_unsized(place.type_id)? {
                    IRExprValueResult { type_id: self.unsized_ref_type(place.type_id, is_mut_ref)?, llvm_value: place.ptr_value.into() }
                } else {
                    IRExprValueResult { type_id: self.reference_type(place.type_id, is_mut_ref)?, llvm_value: place.ptr_value.into() }
                };
                Ok(value_result)
            },
            _ => panic!()
        }
    }

    pub fn lower_prefix_opr_deref(&mut self, ir_context: &mut IRContext<'ctx>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        let new_context_type = if let IRContextType::Value(Some(type_id)) = context_type { IRContextType::Value(Some(self.reference_type(*type_id, false)?)) } else { IRContextType::Value(None) };
        let expr_result = self.lower_expr(ir_context, right_expr, &new_context_type)?;
        let deref_result = self.deref(ir_context, expr_result, span)?;
        if let IRExprResult::Value(value) = deref_result {
            Ok(self.deref(ir_context, deref_result, span)?)
        } else {
            Ok(deref_result)
        }
    }

    pub fn is_derefable(&mut self, type_id: IRTypeId) -> Result<bool, CompilerError> {
        if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(type_id).type_enum {
            return Ok(true);
        }
        if let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = self.ir_type(type_id).type_enum {
            return Ok(true);
        }
        if let Some(_) = self.find_impl_of_core_trait(type_id, &CoreTraitFun::Deref)? {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn deref(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        match expr_result {
            IRExprResult::Value(value) => {
                if let IRTypeEnum::Reference { ptr_type_id, is_mut } = &self.ir_type(value.type_id).type_enum {
                    let place = IRExprPlaceResult { type_id: *ptr_type_id, ptr_value: value.llvm_value, owner: None, is_mut: *is_mut };
                    Ok(IRExprResult::Place(place))
                } else if let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = &self.ir_type(value.type_id).type_enum {
                    let place = IRExprPlaceResult { type_id: *unsized_type, ptr_value: value.llvm_value, owner: None, is_mut: *is_mut };
                    Ok(IRExprResult::Place(place))
                } else {
                    let result = self.call_core_trait(ir_context, IRExprResult::Value(value), None, &CoreTraitFun::Deref, span)?;
                    Ok(IRExprResult::Value(result))
                }
            },
            IRExprResult::Place(place) => {
                if let IRTypeEnum::Reference { ptr_type_id, is_mut } = &self.ir_type(place.type_id).type_enum {
                    let value = self.load_place(place, span)?;
                    let place = IRExprPlaceResult { type_id: *ptr_type_id, ptr_value: value.llvm_value, owner: None, is_mut: *is_mut };
                    Ok(IRExprResult::Place(place))
                } else if let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = &self.ir_type(place.type_id).type_enum {
                    let value = self.load_place(place, span)?;
                    let place = IRExprPlaceResult { type_id: *unsized_type, ptr_value: value.llvm_value, owner: None, is_mut: *is_mut };
                    Ok(IRExprResult::Place(place))
                } else {
                    let result = self.call_core_trait(ir_context, IRExprResult::Place(place), None, &CoreTraitFun::Deref, span)?;
                    Ok(IRExprResult::Value(result))
                }
            },
            _ => panic!()
        }
    }
}