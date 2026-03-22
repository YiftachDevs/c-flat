use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRTypeEnum, IRTypeId, IRVarDeclaration, IRVariable, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprRefResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, PrefixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_prefix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PrefixOpr, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
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

    pub fn lower_prefix_opr_addr(&mut self, ir_context: &mut IRContext<'ctx>, is_ref_mut: bool, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContextType::Impl(ctx_type) = context_type {
            if let IRTypeEnum::Reference { ptr_type_id, is_mut } = &self.ir_type(*ctx_type).type_enum && is_ref_mut == *is_mut {
                return self.lower_expr(ir_context, right_expr, &IRContextType::Type(*ptr_type_id));
            } else {
                return Ok(IRExprResult::NoImplMatch);
            }
        }
        let new_context_type = if let IRContextType::Type(type_id) = context_type && let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(*type_id).type_enum && (!is_mut || is_ref_mut) {
            IRContextType::Type(ptr_type_id)
        } else { IRContextType::Any };
        let expr_result = self.lower_expr(ir_context, right_expr, &new_context_type)?;
        match expr_result {
            IRExprResult::Type(type_id) => {
                let ptr_type = self.reference_type(type_id, is_ref_mut)?;
                Ok(IRExprResult::Type(ptr_type))
            },
            _ => Ok(IRExprResult::Ref(self.address(ir_context, expr_result, is_ref_mut, span)?))
        }
    }

    pub fn address(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, is_mut_ref: bool, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        match expr_result {
            IRExprResult::Value(value) => {
                let ir_fun_context = ir_context.into_fun_context();
                let var_dec = IRVarDeclaration { name: self.get_unreachable_var_name(), type_id: value.type_id, is_mut: is_mut_ref };
                let var_id = self.alloc_var(ir_fun_context, &var_dec, Some(value.llvm_value));
                let var = &mut ir_fun_context.vars[var_id];
                var.moved = true;
                let value_result = IRExprRefResult { expr_result: Box::new(IRExprResult::Place(var.place.clone())), is_mut_ref };
                Ok(ref_result)
            },
            IRExprResult::Place(place) => {
                let ir_fun_context = ir_context.into_fun_context();
                let var = &mut ir_fun_context.vars[place.owner];
                if !var.moved {
                    var.moved = true;
                } else {
                    return Err(self.error(SemanticError::MovedValue, Some(span)));
                }
                let ref_result = IRExprRefResult { expr_result: Box::new(IRExprResult::Place(ir_fun_context.vars[place.owner].place.clone())), is_mut_ref };
                Ok(ref_result)
            },
            IRExprResult::Ref(_) => {
                Ok(IRExprRefResult { expr_result: Box::new(expr_result), is_mut_ref })
            }
            _ => panic!()
        }
    }

    pub fn lower_prefix_opr_deref(&mut self, ir_context: &mut IRContext<'ctx>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        let new_context_type = if let IRContextType::Type(type_id) = context_type { IRContextType::Type(self.reference_type(*type_id, false)?) } else { IRContextType::Any };
        let expr_result = self.lower_expr(ir_context, right_expr, &new_context_type)?;
        Ok(IRExprResult::Place(self.deref(ir_context, expr_result, span)?))
    }

    /*
    if opr == PrefixOpr::Deref && let IRTypeEnum::Reference { ptr_type_id } = &self.ir_type(expr_value_result.type_id).type_enum {
                return Ok(IRExprResult::Value(self.deref(ir_context, expr_value_result)?.unwrap()));
            }
            let (trait_name, fun_name) = Self::get_core_opr_trait_name(CoreOpr::Prefix(opr));
            let fun = self.get_core_trait_fun(expr_value_result.type_id, trait_name, fun_name, Some(span))?;
            let result = self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(expr_value_result)), &Box::new(ExprNode { value: ExprNodeEnum::Empty, span }), context_type, span)?;
           
     */
/*Ok(Some(if !self.is_type_zero_sized(*ptr_type_id)? {
                let value = self.builder.build_load(self.ir_type(*ptr_type_id).llvm_type.unwrap(), value.llvm_value.unwrap().into_pointer_value(), "load_val").unwrap();
                IRExprValueResult { type_id: *ptr_type_id, llvm_value: Some(value) }
            } else {
                IRExprValueResult { type_id: *ptr_type_id, llvm_value: None }
            })) */
    pub fn deref(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        match expr_result {
            IRExprResult::Value(value) => {
                if let IRTypeEnum::Reference { ptr_type_id, is_mut } = &self.ir_type(value.type_id).type_enum {
                    let place = IRExprPlaceResult { type_id: *ptr_type_id, ptr_value: value.llvm_value.into_pointer_value(), owner};
    
                } else {
                    return Err(self.error(Se, opt_span))
                }
            },
            IRExprResult::Place(place) => {

            },
            IRExprResult::Ref(ref_result) => {
                Ok(*ref_result.expr_result)
            }
            _ => panic!()
        }
    }
}