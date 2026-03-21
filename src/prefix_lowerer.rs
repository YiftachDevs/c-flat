use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRTypeEnum, IRTypeId, IRVarDeclaration, IRVariable, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, PrefixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_prefix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PrefixOpr, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContextType::Impl(ctx_type) = context_type && opr == PrefixOpr::Addr {
            if let IRTypeEnum::Reference { ptr_type_id } = &self.ir_type(*ctx_type).type_enum {
                return self.lower_expr(ir_context, right_expr, &IRContextType::Type(*ptr_type_id));
            } else {
                return Ok(IRExprResult::NoImplMatch);
            }
        }
        if opr == PrefixOpr::Addr {
            return self.lower_prefix_opr_addr(ir_context, right_expr, span, context_type);
        }

        let expr_result = self.lower_expr(ir_context, right_expr, context_type)?;
        if let IRExprResult::Value(expr_value_result) = expr_result {
            if opr == PrefixOpr::Addr {
                let ref_type = self.reference_type(expr_value_result.type_id)?;
                let ref_value = self.auto_reference(expr_value_result, ref_type, span)?;
                return Ok(IRExprResult::Value(ref_value));
            }
            if opr == PrefixOpr::Deref && let IRTypeEnum::Reference { ptr_type_id } = &self.ir_type(expr_value_result.type_id).type_enum {
                return Ok(IRExprResult::Value(self.deref(ir_context, expr_value_result)?.unwrap()));
            }
            let (trait_name, fun_name) = Self::get_core_opr_trait_name(CoreOpr::Prefix(opr));
            let fun = self.get_core_trait_fun(expr_value_result.type_id, trait_name, fun_name, Some(span))?;
            let result = self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(expr_value_result)), &Box::new(ExprNode { value: ExprNodeEnum::Empty, span }), context_type, span)?;
            if opr == PrefixOpr::Deref {
                
            }
            return Ok(IRExprResult::Value(result));
        }
        match opr {
            PrefixOpr::Addr => {
                if let IRExprResult::Type(type_id) = expr_result {
                    let ptr_type = self.reference_type(type_id)?;
                    Ok(IRExprResult::Type(ptr_type))
                } else { panic!() }
            },
            _ => panic!()
        }
    }

    pub fn lower_prefix_opr_addr(&mut self, ir_context: &mut IRContext<'ctx>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        let new_context_type = if let IRContextType::Type(type_id) = context_type && let IRTypeEnum::Reference { ptr_type_id } = self.ir_type(*type_id).type_enum {
            IRContextType::Type(ptr_type_id)
        } else { IRContextType::Any };
        let expr_result = self.lower_expr(ir_context, right_expr, &new_context_type)?;
        match expr_result {
            IRExprResult::Value(value) => {
                let var_dec = IRVarDeclaration { name: self.get_unreachable_var_name(), type_id: value.type_id, is_mut: true };
                let place = self.alloc_place(&var_dec);
                self.builder.build_store(place.ptr_value, value.llvm_value).unwrap();
                ir_context.into_fun_context().vars.push(IRVariable { name: var_dec.name, place, moved: false });
                3
            },
            IRExprResult::Place(place) => {

            },
            _ => panic!()
        }
    }

    pub fn deref(&mut self, ir_context: &mut IRContext<'ctx>, value: IRExprValueResult<'ctx>) -> Result<Option<IRExprValueResult<'ctx>>, CompilerError> {
        if let IRTypeEnum::Reference { ptr_type_id } = &self.ir_type(value.type_id).type_enum {
            Ok(Some(if !self.is_type_zero_sized(*ptr_type_id)? {
                let value = self.builder.build_load(self.ir_type(*ptr_type_id).llvm_type.unwrap(), value.llvm_value.unwrap().into_pointer_value(), "load_val").unwrap();
                IRExprValueResult { type_id: *ptr_type_id, llvm_value: Some(value) }
            } else {
                IRExprValueResult { type_id: *ptr_type_id, llvm_value: None }
            }))
        } else { panic!() }
    }
}