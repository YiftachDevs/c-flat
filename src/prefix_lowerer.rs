use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRTypeEnum, IRTypeId, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, PostfixOpr, PrefixOpr, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_prefix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: PrefixOpr, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        let ctx_t = if opr == PrefixOpr::Addr && let IRContextType::Impl(ctx_type) = context_type {
            if let IRTypeEnum::Reference { ptr_type_id } = &self.ir_type(*ctx_type).type_enum {
                &IRContextType::Impl(*ptr_type_id)
            } else {
                return Ok(IRExprResult::NoImplMatch);
            }
        } else { context_type };

        let expr_result = self.lower_expr(ir_context, right_expr, ctx_t)?;
        if let IRExprResult::Value(expr_value_result) = expr_result {
            let (trait_name, fun_name) = Self::get_core_opr_trait_name(CoreOpr::Prefix(opr));
            let fun = self.get_core_trait_fun(expr_value_result.type_id, trait_name, fun_name, Some(span))?;
            let result = self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(expr_value_result)), &Box::new(ExprNode { value: ExprNodeEnum::Empty, span }), context_type, span)?;
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
}