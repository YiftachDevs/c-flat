use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRFunctionId, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, InfixOpr, PostfixOpr, Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_infix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: InfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType) -> Result<IRExprResult<'ctx>, CompilerError> {
        if opr == InfixOpr::Com {
            return Ok(IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone()));
        }
        let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;

        match opr {
            InfixOpr::Com => {
                Ok(IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone()))
            },
            InfixOpr::As => {
                let new_type = self.get_type(ir_context, right_expr , &IRContextType::Any)?;
                Ok(IRExprResult::Value(IRExprValueResult { type_id: new_type, llvm_value: left_expr_result.llvm_value }))
            }
            _ => {
                let type_id = left_expr_result.type_id;
                let fun = self.get_core_trait_fun(type_id, opr, Some(span))?;
                self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(left_expr_result)), right_expr, context_type, span)
            }
        }
    }
}