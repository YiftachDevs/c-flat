use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRFunctionId, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, InfixOpr, PostfixOpr, PrefixOpr, Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_infix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: InfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRContextType<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if opr == InfixOpr::Com {
            return Ok(IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone()));
        }
        if opr == InfixOpr::As {
            let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;
            let new_type = self.get_type(ir_context, right_expr , &IRContextType::Type)?;
            return Ok(IRExprResult::Value(IRExprValueResult { type_id: new_type, llvm_value: left_expr_result.llvm_value }));
        }
        if opr == InfixOpr::Asn {
            let left_expr_result = self.lower_expr(ir_context, left_expr, &context_type)?;
            if let IRExprResult::Place(place) = &left_expr_result {
                if !place.is_mut {
                    return Err(self.error(SemanticError::ExpectedMutable, Some(left_expr.span)));
                }
                let right_expr_result = self.get_value(ir_context, right_expr, &IRContextType::Value(Some(place.type_id)), true)?;
                self.builder.build_store(place.ptr_value, right_expr_result.llvm_value).unwrap();
                return Ok(IRExprResult::Void);
            } else {
                return Err(self.error(SemanticError::ExpectedPlace, Some(left_expr.span)));
            }
        }
        let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;
        let type_id = left_expr_result.type_id;
        let (trait_name, fun_name) = Self::get_core_opr_trait_name(CoreOpr::Infix(opr));
        let fun = self.get_core_trait_fun(type_id, trait_name, fun_name, Some(span))?;
        let mut result = self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(Box::new(IRExprResult::Value(left_expr_result)))), right_expr, span)?;
        if opr == InfixOpr::Neq {
            let bool_type = self.primitive_type(PrimitiveType::Bool)?;
            result.llvm_value = self.build_core_prefix_opr(bool_type, PrefixOpr::Not, result.llvm_value)?.unwrap();
        }
        Ok(IRExprResult::Value(result))
    }
}