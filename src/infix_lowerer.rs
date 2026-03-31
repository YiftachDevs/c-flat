use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRContextType, IRFunctionId, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, InfixOpr, PostfixOpr, PrefixOpr, Span, Struct, Trait}};

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
                self.builder.build_store(place.ptr_value.into_pointer_value(), right_expr_result.llvm_value).unwrap();
                return Ok(IRExprResult::Void);
            } else {
                return Err(self.error(SemanticError::ExpectedPlace, Some(left_expr.span)));
            }
        }
        let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;
        let mut result = self.call_core_trait(ir_context, IRExprResult::Value(left_expr_result), Some(right_expr), &Self::trait_from_opr(CoreOpr::Infix(opr)), span)?;
        if opr == InfixOpr::Neq {
            result = self.call_core_trait(ir_context, IRExprResult::Value(result), None, &Self::trait_from_opr(CoreOpr::Prefix(PrefixOpr::Not)), span)?;
        }
        Ok(IRExprResult::Value(result))
    }

    fn trait_from_opr(opr: CoreOpr) -> CoreTraitFun {
        match opr {
            CoreOpr::Prefix(prefix_opr) => {
                match prefix_opr {
                    PrefixOpr::Deref => CoreTraitFun::Deref,
                    PrefixOpr::Not => CoreTraitFun::Not,
                    _ => panic!()
                }
            },
            CoreOpr::Infix(infix_opr) => {
                match infix_opr {
                    InfixOpr::Add => CoreTraitFun::Add,
                    InfixOpr::Sub => CoreTraitFun::Sub,
                    InfixOpr::Mul => CoreTraitFun::Mul,
                    InfixOpr::Div => CoreTraitFun::Div,
                    InfixOpr::Mod => CoreTraitFun::Mod,
                    InfixOpr::Eq => CoreTraitFun::Eq,
                    InfixOpr::Lss => CoreTraitFun::Lss,
                    InfixOpr::Gtr => CoreTraitFun::Gtr,
                    _ => panic!()
                }
            }
        }
    }
}