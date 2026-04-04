use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRExprContext, IRFunctionId, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, InfixOpr, PostfixOpr, PrefixOpr, Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_infix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: InfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if opr == InfixOpr::Com {
            return Ok(IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone()));
        }
        if opr == InfixOpr::As {
            let left_expr_result = self.get_value(ir_context, left_expr, &IRExprContext::Value(None), false)?;
            let new_type = self.get_type(ir_context, right_expr , &IRExprContext::Type)?;
            return Ok(IRExprResult::Value(self.convert_type(left_expr_result, new_type)?));
        }
        if opr == InfixOpr::Asn {
            let place = self.get_place(ir_context, left_expr, &IRExprContext::Value(None))?;
            let value = self.get_value(ir_context, right_expr, &IRExprContext::Value(Some(place.type_id)), true)?;
            return self.lower_infix_opr_asn(ir_context, place, value, left_expr.span);
        }
        let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;
        let mut result = self.call_core_trait(ir_context, IRExprResult::Value(left_expr_result), Some(right_expr), &Self::trait_from_opr(CoreOpr::Infix(opr)), span)?;
        if opr == InfixOpr::Neq {
            result.llvm_value = self.build_core_trait_fun_body(result.type_id, Some(result.llvm_value), None, &CoreTraitFun::Not)?;
        }
        if opr == InfixOpr::Leq || opr == InfixOpr::Geq  {
            let eq_result = self.call_core_trait(ir_context, IRExprResult::Value(left_expr_result), Some(right_expr), &Self::trait_from_opr(CoreOpr::Infix(InfixOpr::Eq)), span)?;
            result.llvm_value = self.build_core_trait_fun_body(result.type_id, Some(result.llvm_value), Some(eq_result.llvm_value), &CoreTraitFun::Or)?;
        }
        if opr == InfixOpr::AsnAdd ||  opr == InfixOpr::AsnSub ||  opr == InfixOpr::AsnMul ||  opr == InfixOpr::AsnDiv ||  opr == InfixOpr::AsnMod {
            let place = self.get_place(ir_context, left_expr, &IRExprContext::Value(None))?;
            return self.lower_infix_opr_asn(ir_context, place, result, left_expr.span);
        }
        Ok(IRExprResult::Value(result))
    }

    pub fn lower_infix_opr_asn(&mut self, ir_context: &mut IRContext<'ctx>, place: IRExprPlaceResult<'ctx>, value: IRExprValueResult<'ctx>, place_span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        if !place.is_mut {
            return Err(self.error(SemanticError::ExpectedMutable, Some(place_span)));
        }
        self.builder.build_store(place.ptr_value.into_pointer_value(), value.llvm_value).unwrap();
        Ok(IRExprResult::Void)
    }

    fn convert_type(&mut self, value: IRExprValueResult<'ctx>, new_type: IRTypeId) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let type_id = value.type_id;
        let cur_bit_size = self.get_type_mem_size(type_id);
        let new_bit_size = self.get_type_mem_size(new_type);
        let new_llvm_value = self.ir_type(new_type).llvm_type;
        let new_value = if cur_bit_size < new_bit_size {
            self.builder.build_int_z_extend(value.llvm_value.into_int_value(), new_llvm_value.into_int_type(), "cast").unwrap().into()
        } else if new_bit_size > cur_bit_size {
            self.builder.build_int_truncate(value.llvm_value.into_int_value(), new_llvm_value.into_int_type(), "cast").unwrap().into()
        } else {
            if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(type_id).type_enum && let IRTypeEnum::Primitive(_) = self.ir_type(new_type).type_enum {
                self.builder.build_ptr_to_int(value.llvm_value.into_pointer_value(), new_llvm_value.into_int_type(), "cast").unwrap().into()
            } else if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(new_type).type_enum && let IRTypeEnum::Primitive(_) = self.ir_type(type_id).type_enum {
                self.builder.build_int_to_ptr(value.llvm_value.into_int_value(), new_llvm_value.into_pointer_type(), "cast").unwrap().into()
            } else {
                self.builder.build_bit_cast(value.llvm_value, new_llvm_value, "cast").unwrap().into()
            }
        };
        Ok(IRExprValueResult { type_id: new_type, llvm_value: new_value })
    }

    pub fn trait_from_opr(opr: CoreOpr) -> CoreTraitFun {
        match opr {
            CoreOpr::Prefix(prefix_opr) => {
                match prefix_opr {
                    PrefixOpr::Deref => CoreTraitFun::Deref,
                    PrefixOpr::Not => CoreTraitFun::Not,
                    PrefixOpr::UMin => CoreTraitFun::Neg,
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
                    InfixOpr::Neq => CoreTraitFun::Eq,
                    InfixOpr::Lss => CoreTraitFun::Lss,
                    InfixOpr::Leq => CoreTraitFun::Lss,
                    InfixOpr::Gtr => CoreTraitFun::Gtr,
                    InfixOpr::Geq => CoreTraitFun::Gtr,
                    InfixOpr::And => CoreTraitFun::And,
                    InfixOpr::Or => CoreTraitFun::Or,
                    InfixOpr::Xor => CoreTraitFun::Xor,
                    InfixOpr::AsnAdd => CoreTraitFun::Add,
                    InfixOpr::AsnSub => CoreTraitFun::Sub,
                    InfixOpr::AsnMul => CoreTraitFun::Mul,
                    InfixOpr::AsnDiv => CoreTraitFun::Div,
                    InfixOpr::AsnMod => CoreTraitFun::Mod,
                    _ => panic!()
                }
            }
        }
    }
}