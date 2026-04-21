use std::any::Any;

use inkwell::{types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType}, values::{BasicValueEnum, IntValue}};

use crate::{code_lowerer::{CodeLowerer, CoreOpr, IRContext, IRExprContext, IRFunctionId, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, ExprNodeEnum, InfixOpr, PostfixOpr, PrefixOpr, Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_infix_opr(&mut self, ir_context: &mut IRContext<'ctx>, opr: InfixOpr, left_expr: &Box<ExprNode>, right_expr: &Box<ExprNode>, span: Span, context_type: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        let err = self.error(SemanticError::InvalidOpr(opr.to_string()), Some(span));
        if opr == InfixOpr::Com {
            return Ok(IRExprResult::CommaSeperated(left_expr.clone(), right_expr.clone()));
        }
        if let IRExprContext::TypeConstraint(type_id) = context_type {
            let left_res = self.lower_expr(ir_context, left_expr, context_type)?;
            let right_res = self.lower_expr(ir_context, left_expr, context_type)?;
            match opr {
                InfixOpr::And => {
                    if let IRExprResult::NoTypeConstraintMatch = left_res { return Ok(IRExprResult::NoTypeConstraintMatch); }
                    if let IRExprResult::NoTypeConstraintMatch = right_res { return Ok(IRExprResult::NoTypeConstraintMatch); }
                    return Ok(IRExprResult::Type(*type_id));
                },
                InfixOpr::Or => {
                    if let IRExprResult::Type(_) = left_res { return Ok(IRExprResult::Type(*type_id)); }
                    if let IRExprResult::Type(_) = right_res { return Ok(IRExprResult::Type(*type_id)); }
                    return Ok(IRExprResult::NoTypeConstraintMatch);
                },
                _ => return Err(err)
            }
        }
        if let IRContext::FunContext(_) = ir_context { } else { return Err(err); }
        if opr == InfixOpr::As {
            let left_expr_result = self.get_value(ir_context, left_expr, &IRExprContext::Value(None), false)?;
            let new_type = self.get_type(ir_context, right_expr, &IRExprContext::Type)?;
            return Ok(IRExprResult::Value(self.convert_type(left_expr_result, new_type, span)?));
        }
        if opr == InfixOpr::Asn {
            let place = self.get_place(ir_context, left_expr, &IRExprContext::Value(None))?;
            let value = self.get_value(ir_context, right_expr, &IRExprContext::Value(Some(place.type_id)), true)?;
            return self.lower_infix_opr_asn(ir_context, place, value, left_expr.span);
        }
        if opr == InfixOpr::Eq || opr == InfixOpr::Neq {
            let left_expr_2 = &Box::new(ExprNode { value: ExprNodeEnum::PrefixOpr(PrefixOpr::Addr { is_mut: false }, left_expr.clone()), span });
            let left_expr_ptr_result = self.get_value(ir_context, left_expr_2, &context_type, false)?;
            let left_expr_result = self.deref(ir_context, IRExprResult::Value(left_expr_ptr_result), span)?;
            let right_expr_2 = &Box::new(ExprNode { value: ExprNodeEnum::PrefixOpr(PrefixOpr::Addr { is_mut: false }, right_expr.clone()), span });
            let mut result = self.call_core_trait(ir_context, left_expr_result, Some(right_expr_2), &Self::trait_from_opr(CoreOpr::Infix(opr)), span)?;
            if opr == InfixOpr::Neq {
                result.llvm_value = self.build_core_trait_fun_body(ir_context, result.type_id, Some(result.llvm_value), None, &CoreTraitFun::Not)?;
            }
            return Ok(IRExprResult::Value(result));
        }
        let context_type = if opr == InfixOpr::Range && let IRExprContext::Value(Some(range_t)) = context_type && let IRTypeEnum::Struct(ir_struct) = &self.ir_type(*range_t).type_enum && let IRTemplateValue::Type(int_t) = ir_struct.templates_map[&"T".to_string()] {
            &IRExprContext::Value(Some(int_t))
        } else {
            context_type
        };
        let left_expr_result = self.get_value(ir_context, left_expr, &context_type, false)?;
        let mut result = self.call_core_trait(ir_context, IRExprResult::Value(left_expr_result), Some(right_expr), &Self::trait_from_opr(CoreOpr::Infix(opr)), span)?;
        if opr == InfixOpr::Leq || opr == InfixOpr::Geq  {
            let eq_result = self.call_core_trait(ir_context, IRExprResult::Value(left_expr_result), Some(right_expr), &Self::trait_from_opr(CoreOpr::Infix(InfixOpr::Eq)), span)?;
            result.llvm_value = self.build_core_trait_fun_body(ir_context, result.type_id, Some(result.llvm_value), Some(eq_result.llvm_value), &CoreTraitFun::Or)?;
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

    fn convert_type(&mut self, value: IRExprValueResult<'ctx>, new_type: IRTypeId, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let type_id = value.type_id;
        let err = self.error(SemanticError::InvalidTypeConversion(self.format_type(type_id), self.format_type(new_type)), Some(span));
        let cur_llvm_value = value.llvm_value;
        let cur_bit_size = self.get_type_mem_size(type_id);
        let new_bit_size = self.get_type_mem_size(new_type);
        let new_llvm_type = self.ir_type(new_type).llvm_type;

        let int_cast  = |signed: bool, val: IntValue<'ctx>, ty: IntType<'ctx>| -> BasicValueEnum<'ctx> {
            if cur_bit_size < new_bit_size {
                if signed {
                    self.builder.build_int_s_extend(val, ty, "cast").unwrap().into()
                } else {
                    self.builder.build_int_z_extend(val, ty, "cast").unwrap().into()
                }
            } else if cur_bit_size > new_bit_size {
                self.builder.build_int_truncate(val, ty, "cast").unwrap().into()
            } else {
                cur_llvm_value
            }
        };
        let is_int_like = |p: PrimitiveType| p.is_int() || p.is_uint() || p == PrimitiveType::Char || p == PrimitiveType::Bool;


        let new_value = if let IRTypeEnum::Primitive(cur_prim) = self.ir_type(type_id).type_enum && let IRTypeEnum::Primitive(new_prim) = self.ir_type(new_type).type_enum {
            if new_prim.is_float() {
                if cur_prim.is_int() {
                    self.builder.build_signed_int_to_float(cur_llvm_value.into_int_value(), new_llvm_type.into_float_type(), "cast").unwrap().into()
                } else if cur_prim.is_uint() {
                    self.builder.build_unsigned_int_to_float(cur_llvm_value.into_int_value(), new_llvm_type.into_float_type(), "cast").unwrap().into()
                } else if cur_prim.is_float() {
                    if cur_bit_size < new_bit_size {
                        self.builder.build_float_ext(cur_llvm_value.into_float_value(), new_llvm_type.into_float_type(), "cast").unwrap().into()
                    } else if cur_bit_size > new_bit_size {
                        self.builder.build_float_trunc(cur_llvm_value.into_float_value(), new_llvm_type.into_float_type(), "cast").unwrap().into()
                    } else {
                        cur_llvm_value
                    }
                } else {
                    return Err(err);
                }
            } else if new_prim.is_int() || new_prim.is_uint() || new_prim == PrimitiveType::Char || new_prim == PrimitiveType::Bool {
                if cur_prim.is_float() {
                    if new_prim.is_int() {
                        self.builder.build_float_to_signed_int(cur_llvm_value.into_float_value(), new_llvm_type.into_int_type(), "cast").unwrap().into()
                    } else {
                        self.builder.build_float_to_unsigned_int(cur_llvm_value.into_float_value(), new_llvm_type.into_int_type(), "cast").unwrap().into()
                    }
                } else if cur_prim.is_int() || cur_prim.is_uint() || cur_prim == PrimitiveType::Char || cur_prim == PrimitiveType::Bool {
                    int_cast(cur_prim.is_int(), cur_llvm_value.into_int_value(), new_llvm_type.into_int_type())
                } else {
                    return Err(err);
                }
            } else {
                return Err(err);
            }
        } else if cur_bit_size < new_bit_size {
            self.builder.build_int_z_extend(cur_llvm_value.into_int_value(), new_llvm_type.into_int_type(), "cast").unwrap().into()
        } else if cur_bit_size > new_bit_size {
            self.builder.build_int_truncate(cur_llvm_value.into_int_value(), new_llvm_type.into_int_type(), "cast").unwrap().into()
        } else {
            if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(type_id).type_enum && let IRTypeEnum::Primitive(_) = self.ir_type(new_type).type_enum {
                self.builder.build_ptr_to_int(cur_llvm_value.into_pointer_value(), new_llvm_type.into_int_type(), "cast").unwrap().into()
            } else if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(new_type).type_enum && let IRTypeEnum::Primitive(_) = self.ir_type(type_id).type_enum {
                self.builder.build_int_to_ptr(cur_llvm_value.into_int_value(), new_llvm_type.into_pointer_type(), "cast").unwrap().into()
            } else {
                self.builder.build_bit_cast(cur_llvm_value, new_llvm_type, "cast").unwrap().into()
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
                    InfixOpr::Range => CoreTraitFun::UpTo,
                    InfixOpr::Shl => CoreTraitFun::Shl,
                    InfixOpr::Shr => CoreTraitFun::Shr,
                    _ => panic!("{:?}", infix_opr)
                }
            }
        }
    }
}