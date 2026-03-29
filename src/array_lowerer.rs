use inkwell::values::{BasicValueEnum, IntValue};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRTemplateValue, IRTypeEnum, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, Literal, Span}};


impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_array(&mut self, ir_context: &mut IRContext<'ctx>, expr: &Box<ExprNode>, opt_size: &Option<Box<ExprNode>>, span: Span, context_type: &IRContextType<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRContextType::Value(opt_ctx_t) = context_type {
            if let Some(_) = opt_size {
                panic!()
            } else {
                let ctx_t = if let Some(cur_type) = opt_ctx_t {
                    if let IRTypeEnum::Array { arr_type, size } = self.ir_type(*cur_type).type_enum {
                        IRContextType::Value(Some(arr_type))
                    } else if let IRTypeEnum::Slice { slice_type } = self.ir_type(*cur_type).type_enum && let None = opt_size {
                        IRContextType::Value(Some(slice_type))
                    } else {
                        IRContextType::Value(None)
                    }
                } else {
                    IRContextType::Value(None)
                };
                let arr_values = self.lower_args_values(ir_context, expr, &[ctx_t.clone()], true)?;
                let arr_inner_type = if arr_values.is_empty() {
                    if let IRContextType::Value(Some(type_id)) = ctx_t {
                        type_id
                    } else {
                        return Err(self.error(SemanticError::UndeducibleType, Some(span)));
                    }
                } else { arr_values[0].type_id };
                let arr_type = self.array_type(arr_inner_type, arr_values.len() as u64)?;
                let arr_value = self.get_type_zero(arr_type);
                Ok(IRExprResult::Value(arr_value))
            }
        } else {
            if let Some(cur_size) = opt_size {
                let u64_type = self.primitive_type(PrimitiveType::U64)?;
                let (ctx_t, size_value) = if let IRContextType::Impl(template_value) = context_type && let IRTemplateValue::Type(ctx_type) = template_value {
                    if let IRTypeEnum::Array { arr_type, size } = self.ir_type(*ctx_type).type_enum {
                        let size_value = IRExprValueResult { type_id: self.primitive_type(PrimitiveType::U64)?, llvm_value: self.llvm_context.i64_type().const_int(size, false).into() };
                        let size = self.get_constant(ir_context, cur_size, &IRContextType::Impl(IRTemplateValue::Const(size_value)), true)?.llvm_value.into_int_value().get_zero_extended_constant().unwrap();
                        (&IRContextType::Impl(IRTemplateValue::Type(arr_type)), size)
                    } else {
                        return Ok(IRExprResult::NoImplMatch);
                    }
                } else {
                    let size = self.get_constant(ir_context, cur_size, &IRContextType::Value(Some(u64_type)), true)?.llvm_value.into_int_value().get_zero_extended_constant().unwrap();
                    (context_type, size)
                };
                let arr_type = self.get_type(ir_context, expr, ctx_t)?;
                let type_id = self.array_type(arr_type, size_value)?;
                Ok(IRExprResult::Type(type_id))
            } else {
                let ctx_t = if let IRContextType::Impl(template_value) = context_type && let IRTemplateValue::Type(ctx_type) = template_value {
                    if let IRTypeEnum::Slice { slice_type } = self.ir_type(*ctx_type).type_enum && let None = opt_size {
                        &IRContextType::Impl(IRTemplateValue::Type(slice_type))
                    } else {
                        return Ok(IRExprResult::NoImplMatch);
                    }
                } else { context_type };
                let slice_type = self.get_type(ir_context, expr, ctx_t)?;
                let type_id = self.slice_type(slice_type)?;
                Ok(IRExprResult::Type(type_id))
            }
        }
    }
}