use inkwell::values::{BasicValue, BasicValueEnum, IntValue};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRExprContext, IRTemplateValue, IRTypeEnum, IRVarDeclaration, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{ExprNode, Literal, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_array(&mut self, ir_context: &mut IRContext<'ctx>, expr: &Box<ExprNode>, opt_size: &Option<Box<ExprNode>>, span: Span, context_type: &IRExprContext<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
        if let IRExprContext::Value(opt_ctx_t) = context_type {
            if let Some(_) = opt_size {
                panic!()
            } else {
                let ctx_t = if let Some(cur_type) = opt_ctx_t {
                    if let IRTypeEnum::Array { arr_type, size } = self.ir_type(*cur_type).type_enum {
                        IRExprContext::Value(Some(arr_type))
                    } else if let IRTypeEnum::Slice { slice_type } = self.ir_type(*cur_type).type_enum && let None = opt_size {
                        IRExprContext::Value(Some(slice_type))
                    } else {
                        IRExprContext::Value(None)
                    }
                } else {
                    IRExprContext::Value(None)
                };
                let arr_values = self.lower_args_values(ir_context, expr, &[ctx_t.clone()], true)?;
                let arr_inner_type = if arr_values.is_empty() {
                    if let IRExprContext::Value(Some(type_id)) = ctx_t {
                        type_id
                    } else {
                        return Err(self.error(SemanticError::UndeducibleType, Some(span)));
                    }
                } else { arr_values[0].type_id };
                let arr_type = self.array_type(arr_inner_type, arr_values.len() as u64)?;
                let temp_name = format!("tmp arr{}", ir_context.into_fun_context().vars.len());
                let mut llvm_value: inkwell::values::AggregateValueEnum<'_> = self.ir_type(arr_type).llvm_type.into_array_type().get_undef().into();
                for i in 0..arr_values.len() {
                    llvm_value.set_name("tmp_agg");
                    llvm_value = self.builder.build_insert_value(llvm_value, arr_values[i].llvm_value, i as u32, "tmp_insert").unwrap();
                }
                llvm_value.set_name(&temp_name);
                Ok(IRExprResult::Value(IRExprValueResult { type_id: arr_type, llvm_value: llvm_value.as_basic_value_enum() }))
            }
        } else {
            if let Some(cur_size) = opt_size {
                let u64_type = self.primitive_type(PrimitiveType::U64)?;
                let (ctx_t, size_value) = if let IRExprContext::Impl(template_value) = context_type && let IRTemplateValue::Type(ctx_type) = template_value {
                    if let IRTypeEnum::Array { arr_type, size } = self.ir_type(*ctx_type).type_enum {
                        let size_value = IRExprValueResult { type_id: self.primitive_type(PrimitiveType::U64)?, llvm_value: self.llvm_context.i64_type().const_int(size, false).into() };
                        let size = self.get_constant(ir_context, cur_size, &IRExprContext::Impl(IRTemplateValue::Const(size_value)), true)?.llvm_value.into_int_value().get_zero_extended_constant().unwrap();
                        (&IRExprContext::Impl(IRTemplateValue::Type(arr_type)), size)
                    } else {
                        return Ok(IRExprResult::NoImplMatch);
                    }
                } else {
                    let size = self.get_constant(ir_context, cur_size, &IRExprContext::Value(Some(u64_type)), true)?.llvm_value.into_int_value().get_zero_extended_constant().unwrap();
                    (context_type, size)
                };
                let arr_type = self.get_type(ir_context, expr, ctx_t)?;
                let type_id = self.array_type(arr_type, size_value)?;
                Ok(IRExprResult::Type(type_id))
            } else {
                let ctx_t = if let IRExprContext::Impl(template_value) = context_type && let IRTemplateValue::Type(ctx_type) = template_value {
                    if let IRTypeEnum::Slice { slice_type } = self.ir_type(*ctx_type).type_enum && let None = opt_size {
                        &IRExprContext::Impl(IRTemplateValue::Type(slice_type))
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