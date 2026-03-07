use inkwell::{basic_block::BasicBlock, values::{BasicValue, BasicValueEnum}};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::IRExprValueResult, parser::{Conditional, ConditionalChain, Span}};



impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_conditional_chain(&mut self, ir_context: &mut IRContext<'ctx>, conditional_chain: &Box<ConditionalChain>, context_type: &IRContextType) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let fun_context = if let IRContext::FunContext(fun_context) = ir_context { fun_context } else { panic!() };
        let fun_value = self.ir_function(fun_context.fun).llvm_value;
        let merge_block = self.llvm_context.append_basic_block(fun_value, "merge");
        let result = self.lower_conditional_chain_rec(ir_context, merge_block, conditional_chain, conditional_chain.span, context_type)?;
        self.builder.position_at_end(merge_block);
        if !result.is_empty() {
            let result_type_id = result[0].0.type_id;
            let result_type = self.ir_type(result_type_id);
            let phi = self.builder.build_phi(result_type.llvm_type.unwrap(), "result").unwrap();
            let refs: Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)> = result.iter().map(|(val, block)| (val.llvm_value.as_ref().unwrap() as &dyn BasicValue<'_>, *block)).collect();
            phi.add_incoming(&refs);
            Ok(IRExprValueResult { type_id: result_type_id, llvm_value: Some(phi.as_basic_value()) })
        } else {
            Ok(IRExprValueResult { type_id: self.primitive_type(PrimitiveType::Void)?, llvm_value: None })
        }
    }

    pub fn lower_conditional_chain_rec(&mut self, ir_context: &mut IRContext<'ctx>, merge_block: BasicBlock, conditional_chain: &Box<ConditionalChain>, chain_span: Span, context_type: &IRContextType) -> Result<Vec<(IRExprValueResult<'ctx>, BasicBlock<'ctx>)>, CompilerError> {
        let fun_context = if let IRContext::FunContext(fun_context) = ir_context { fun_context } else { panic!() };
        let fun_value = self.ir_function(fun_context.fun).llvm_value;

        if let Some(cond) = &conditional_chain.cond_expr {
            let bool_type = self.primitive_type(PrimitiveType::Bool)?;
            let void_type = self.primitive_type(PrimitiveType::Void)?;
            let then_block = self.llvm_context.append_basic_block(fun_value, "then");
            let else_block = self.llvm_context.append_basic_block(fun_value, "else");
            let cond_expr = self.get_value(ir_context, cond, &IRContextType::Type(bool_type), true)?;
            self.builder.build_conditional_branch(cond_expr.llvm_value.unwrap().into_int_value(), then_block, else_block).unwrap();
            self.builder.position_at_end(then_block);
            let then_result = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type)?;
            self.builder.build_unconditional_branch(merge_block).unwrap();
            let mut rec_result = if let Some(_) = then_result.llvm_value { vec![(then_result, then_block)] } else { Vec::new() };
            if let Some(else_chain) = &conditional_chain.else_node {
                self.builder.position_at_end(else_block);
                rec_result.extend(self.lower_conditional_chain_rec(ir_context, merge_block, else_chain, chain_span, &IRContextType::Type(then_result.type_id))?);
            } else if then_result.type_id != void_type {
                return Err(self.error(SemanticError::IncompleteConditionalChain, Some(chain_span)));
            } else {
                self.builder.position_at_end(else_block);
                self.builder.build_unconditional_branch(merge_block).unwrap();
            }
            Ok(rec_result)
        } else {
            let then_result = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type)?;
            self.builder.build_unconditional_branch(merge_block).unwrap();
            let rec_result = if let Some(_) = then_result.llvm_value { vec![(then_result, self.builder.get_insert_block().unwrap())] } else { Vec::new() };
            Ok(rec_result)
        }
    }
}