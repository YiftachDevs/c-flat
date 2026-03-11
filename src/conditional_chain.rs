use inkwell::{basic_block::BasicBlock, values::{BasicValue, BasicValueEnum}};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRFunContext, IRLoop, IRPhiValues, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{Conditional, ConditionalChain, ControlFlow, Label, Span}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_conditional_chain(&mut self, ir_context: &mut IRContext<'ctx>, conditional_chain: &ConditionalChain, context_type: &IRContextType) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let fun_context = if let IRContext::FunContext(fun_context) = ir_context { fun_context } else { panic!() };
        let fun_value = self.ir_function(fun_context.fun).llvm_value;
        let merge_block = self.llvm_context.append_basic_block(fun_value, "merge");
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        let mut result: IRPhiValues = self.lower_conditional_chain_rec(ir_context, merge_block, conditional_chain, conditional_chain.span, context_type)?;
        let is_never = result.is_empty();
        result = result.into_iter().filter(|(expr_value, _)| expr_value.type_id != void_type).collect();
        let refs: Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)> = result.iter().map(|(val, block)| (val.llvm_value.as_ref().unwrap() as &dyn BasicValue<'_>, *block)).collect();
        self.builder.position_at_end(merge_block);

        if !result.is_empty() {
            let result_type_id = result[0].0.type_id;
            let result_type = self.ir_type(result_type_id);
            let phi = self.builder.build_phi(result_type.llvm_type.unwrap(), "result").unwrap();
            phi.add_incoming(&refs);
            Ok(IRExprValueResult { type_id: result_type_id, llvm_value: Some(phi.as_basic_value()) })
        } else {
            Ok(IRExprValueResult { type_id: if is_never { never_type } else { void_type }, llvm_value: None })
        }
    }

    pub fn lower_conditional_chain_rec(&mut self, ir_context: &mut IRContext<'ctx>, merge_block: BasicBlock<'ctx>, conditional_chain: &ConditionalChain, chain_span: Span, context_type: &IRContextType) -> Result<IRPhiValues<'ctx>, CompilerError> {
        let fun_value = self.ir_function(ir_context.into_fun_context().fun).llvm_value;

        let bool_type = self.primitive_type(PrimitiveType::Bool)?;
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;

        let then_block = self.llvm_context.append_basic_block(fun_value, "then");

        let (cond_block, else_block) = if conditional_chain.kind != Conditional::Else && conditional_chain.kind != Conditional::Loop {
            let cond_block = self.llvm_context.append_basic_block(fun_value, "cond");
            let else_block = self.llvm_context.append_basic_block(fun_value, "else");
            self.builder.build_unconditional_branch(cond_block).unwrap();
            self.builder.position_at_end(cond_block);
            let cond_result = self.get_value(ir_context, conditional_chain.cond_expr.as_ref().unwrap(), &IRContextType::Type(bool_type), true)?;
            self.builder.build_conditional_branch(cond_result.llvm_value.unwrap().into_int_value(), then_block, else_block).unwrap();
            (Some(cond_block), Some(else_block))
        } else {
            self.builder.build_unconditional_branch(then_block).unwrap();
            (None, None)
        };
        self.builder.position_at_end(then_block);
        
        if conditional_chain.kind == Conditional::Loop || conditional_chain.kind == Conditional::While {
            let (loop_block, merge_block) = if conditional_chain.kind == Conditional::Loop { (then_block, merge_block) } else { (cond_block.unwrap(), merge_block) };
            let ir_loop = IRLoop { loop_block, merge_block: merge_block, label: conditional_chain.label.clone(), span: conditional_chain.span, ctx_type: context_type.clone(), phi_values: Vec::new() };
            ir_context.into_fun_context().loop_stack.push(ir_loop);
        }
        let mut phi_result = match &conditional_chain.kind {
            Conditional::If => {
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type)?;
                let cur_block = self.builder.get_insert_block().unwrap();
                if then_result.type_id == never_type {
                    if cur_block.get_terminator().is_none() {
                        self.builder.build_unreachable().unwrap();
                    }
                    Vec::new()
                } else {
                    self.builder.build_unconditional_branch(merge_block).unwrap();
                    vec![(then_result, cur_block)]
                }
            },
            Conditional::Loop => {
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, &IRContextType::Any)?;
                let cur_block = self.builder.get_insert_block().unwrap();
                if then_result.type_id != never_type {
                    self.builder.build_unconditional_branch(then_block).unwrap();
                } else if cur_block.get_terminator().is_none() {
                    self.builder.build_unreachable().unwrap();
                }     
                let ir_loop = ir_context.into_fun_context().loop_stack.pop().unwrap();
                ir_loop.phi_values
            },
            Conditional::While => {
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, &IRContextType::Any)?;
                let cur_block = self.builder.get_insert_block().unwrap();
                if then_result.type_id != never_type {
                    self.builder.build_unconditional_branch(cond_block.unwrap()).unwrap();
                } else if cur_block.get_terminator().is_none() {
                    self.builder.build_unreachable().unwrap();
                }  
                let ir_loop = ir_context.into_fun_context().loop_stack.pop().unwrap();
                ir_loop.phi_values
            },
            Conditional::Else => {
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type)?;
                let cur_block = self.builder.get_insert_block().unwrap();
                if then_result.type_id != never_type {
                    self.builder.build_unconditional_branch(merge_block).unwrap();
                } else if cur_block.get_terminator().is_none() {
                    self.builder.build_unreachable().unwrap();
                }     
                if then_result.type_id == never_type { Vec::new() } else { vec![(then_result, cur_block)] }
            },
            _ => panic!()
        };

        let next_context_type = if let Some((expr_value, _)) = phi_result.get(0) {
            IRContextType::Type(expr_value.type_id)
        } else { IRContextType::Any };

        if let Some(else_chain) = &conditional_chain.else_node {
            self.builder.position_at_end(else_block.unwrap());
            phi_result.extend(self.lower_conditional_chain_rec(ir_context, merge_block, else_chain, chain_span, &next_context_type)?);
        } else if let Some(else_block) = else_block {
            self.builder.position_at_end(else_block);
            if self.type_matches(void_type, &next_context_type) {
                phi_result.push((IRExprValueResult { type_id: void_type, llvm_value: None }, else_block));
            } else {
                return Err(self.error(SemanticError::IncompleteConditionalChain, Some(chain_span)));
            }
            self.builder.build_unconditional_branch(merge_block).unwrap();
        }
        Ok(phi_result)
    }

    pub fn lower_control_flow(&mut self, ir_context: &mut IRContext<'ctx>, control_flow: &ControlFlow, context_type: &IRContextType) -> Result<IRExprValueResult<'ctx>, CompilerError> {      
        match control_flow {
            ControlFlow::Return(expr) => {
                let fun_type = self.ir_function(ir_context.into_fun_context().fun).return_type;
                let fun_type_ctx = IRContextType::Type(fun_type);
                let return_expr_result = self.lower_expr(ir_context, expr, &fun_type_ctx)?;
                let return_result = if let IRExprResult::Empty = return_expr_result {
                    self.ensure_expr_result_value(&IRExprResult::Void, true, expr.span, &fun_type_ctx)?
                } else {
                    self.ensure_expr_result_value(&return_expr_result, true, expr.span, &fun_type_ctx)?
                };
                if let Some(value) = return_result.llvm_value {
                    self.builder.build_return(Some(&value)).expect("Return build failed");
                } else {
                    self.builder.build_return(None).expect("Return build failed");
                }
            },
            ControlFlow::Skip { label, span } => {
                let loop_idx = self.find_loop_idx(ir_context.into_fun_context(), control_flow, label, *span)?;
                let ir_loop = &ir_context.into_fun_context().loop_stack[loop_idx];
                self.builder.build_unconditional_branch(ir_loop.loop_block).unwrap();
            },
            ControlFlow::Break { label, expr, span } => {
                let loop_idx = self.find_loop_idx(ir_context.into_fun_context(), control_flow, label, *span)?;
                let ctx_type = &ir_context.into_fun_context().loop_stack[loop_idx].ctx_type.clone();
                let break_expr_result = self.lower_expr(ir_context, expr, &ctx_type)?;
                let break_value = self.ensure_expr_result_value(if break_expr_result == IRExprResult::Empty { &IRExprResult::Void } else { &break_expr_result }, true, expr.span, &ctx_type)?;
                ir_context.into_fun_context().loop_stack[loop_idx].ctx_type = IRContextType::Type(break_value.type_id);
                ir_context.into_fun_context().loop_stack[loop_idx].phi_values.push((break_value, self.builder.get_insert_block().unwrap()));
                self.builder.build_unconditional_branch(ir_context.into_fun_context().loop_stack[loop_idx].merge_block).unwrap();
            }
            _ => panic!()
        }
        Ok(IRExprValueResult { type_id: self.primitive_type(PrimitiveType::Never)?, llvm_value: None })
    }
    
    fn find_loop_idx(&self, fun_context: &IRFunContext<'ctx>, control_flow: &ControlFlow, label: &Option<Label>, span: Span) -> Result<usize, CompilerError> {
        if let Some((i, ir_loop)) = fun_context.loop_stack.iter().enumerate().rev().find(|(_, ir_loop)| {
            if let Some(cur_label) = &ir_loop.label && let Some(other_label) = label && cur_label == other_label {
                true
            } else if *label == None {
                true
            }
            else { false }
        }) {
            return Ok(i);
        }
        if fun_context.loop_stack.is_empty() {
            return Err(self.error(SemanticError::OutOfTheLoop { control_flow: control_flow.to_string() }, Some(span)))
        }
        return Err(self.error(SemanticError::UnrecognizedLabel, Some(label.as_ref().unwrap().span)));
    }
}