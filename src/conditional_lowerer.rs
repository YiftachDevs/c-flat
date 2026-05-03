use inkwell::{basic_block::BasicBlock, values::{BasicValue, BasicValueEnum}};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRExprContext, IRFunContext, IRLoop, IRPhiValues, IRVarDeclaration, IRVariable, PrimitiveType}, core_lowerer::CoreTraitFun, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprResult, IRExprValueResult}, parser::{Conditional, ConditionalChain, ControlFlow, Label, Span}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn lower_conditional_chain(&mut self, ir_context: &mut IRContext<'ctx>, conditional_chain: &ConditionalChain, context_type: &IRExprContext<'ctx>) -> Result<IRExprValueResult<'ctx>, CompilerError> {
        let fun_context = if let IRContext::FunContext(fun_context) = ir_context { fun_context } else { panic!() };
        let fun_value = self.ir_function(fun_context.fun).llvm_value;
        let merge_block = self.llvm_context.append_basic_block(fun_value, "merge");
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        let result: IRPhiValues = self.lower_conditional_chain_rec(ir_context, merge_block, conditional_chain, conditional_chain.span, context_type)?;
        let refs: Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)> = result.iter().map(|(val, block)| (&val.llvm_value as &dyn BasicValue<'_>, *block)).collect();
        self.builder.position_at_end(merge_block);

        if !result.is_empty() {
            let result_type_id = result[0].0.type_id;
            let result_type = self.ir_type(result_type_id);
            let phi = self.builder.build_phi(result_type.llvm_type, "result").unwrap();
            phi.add_incoming(&refs);
            Ok(IRExprValueResult { type_id: result_type_id, llvm_value: phi.as_basic_value() })
        } else {
            Ok(self.get_type_zero(never_type))
        }
    }

    pub fn lower_conditional_chain_rec(&mut self, ir_context: &mut IRContext<'ctx>, merge_block: BasicBlock<'ctx>, conditional_chain: &ConditionalChain, chain_span: Span, context_type: &IRExprContext<'ctx>) -> Result<IRPhiValues<'ctx>, CompilerError> {
        let fun_value = self.ir_function(ir_context.into_fun_context().fun).llvm_value;

        let bool_type = self.primitive_type(PrimitiveType::Bool)?;
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;

        let break_var_count = ir_context.into_fun_context().vars.len();

        let iter_place = if conditional_chain.kind == Conditional::For {
            let iter_result = self.get_value(ir_context, conditional_chain.cond_expr.as_ref().unwrap(), &IRExprContext::Value(None), false)?;
            /*if let None = self.find_impl_of_core_trait(iter_result.type_id, &CoreTraitFun::HasNext)? {
                return Err(self.error(SemanticError::ExpectedIter, Some(conditional_chain.cond_expr.as_ref().unwrap().span)));
            }*/
            let temp_name = format!("tmp_iter{}", ir_context.into_fun_context().vars.len());
            let var_dec = IRVarDeclaration { name: temp_name, type_id: iter_result.type_id, is_mut: true, is_static: false };
            let var_id = self.alloc_var(ir_context.into_fun_context(), &var_dec, Some(iter_result.llvm_value), Some(conditional_chain.span))?;
            Some(ir_context.into_fun_context().vars[var_id].place.clone())
        } else { None };

        let skip_var_count = ir_context.into_fun_context().vars.len();

        let then_block = self.llvm_context.append_basic_block(fun_value, "then");

        let (cond_block, else_block) = if conditional_chain.kind != Conditional::Else && conditional_chain.kind != Conditional::Loop {
            let cond_block = self.llvm_context.append_basic_block(fun_value, "cond");
            let else_block = self.llvm_context.append_basic_block(fun_value, "else");
            self.builder.build_unconditional_branch(cond_block).unwrap();
            self.builder.position_at_end(cond_block);
            let cond_result = if conditional_chain.kind == Conditional::For {
                self.call_core_trait(ir_context, IRExprResult::Place(iter_place.clone().unwrap()), None, &Vec::new(), &CoreTraitFun::HasNext, conditional_chain.cond_expr.as_ref().unwrap().span)?
            } else {
                self.get_value(ir_context, conditional_chain.cond_expr.as_ref().unwrap(), &IRExprContext::Value(Some(bool_type)), true)?
            };
            self.builder.build_conditional_branch(cond_result.llvm_value.into_int_value(), then_block, else_block).unwrap();
            (Some(cond_block), Some(else_block))
        } else {
            self.builder.build_unconditional_branch(then_block).unwrap();
            (None, None)
        };
        self.builder.position_at_end(then_block);
        
        let vars_unmoved_states = self.save_vars_move_state(ir_context.into_fun_context());
        
        if conditional_chain.kind == Conditional::Loop || conditional_chain.kind == Conditional::While || conditional_chain.kind == Conditional::For {
            let (loop_block, merge_block) = if conditional_chain.kind == Conditional::Loop { (then_block, merge_block) } else { (cond_block.unwrap(), merge_block) };
            let ir_loop = IRLoop { loop_block, merge_block: merge_block, label: conditional_chain.label.clone(), break_var_count, skip_var_count, ctx_type: context_type.clone(), phi_values: Vec::new() };
            ir_context.into_fun_context().loop_stack.push(ir_loop);
        }
        let mut phi_result = match &conditional_chain.kind {
            Conditional::If => {
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type, false)?;
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
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, &IRExprContext::Value(None), false)?;
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
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, &IRExprContext::Value(None), false)?;
                let cur_block = self.builder.get_insert_block().unwrap();
                if then_result.type_id != never_type {
                    self.builder.build_unconditional_branch(cond_block.unwrap()).unwrap();
                } else if cur_block.get_terminator().is_none() {
                    self.builder.build_unreachable().unwrap();
                }  
                let ir_loop = ir_context.into_fun_context().loop_stack.pop().unwrap();
                ir_loop.phi_values
            },
            Conditional::For => {
                let prev_len = ir_context.into_fun_context().vars.len();
                let span =  conditional_chain.cond_expr.as_ref().unwrap().span;
                let next_value = self.call_core_trait(ir_context, IRExprResult::Place(iter_place.unwrap()), None, &Vec::new(), &CoreTraitFun::Next, span)?;
                let name = conditional_chain.iter_name.as_ref().unwrap().clone();
                self.alloc_var(ir_context.into_fun_context(), &IRVarDeclaration { name, type_id: next_value.type_id, is_mut: false, is_static: false }, Some(next_value.llvm_value), Some(span))?;
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, &IRExprContext::Value(None), false)?;
                self.drop_vars(ir_context, prev_len, conditional_chain.span)?;
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
                let then_result: IRExprValueResult<'_> = self.lower_scope(ir_context, &conditional_chain.then_scope, context_type, false)?;
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

        let vars_moved_states = self.save_vars_move_state(ir_context.into_fun_context());
        self.load_vars_move_states(ir_context.into_fun_context(), &vars_unmoved_states, true);

        let next_context_type = if let Some((expr_value, _)) = phi_result.get(0) {
            Some(expr_value.type_id)
        } else { None };

        if let Some(else_chain) = &conditional_chain.else_node {
            self.builder.position_at_end(else_block.unwrap());
            phi_result.extend(self.lower_conditional_chain_rec(ir_context, merge_block, else_chain, chain_span, &IRExprContext::Value(next_context_type))?);
        } else if let Some(else_block) = else_block {
            self.builder.position_at_end(else_block);
            if let Ok(_) = self.ensure_type_matches(void_type, next_context_type, None, false) {
                phi_result.push((self.get_type_zero(void_type), else_block));
            } else {
                return Err(self.error(SemanticError::IncompleteConditionalChain, Some(chain_span)));
            }
            self.builder.build_unconditional_branch(merge_block).unwrap();
        }

        self.load_vars_move_states(ir_context.into_fun_context(), &vars_moved_states, false);

        Ok(phi_result)
    }

    pub fn lower_control_flow(&mut self, ir_context: &mut IRContext<'ctx>, control_flow: &ControlFlow) -> Result<IRExprValueResult<'ctx>, CompilerError> {      
        match control_flow {
            ControlFlow::Return(expr) => {
                let fun_type = self.ir_function(ir_context.into_fun_context().fun).return_type;
                let fun_type_ctx = IRExprContext::Value(Some(fun_type));
                let saved_vars = self.save_vars(ir_context.into_fun_context());
                let return_expr_result = self.lower_expr(ir_context, expr, &fun_type_ctx)?;
                let return_result = if let IRExprResult::Empty = return_expr_result {
                    self.ensure_expr_result_value(ir_context, &IRExprResult::Void, true, expr.span, &fun_type_ctx)?
                } else {
                    self.ensure_expr_result_value(ir_context, &return_expr_result, true, expr.span, &fun_type_ctx)?
                };
                self.drop_vars(ir_context, 0, expr.span)?;
                self.builder.build_return(Some(&return_result.llvm_value)).expect("Return build failed");
                self.load_vars(ir_context.into_fun_context(), saved_vars);
            },
            ControlFlow::Skip { label, span } => {
                let loop_idx = self.find_loop_idx(ir_context.into_fun_context(), control_flow, label, *span)?;
                let skip_var_count = ir_context.into_fun_context().loop_stack[loop_idx].skip_var_count;
                let saved_vars = self.save_vars(ir_context.into_fun_context());
                self.drop_vars(ir_context, skip_var_count, *span)?;
                self.load_vars(ir_context.into_fun_context(), saved_vars);
                self.builder.build_unconditional_branch(ir_context.into_fun_context().loop_stack[loop_idx].loop_block).unwrap();
            },
            ControlFlow::Break { label, expr, span } => {
                let loop_idx = self.find_loop_idx(ir_context.into_fun_context(), control_flow, label, *span)?;
                let ctx_type = &ir_context.into_fun_context().loop_stack[loop_idx].ctx_type.clone();
                let break_expr_result = self.lower_expr(ir_context, expr, &ctx_type)?;
                let break_value = self.ensure_expr_result_value(ir_context, if break_expr_result == IRExprResult::Empty { &IRExprResult::Void } else { &break_expr_result }, true, expr.span, &ctx_type)?;
                ir_context.into_fun_context().loop_stack[loop_idx].ctx_type = IRExprContext::Value(Some(break_value.type_id));
                ir_context.into_fun_context().loop_stack[loop_idx].phi_values.push((break_value, self.builder.get_insert_block().unwrap()));
                let break_var_count = ir_context.into_fun_context().loop_stack[loop_idx].break_var_count;
                let saved_vars = self.save_vars(ir_context.into_fun_context());
                self.drop_vars(ir_context, break_var_count, *span)?;
                self.load_vars(ir_context.into_fun_context(), saved_vars);
                self.builder.build_unconditional_branch(ir_context.into_fun_context().loop_stack[loop_idx].merge_block).unwrap();
            }
            _ => panic!()
        }
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        Ok(self.get_type_zero(never_type))
    }
    
    fn find_loop_idx(&self, fun_context: &IRFunContext<'ctx>, control_flow: &ControlFlow, label: &Option<Label>, span: Span) -> Result<usize, CompilerError> {
        if let Some((i, ir_loop)) = fun_context.loop_stack.iter().enumerate().rev().find(|(_, ir_loop)| {
            if let Some(cur_label) = &ir_loop.label && let Some(other_label) = label && cur_label.label == other_label.label {
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