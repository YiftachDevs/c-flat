
use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Implementation, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_fun_impl(&mut self, parent_scope: IRScopeId, type_id: IRTypeId, fun_name: &str, call_span: Span) -> Result<Option<IRImplId>, CompilerError> {
        for lowered_impl_id in &self.ir_type(type_id).lowered_impls {
            let ir_impl = self.ir_impl(*lowered_impl_id);
            if let Some(_) = ir_impl.ast_def.scope.functions.iter().find(|fun| fun.name == fun_name) {
                return Ok(Some(*lowered_impl_id));
            }
        }
        let mut found_impls_ids = Vec::new();
        for impl_def in self.ir_scope(parent_scope).ast_def.unwrap().implementations.iter() {
            if impl_def.scope.functions.iter().any(|fun| fun.name == fun_name) {
                let impl_templates_keys = self.get_templates_keys_from(&impl_def.templates)?;
                if let Some(templates_map) = self.match_impl_type(parent_scope, impl_templates_keys, &impl_def.target_type, type_id)? {
                    let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
                    new_templates_map.insert("Self".to_string(), type_id );
                    new_templates_map.extend(templates_map.clone());

                    let impl_id = self.impls_table.len();
                    let scope_id = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Impl(impl_id), templates_map: new_templates_map, ast_def: Some(&impl_def.scope) });
                    let mut ir_context = IRContext::ScopeContext(scope_id);

                    if let Err(_) = self.ensure_templates_constraints(&mut ir_context, &templates_map,&impl_def.templates, Some(call_span)) {
                        continue;
                    }
                    
                    let opt_trait_id = if let Some(trait_expr) = &impl_def.opt_trait {
                        if let IRExprResult::Trait(trait_id) = self.lower_expr(&mut ir_context, trait_expr, &IRContextType::Any)? {
                            Some(trait_id)
                        } else {
                            return Err(self.error(SemanticError::ExpectedTrait, Some(trait_expr.span)));   
                        }
                    } else { None };

                    let ir_impl = IRImpl { scope: scope_id, type_id, trait_id: opt_trait_id, ast_def: impl_def };
                    self.impls_table.push(ir_impl);

                    self.types_table[type_id].lowered_impls.push(impl_id);

                    found_impls_ids.push(impl_id);
                }
            }
        }
        for module in self.ir_scope(parent_scope).ast_def.unwrap().modules.iter() {
            let scope = self.get_module_scope_in_scope(parent_scope, &module.name).unwrap();
            if let Some(impl_id) = self.find_fun_impl(scope, type_id, fun_name, call_span)? {
                found_impls_ids.push(impl_id);
            }
        }
        match found_impls_ids.len() {
            2.. => Err(self.error(SemanticError::CollidingImpl { type_str: self.format_type(type_id), fun_name: fun_name.to_string() }, Some(call_span))),
            1 => Ok(Some(found_impls_ids[0])),
            0 => Ok(None)
        }
    }

    pub fn match_impl_type(&mut self, parent_scope: IRScopeId, templates_keys: Vec<IRTemplateKey>, expr: &ExprNode, target_type: IRTypeId) -> Result<Option<IRTemplatesMap>, CompilerError> {
        let mut ir_context = IRContext::ImplDefContext(parent_scope, templates_keys, IndexMap::new());
        let expr_result = self.lower_expr(&mut ir_context, expr, &IRContextType::Impl(target_type))?;
        if let IRExprResult::Type(result_type) = expr_result && result_type == target_type && let IRContext::ImplDefContext(_, _, map) = ir_context {
            return Ok(Some(map));
        }
        Ok(None)
    }

    pub fn find_trait(&mut self, ) {

    }
}