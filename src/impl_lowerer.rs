
use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::CompilerError, parser::{ExprNode, Function, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_impls(&mut self, parent_scope: IRScopeId, type_id: IRTypeId) -> Result<Vec<IRImplId>, CompilerError> {
        let mut result = Vec::new();
        if let Some(ast_def) = self.ir_scope(parent_scope).ast_def {
            for impl_def in ast_def.implementations.iter() {
                let impl_templates_keys = self.get_templates_keys_from(&impl_def.templates)?;
                if let Some(templates_map) = self.match_type(parent_scope, impl_templates_keys, &impl_def.target_type, type_id)? {
                    let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
                    new_templates_map.extend(templates_map.clone());

                    let impl_id = self.impls_table.len();
                    let impl_scope_path = IRScopePath::Impl(impl_id);
                    let ir_scope = IRScope { parent_scope: Some(parent_scope), path: impl_scope_path, path_string: self.format_type(type_id), templates_map: new_templates_map, ast_def: Some(&impl_def.scope) };
                    let scope_id = self.scope_id(ir_scope);
                    let ir_impl = IRImpl { scope: scope_id, type_id, ast_def: impl_def, trait_id: None };
                    self.impls_table.push(ir_impl);
                    result.push(impl_id);
                }
            }
        }
        Ok(result)
    }

    pub fn match_type(&mut self, parent_scope: IRScopeId, templates_keys: Vec<IRTemplateKey>, expr: &ExprNode, target_type: IRTypeId) -> Result<Option<IRTemplatesMap>, CompilerError> {
        let mut ir_context = IRContext::ImplDefContext(parent_scope, templates_keys, IndexMap::new());
        let expr_result = self.lower_expr(&mut ir_context, expr, Some(target_type))?;
        Ok(None)
    }
}