
use indexmap::IndexMap;
use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_impls(&mut self, parent_scope: IRScopeId, type_id: IRTypeId) -> Result<(), CompilerError> {
        self.types_table[type_id].lowered_impls = Some(Vec::new());
        let mut result = Vec::new();
        if let Some(ast_def) = self.ir_scope(parent_scope).ast_def {
            for impl_def in ast_def.implementations.iter() {
                let impl_templates_keys = self.get_templates_keys_from(&impl_def.templates)?;
                if let Some(templates_map) = self.match_impl_rename_me_type(parent_scope, impl_templates_keys, &impl_def.target_type, type_id)? {
                    let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
                    new_templates_map.insert(IRTemplateKey { name: "Self".to_string(), constraint: IRContextType::Any }, type_id );
                    new_templates_map.extend(templates_map.clone());

                    let impl_id = self.impls_table.len();
                    let impl_scope_path = IRScopePath::Impl(impl_id);
                    let ir_scope = IRScope { parent_scope: Some(parent_scope), path: impl_scope_path, path_string: self.format_type(type_id), templates_map: new_templates_map, ast_def: Some(&impl_def.scope) };
                    let scope_id = self.scope_id(ir_scope);

                    if let Some(trait_expr) = impl_def.opt_trait {
                        let trait_id = 
                    }

                    let ir_impl = IRImpl { scope: scope_id, type_id, ast_def: impl_def, trait_id: None };
                    self.impls_table.push(ir_impl);
                    result.push(impl_id);
                }
            }
        }
        self.types_table[type_id].lowered_impls = Some(result);
        Ok(())
    }

    pub fn match_impl_rename_me_type(&mut self, parent_scope: IRScopeId, templates_keys: Vec<IRTemplateKey>, expr: &ExprNode, target_type: IRTypeId) -> Result<Option<IRTemplatesMap>, CompilerError> {
        let mut ir_context = IRContext::ImplDefContext(parent_scope, templates_keys, IndexMap::new(), target_type);
        let result_type_id: usize = self.get_type(&mut ir_context, expr, &IRContextType::Any)?;
        if result_type_id == target_type && let IRContext::ImplDefContext(_, _, map, _) = ir_context {
            return Ok(Some(map));
        }
        Ok(None)
    }

    pub fn find_trait(&mut self, ) {

    }
}