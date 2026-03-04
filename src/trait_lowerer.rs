use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRScope, IRScopeId, IRScopePath, IRTemplateValue, IRTemplatesMap, IRTrait, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, parser::{Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_trait_def_in_scope(&self, parent_scope: IRScopeId, name: &str, opt_call_span: Option<Span>) -> Result<Option<(IRScopeId, &'ctx Trait)>, CompilerError> {
        if let Some(actual_scope) = self.get_name_parent_scope(parent_scope, name, opt_call_span)? {
            let ir_scope = self.ir_scope(actual_scope);
            if let Some(def) = ir_scope.ast_def.unwrap().traits.iter().find(|def| def.name == name) {
                return Ok(Some((actual_scope, def)));
            }
        }
        Ok(None)
    }

    fn find_existing_trait(&mut self, parent_scope: IRScopeId, name: &str, templates_map: &IRTemplatesMap) -> Option<IRTypeId> {
        for (i, ir_trait) in self.traits_table.iter().enumerate() {
            let ir_scope = self.ir_scope(ir_trait.scope);
            if ir_scope.parent_scope.unwrap() == parent_scope && ir_trait.ast_def.name == name && ir_trait.templates_map == *templates_map {
                return Some(i);
            }
        }
        None
    }

    pub fn lower_trait(&mut self, parent_scope: IRScopeId, name: &str, templates_values: &[IRTemplateValue], call_span: Option<Span>) -> Result<IRTraitId, CompilerError> {
        let trait_def = self.find_trait_def_in_scope(parent_scope, name, call_span)?.unwrap().1;
        let templates_keys = self.get_templates_keys_from(&trait_def.templates)?;
        let templates_map = self.merge_templates_keys_values(&templates_keys, templates_values, call_span)?;
        if let Some(id) = self.find_existing_trait(parent_scope, name, &templates_map) {
            return Ok(id);
        }
        let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
        new_templates_map.extend(templates_map.clone());

        let trait_id: usize = self.traits_table.len();
        let trait_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Trait(trait_id), templates_map: new_templates_map, ast_def: None });
        let mut ir_context = IRContext::ScopeContext(trait_scope);
        self.traits_table.push(IRTrait { scope: trait_scope, parent_scope: parent_scope, templates_map: templates_map.clone(), ast_def: trait_def });

        self.ensure_templates_constraints(&mut ir_context, &templates_map, &trait_def.templates, call_span)?;

        Ok(trait_id)
    }
}