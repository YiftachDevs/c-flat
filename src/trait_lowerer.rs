use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRScope, IRScopeId, IRScopePath, IRTemplatesMap, IRTraitId, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, parser::{Span, Struct, Trait}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_trait_def_in_scope(&self, parent_scope: IRScopeId, name: &str) -> Option<(IRScopeId, &'ctx Trait)> {
        let ir_scope = self.ir_scope(parent_scope);
        if let Some(ast_def) = ir_scope.ast_def {
            for _trait in ast_def.traits.iter() {
                if _trait.name == name {
                    return Some((parent_scope, _trait));
                }
            }
        }
        if let Some(grand_parent_scope) = ir_scope.parent_scope {
            self.find_trait_def_in_scope(grand_parent_scope, name)
        } else {
            None
        }
    }

    pub fn lower_trait(&mut self, parent_scope: IRScopeId, name: &str, templates_map: IRTemplatesMap, call_span: Option<Span>) -> Result<IRTraitId, CompilerError> {

    }
}