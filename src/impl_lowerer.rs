
use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::CompilerError, parser::{Function, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_impls(&mut self, scope: IRScopeId, type_id: IRTypeId) -> Result<Vec<IRImpl>, CompilerError> {
        let ir_scope = self.ir_scope(scope);
        let result = Vec::new();
        if let Some(ast_def) = ir_scope.ast_def {
            for impl_def in ast_def.implementations.iter() {
                let templates_keys = self.get_templates_keys_from(&impl_def.templates)?;
            }
        }
        Ok(result)
    }
}