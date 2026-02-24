use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRScope, IRScopeId, IRScopePath, IRTemplatesValues, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables}, errors::CompilerError, parser::{Span, Struct}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_struct_def_in_scope(&self, parent_scope: IRScopeId, name: &str) -> Option<&'ctx Struct> {
        let ir_scope = self.ir_scope(parent_scope);
        for strct in ir_scope.ast_def.unwrap().structs.iter() {
            if strct.name == name {
                return Some(strct);
            }
        }
        None
    }

    fn find_type_in_scope(&mut self, _parent_scope: IRScopeId, name: &str, _templates_values: &IRTemplatesValues) -> Option<IRTypeId> {
        for (i, ir_type) in self.types_table.iter().enumerate() {
            match &ir_type.type_enum {
                IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built } => {
                    if _parent_scope == *parent_scope && def.name == name && _templates_values == templates_values {
                        return Some(i);
                    }
                },
                _ => {}
            }
        }
        None
    }

    pub fn lower_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_values: IRTemplatesValues, call_span: Option<Span>) -> Result<IRTypeId, CompilerError> {
        if let Some(id) = self.find_type_in_scope(parent_scope, name, &templates_values) {
            return Ok(id);
        }
        let struct_def = if let Some(def) = self.find_struct_def_in_scope(parent_scope, name) {
            def
        } else {
            return Err(self.error("Missing struct", Some(format!("Called a non existing structure '{}'", name)), call_span));
        };
        let struct_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_values);

        let mut new_templates_values = self.ir_scope(parent_scope).templates_values.clone();
        new_templates_values.extend(templates_values.clone());

        let struct_id: usize = self.types_table.len();
        let struct_scope = self.scope_id(IRScope { path: IRScopePath::Type(struct_id), path_string: struct_path_string.clone(), templates_values: new_templates_values, ast_def: None });
        let ir_type_enum = IRTypeEnum::Struct { parent_scope, scope: struct_scope, templates_values, def: struct_def, args: Vec::new(), vars_built: false };
        self.types_table.push(IRType { type_enum: ir_type_enum, llvm_type: None });

        let mut members: IRVariables = Vec::new();
        let mut members_llvm_types: Vec<BasicTypeEnum> = Vec::new();
        if let Some(def_members) = struct_def.vars.as_ref() {
            for member in def_members.variables.iter() {
                let ir_var: IRVariable = self.get_ir_var(struct_scope, member)?;
                if let Some(llvm_type) = self.ir_type(ir_var.type_id).llvm_type {
                    members_llvm_types.push(llvm_type); 
                    members.push(ir_var);
                }
            }
        }
        
        let struct_llvm_type = self.llvm_context.struct_type(members_llvm_types.as_slice(), false).into();
        if let IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built } = &mut self.types_table[struct_id].type_enum {
            *args = members;
        }
        self.types_table[struct_id].llvm_type = Some(struct_llvm_type);

        Ok(struct_id)
    }
}