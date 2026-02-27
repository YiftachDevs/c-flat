use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRScope, IRScopeId, IRScopePath, IRTemplatesMap, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::CompilerError, parser::{Span, Struct}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_struct_def_in_scope(&self, parent_scope: IRScopeId, name: &str) -> Option<(IRScopeId, &'ctx Struct)> {
        let ir_scope = self.ir_scope(parent_scope);
        if let Some(ast_def) = ir_scope.ast_def {
            for strct in ast_def.structs.iter() {
                if strct.name == name {
                    return Some((parent_scope, strct));
                }
            }
        }
        if let Some(grand_parent_scope) = ir_scope.parent_scope {
            self.find_struct_def_in_scope(grand_parent_scope, name)
        } else {
            None
        }
    }

    pub fn primitive_type_from(&mut self, name: &str) -> Option<PrimitiveType> {
        match name {
            "!" => Some(PrimitiveType::Never),
            "()" => Some(PrimitiveType::Void),
            "bool" => Some(PrimitiveType::Bool),
            "char" => Some(PrimitiveType::Char),
            "u8" => Some(PrimitiveType::U8),
            "i8" => Some(PrimitiveType::I8),
            "u16" => Some(PrimitiveType::U16),
            "i16" => Some(PrimitiveType::I16),
            "f16" => Some(PrimitiveType::F16),
            "u32" => Some(PrimitiveType::U32),
            "i32" => Some(PrimitiveType::I32),
            "f32" => Some(PrimitiveType::F32),
            "u64" => Some(PrimitiveType::U64),
            "i64" => Some(PrimitiveType::I64),
            "f64" => Some(PrimitiveType::F64),
            "u128" => Some(PrimitiveType::U128),
            "i128" => Some(PrimitiveType::I128),
            _ => None
        }
    }

    fn find_type_in_scope(&mut self, _parent_scope: IRScopeId, name: &str, _templates_values: &IRTemplatesMap) -> Option<IRTypeId> {
        for (i, ir_type) in self.types_table.iter().enumerate() {
            match &ir_type.type_enum {
                IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built, templates_keys } => {
                    if _parent_scope == *parent_scope && def.name == name && _templates_values == templates_values {
                        return Some(i);
                    }
                },
                _ => {}
            }
        }
        None
    }

    pub fn lower_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_values: IRTemplatesMap, call_span: Option<Span>) -> Result<IRTypeId, CompilerError> {
        if let Some(id) = self.find_type_in_scope(parent_scope, name, &templates_values) {
            return Ok(id);
        }
        let (_, struct_def) = if let Some(def) = self.find_struct_def_in_scope(parent_scope, name) {
            def
        } else {
            return Err(self.error("Missing struct", Some(format!("Called a non existing structure '{}'", name)), call_span));
        };
        let templates_keys = self.get_templates_keys_from(&struct_def.templates)?;
        let struct_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_values, &templates_keys);

        let mut new_templates_values = self.ir_scope(parent_scope).templates_values.clone();
        new_templates_values.extend(templates_values.clone());

        let struct_id: usize = self.types_table.len();
        let struct_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Type(struct_id), path_string: struct_path_string.clone(), templates_values: new_templates_values, ast_def: None });
        let ir_type_enum = IRTypeEnum::Struct { parent_scope, scope: struct_scope, templates_values, def: struct_def, args: Vec::new(), vars_built: false, templates_keys };
        self.types_table.push(IRType { type_enum: ir_type_enum, llvm_type: None, lowered_impls: Vec::new() });

        let mut ir_context = IRContext::ScopeContext(struct_scope);
        let mut members: IRVariables = Vec::new();
        let mut members_llvm_types: Vec<BasicTypeEnum> = Vec::new();
        if let Some(def_members) = struct_def.vars.as_ref() {
            for member in def_members.variables.iter() {
                let ir_var: IRVariable = self.get_ir_var(&mut ir_context, member)?;
                if let Some(llvm_type) = self.ir_type(ir_var.type_id).llvm_type {
                    members_llvm_types.push(llvm_type); 
                    members.push(ir_var);
                }
            }
        }
        // members_llvm_types.as_slice(), false
        let struct_llvm_type = self.llvm_context.opaque_struct_type(struct_path_string.as_str());
        struct_llvm_type.set_body(members_llvm_types.as_slice(), false);
        if let IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built, templates_keys } = &mut self.types_table[struct_id].type_enum {
            *args = members;
        }
        self.types_table[struct_id].llvm_type = Some(struct_llvm_type.into());

        Ok(struct_id)
    }
}