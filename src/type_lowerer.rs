use std::any::Any;

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRContextType, IRScope, IRScopeId, IRScopePath, IRTemplatesMap, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, parser::{Span, Struct}};

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

    pub fn primitive_type(&mut self, prim: PrimitiveType) -> Result<IRTypeId, CompilerError> {
        let ir_type_enum = IRTypeEnum::Primitive(prim);
        let llvm_type: Option<BasicTypeEnum> = match prim {
            PrimitiveType::I8 | PrimitiveType::U8 => Some(self.llvm_context.i8_type().into()),
            PrimitiveType::I16 | PrimitiveType::U16 => Some(self.llvm_context.i16_type().into()),
            PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::Char => Some(self.llvm_context.i32_type().into()),
            PrimitiveType::I64 | PrimitiveType::U64 => Some(self.llvm_context.i64_type().into()),
            PrimitiveType::I128 | PrimitiveType::U128 => Some(self.llvm_context.i128_type().into()),
            PrimitiveType::F16 => Some(self.llvm_context.f16_type().into()),
            PrimitiveType::F32 => Some(self.llvm_context.f32_type().into()),
            PrimitiveType::F64 => Some(self.llvm_context.f64_type().into()),
            PrimitiveType::Bool => Some(self.llvm_context.bool_type().into()),
            PrimitiveType::Void => None,
            PrimitiveType::Never => None
        };
        let type_id = self.type_id(IRType { type_enum: ir_type_enum, llvm_type, lowered_impls: None });
        if let None = self.types_table[type_id].lowered_impls {
            let global_scope = self.get_global_scope();
            self.find_impls(global_scope, type_id)?;
        }
        Ok(type_id)
    }

    pub fn is_type_zero_sized(&mut self, type_id: IRTypeId) -> Result<bool, CompilerError> {
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        Ok(type_id == void_type || type_id == never_type)
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

    fn find_type_in_scope(&mut self, _parent_scope: IRScopeId, name: &str, _templates_map: &IRTemplatesMap) -> Option<IRTypeId> {
        for (i, ir_type) in self.types_table.iter().enumerate() {
            match &ir_type.type_enum {
                IRTypeEnum::Struct { parent_scope, scope, templates_map, args, def, vars_built } => {
                    if _parent_scope == *parent_scope && def.name == name && _templates_map == templates_map {
                        return Some(i);
                    }
                },
                _ => {}
            }
        }
        None
    }

    pub fn lower_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_map: IRTemplatesMap, call_span: Option<Span>) -> Result<IRTypeId, CompilerError> {
        if let Some(id) = self.find_type_in_scope(parent_scope, name, &templates_map) {
            return Ok(id);
        }
        let (_, struct_def) = if let Some(def) = self.find_struct_def_in_scope(parent_scope, name) {
            def
        } else {
            panic!("lower_struct: Called a non existing structure '{}'", name);
        };
        let struct_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_map);

        let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
        new_templates_map.extend(templates_map.clone());

        let struct_id: usize = self.types_table.len();
        let struct_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Type(struct_id), path_string: struct_path_string.clone(), templates_map: new_templates_map, ast_def: None });
        let ir_type_enum = IRTypeEnum::Struct { parent_scope, scope: struct_scope, templates_map, def: struct_def, args: Vec::new(), vars_built: false };
        self.types_table.push(IRType { type_enum: ir_type_enum, llvm_type: None, lowered_impls: None });

        let mut ir_context = IRContext::ScopeContext(struct_scope);
        let mut members: IRVariables = Vec::new();
        let mut members_llvm_types: Vec<BasicTypeEnum> = Vec::new();
        for member in struct_def.vars.variables.iter() {
            let ir_var: IRVariable = self.get_ir_var(&mut ir_context, member)?;
            if let Some(llvm_type) = self.ir_type(ir_var.type_id).llvm_type {
                members_llvm_types.push(llvm_type); 
                members.push(ir_var);
            }
        }
        // members_llvm_types.as_slice(), false
        let struct_llvm_type = self.llvm_context.opaque_struct_type(struct_path_string.as_str());
        struct_llvm_type.set_body(members_llvm_types.as_slice(), false);
        if let IRTypeEnum::Struct { parent_scope, scope, templates_map, args, def, vars_built } = &mut self.types_table[struct_id].type_enum {
            *args = members;
        }
        self.types_table[struct_id].llvm_type = Some(struct_llvm_type.into());

        let global_scope = self.get_global_scope();
        self.find_impls(global_scope, struct_id)?;

        Ok(struct_id)
    }

    pub fn ensure_type_matches(&mut self, type_id: IRTypeId, context_type: &IRContextType, span: Span) -> Result<(), CompilerError> {
        match context_type {
            IRContextType::Any => Ok(()),
            IRContextType::Impl(_, other_context_type) => self.ensure_type_matches(type_id, other_context_type, span),
            IRContextType::Type(ctx_type_id) => if type_id == *ctx_type_id { Ok(()) } else {
                Err(self.error(SemanticError::TypeMismatch { expected: self.format_type(*ctx_type_id), got: self.format_type(type_id) }, Some(span)))
            }
        }
    }
}