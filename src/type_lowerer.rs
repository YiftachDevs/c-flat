use std::any::Any;

use inkwell::{AddressSpace, types::{BasicMetadataTypeEnum, BasicTypeEnum}};

use crate::{code_lowerer::{CodeLowerer, IRConstraint, IRConstraints, IRContext, IRContextType, IRScope, IRScopeId, IRScopePath, IRStruct, IRTemplateValue, IRTemplatesMap, IRType, IRTypeEnum, IRTypeId, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, parser::{ExprNode, Span, Struct, Templates}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_struct_def_in_scope(&self, parent_scope: IRScopeId, name: &str, opt_call_span: Option<Span>) -> Result<Option<(IRScopeId, &'ctx Struct)>, CompilerError> {
        if let Some(actual_scope) = self.get_name_parent_scope(parent_scope, name, opt_call_span)? {
            let ir_scope = self.ir_scope(actual_scope);
            if let Some(def) = ir_scope.ast_def.unwrap().structs.iter().find(|def| def.name == name) {
                return Ok(Some((actual_scope, def)));
            }
        }
        Ok(None)
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
        self.lower_impls(type_id)?;
        
        Ok(type_id)
    }

    pub fn is_type_zero_sized(&self, type_id: IRTypeId) -> Result<bool, CompilerError> {
        let ir_type_enum = &&&self.ir_type(type_id).type_enum;
        if let IRTypeEnum::Primitive(prim) = ir_type_enum {
            return Ok(*prim == PrimitiveType::Void || *prim == PrimitiveType::Never);
        }
        if let IRTypeEnum::Struct(ir_struct) = &self.ir_type(type_id).type_enum {
            for arg in ir_struct.args.iter() {
                if !self.is_type_zero_sized(arg.type_id)? { 
                    return Ok(false);
                }
            }
        }
        return Ok(false);
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

    pub fn pointer_type(&mut self, type_id: IRTypeId) -> Result<IRTypeId, CompilerError> {
        let ptr_type = self.type_id(IRType { type_enum: IRTypeEnum::Pointer { ptr_type_id: type_id }, llvm_type: Some(self.llvm_context.ptr_type(AddressSpace::default()).into()), lowered_impls: None });
        self.lower_impls(ptr_type)?;
        Ok(ptr_type)
    }

    pub fn reference_type(&mut self, type_id: IRTypeId) -> Result<IRTypeId, CompilerError> {
        let ptr_type = self.type_id(IRType { type_enum: IRTypeEnum::Reference { ptr_type_id: type_id }, llvm_type: Some(self.llvm_context.ptr_type(AddressSpace::default()).into()), lowered_impls: None });
        self.lower_impls(ptr_type)?;
        Ok(ptr_type)
    }

    fn find_existing_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_map: &IRTemplatesMap) -> Option<IRTypeId> {
        for (i, ir_type) in self.types_table.iter().enumerate() {
            match &ir_type.type_enum {
                IRTypeEnum::Struct(_struct) => {
                    if _struct.parent_scope == parent_scope && _struct.def.name == name && _struct.templates_map == *templates_map {
                        return Some(i);
                    }
                },
                _ => {}
            }
        }
        None
    }

    pub fn lower_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_values: &[IRTemplateValue], call_span: Option<Span>) -> Result<IRTypeId, CompilerError> {
        let struct_def = self.find_struct_def_in_scope(parent_scope, name, call_span)?.unwrap().1;
        let templates_keys = self.get_templates_keys_from(&struct_def.templates)?;
        let templates_map = self.merge_templates_keys_values(&templates_keys, templates_values, call_span)?;
        if let Some(id) = self.find_existing_struct(parent_scope, name, &templates_map) {
            return Ok(id);
        }
        let struct_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_map);

        let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
        new_templates_map.extend(templates_map.clone());

        let struct_id: usize = self.types_table.len();
        let struct_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Type(struct_id), templates_map: new_templates_map, ast_def: None });
        let ir_type_enum = IRTypeEnum::Struct(IRStruct { parent_scope, scope: struct_scope, templates_map: templates_map.clone(), def: struct_def, args: Vec::new() });
        let mut ir_context = IRContext::ScopeContext(struct_scope);
        self.types_table.push(IRType { type_enum: ir_type_enum, llvm_type: None, lowered_impls: None });

        self.ensure_templates_constraints(&mut ir_context, &templates_map, &struct_def.templates, call_span)?;

        let mut members: IRVariables = Vec::new();
        let mut members_llvm_types: Vec<BasicTypeEnum> = Vec::new();
        for member in struct_def.vars.variables.iter() {
            let ir_var: IRVariable = self.get_ir_var(&mut ir_context, member)?;
            if let Some(llvm_type) = self.ir_type(ir_var.type_id).llvm_type {
                members_llvm_types.push(llvm_type); 
            }
            members.push(ir_var);
        }
        let struct_llvm_type = self.llvm_context.opaque_struct_type(struct_path_string.as_str());
        struct_llvm_type.set_body(members_llvm_types.as_slice(), false);
        if let IRTypeEnum::Struct(_struct) = &mut self.types_table[struct_id].type_enum {
            _struct.args = members;
        }
        self.types_table[struct_id].llvm_type = Some(struct_llvm_type.into());
        
        Ok(struct_id)
    }

    pub fn ensure_type_matches(&mut self, type_id: IRTypeId, context_type: &IRContextType, span: Option<Span>) -> Result<(), CompilerError> {
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        match context_type {
            IRContextType::Any => Ok(()),
            IRContextType::Type(ctx_type_id) => if type_id == never_type || type_id == *ctx_type_id { Ok(()) } else {
                Err(self.error(SemanticError::TypeMismatch { expected: self.format_type(*ctx_type_id), got: self.format_type(type_id) }, span))
            },
            IRContextType::Impl(_) => Ok(())
        }
    }

    pub fn type_matches(&mut self, type_id: IRTypeId, context_type: &IRContextType) -> bool {
        match context_type {
            IRContextType::Any => true,
            IRContextType::Type(ctx_type_id) => type_id == *ctx_type_id,
            IRContextType::Impl(_) => true
        }
    }
}