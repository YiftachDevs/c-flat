use std::any::Any;

use inkwell::{AddressSpace, types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum}, values::{BasicValue, BasicValueEnum, PointerValue}};

use crate::{code_lowerer::{CodeLowerer, IRContext, IRExprContext, IRScope, IRScopeId, IRScopePath, IRStruct, IRTemplateValue, IRTemplatesMap, IRType, IRTypeEnum, IRTypeId, IRVarDeclaration, IRVarDeclarations, IRVariable, IRVariables, PrimitiveType}, errors::{CompilerError, SemanticError}, expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult}, parser::{ExprNode, Span, Struct, Templates}};

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
        let llvm_type: BasicTypeEnum = match prim {
            PrimitiveType::I8 | PrimitiveType::U8 => self.llvm_context.i8_type().into(),
            PrimitiveType::I16 | PrimitiveType::U16 => self.llvm_context.i16_type().into(),
            PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::Char => self.llvm_context.i32_type().into(),
            PrimitiveType::I64 | PrimitiveType::U64 => self.llvm_context.i64_type().into(),
            PrimitiveType::I128 | PrimitiveType::U128 => self.llvm_context.i128_type().into(),
            PrimitiveType::F16 => self.llvm_context.f16_type().into(),
            PrimitiveType::F32 => self.llvm_context.f32_type().into(),
            PrimitiveType::F64 => self.llvm_context.f64_type().into(),
            PrimitiveType::Bool => self.llvm_context.bool_type().into(),
            PrimitiveType::Void | PrimitiveType::Never => self.llvm_context.struct_type(&[], false).into(),
        };
        let type_id = self.type_id(IRType { type_enum: ir_type_enum, llvm_type, lowered_impls: None });
        self.lower_impls(type_id)?;
        
        Ok(type_id)
    }

    pub fn is_type_zero_sized(&self, type_id: IRTypeId) -> bool {
        let ir_type_enum = &self.ir_type(type_id).type_enum;
        if let IRTypeEnum::Primitive(prim) = ir_type_enum {
            return *prim == PrimitiveType::Void || *prim == PrimitiveType::Never;
        }
        if let IRTypeEnum::Struct(ir_struct) = &self.ir_type(type_id).type_enum {
            for arg in ir_struct.args.iter() {
                if !self.is_type_zero_sized(arg.type_id) { 
                    return false;
                }
            }
        }
        return false;
    }

    pub fn get_type_mem_size(&self, type_id: IRTypeId) -> u64 {
        if self.is_type_zero_sized(type_id) {
            return  0;
        }
        match &self.ir_type(type_id).type_enum {
            IRTypeEnum::Primitive(prim) => prim.get_mem_size(),
            IRTypeEnum::Array { arr_type, size } => self.get_type_mem_size(*arr_type) as u64 * size,
            IRTypeEnum::Reference { ptr_type_id, is_mut } => 8,
            IRTypeEnum::Struct(ir_struct) => ir_struct.args.iter().map(|arg| self.get_type_mem_size(arg.type_id)).sum(),
            IRTypeEnum::UnsizedRef { unsized_type, is_mut } => 16,
            _ => panic!()
        }
    }

    pub fn is_type_unsized(&self, type_id: IRTypeId) -> Result<bool, CompilerError> {
        let ir_type_enum = &self.ir_type(type_id).type_enum;
        if let IRTypeEnum::Slice { slice_type } = ir_type_enum {
            return Ok(true);
        }
        return Ok(false);
    }

    pub fn get_type_zero(&mut self, type_id: IRTypeId) -> IRExprValueResult<'ctx> {
        IRExprValueResult { type_id: type_id, llvm_value: self.ir_type(type_id).llvm_type.const_zero() }
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

    pub fn unsized_ref_type(&mut self, unsized_type: IRTypeId, is_mut: bool) -> Result<IRTypeId, CompilerError> {
        let ptr_type = self.llvm_context.ptr_type(AddressSpace::default());
        let usize_type = self.llvm_context.i64_type();
        let llvm_type = self.llvm_context.struct_type(&[ptr_type.into(), usize_type.into()], false).into();
        let type_id = self.type_id(IRType { type_enum: IRTypeEnum::UnsizedRef { unsized_type, is_mut }, llvm_type, lowered_impls: None });
        self.lower_impls(type_id)?;
        Ok(type_id)
    }

    pub fn slice_type(&mut self, slice_type: IRTypeId) -> Result<IRTypeId, CompilerError> {
        let llvm_type = self.ir_type(slice_type).llvm_type.array_type(0).into();
        let type_id = self.type_id(IRType { type_enum: IRTypeEnum::Slice { slice_type }, llvm_type, lowered_impls: None });
        self.lower_impls(type_id)?;
        Ok(type_id)
    }

    pub fn unsized_ref_value(&mut self, unsized_ref_type: IRTypeId, ptr_value: PointerValue<'ctx>, len: u64) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        let mut llvm_value = self.ir_type(unsized_ref_type).llvm_type.into_struct_type().get_undef().into();
        llvm_value = self.builder.build_insert_value(llvm_value, ptr_value, 0, "ptr").unwrap();
        llvm_value = self.builder.build_insert_value(llvm_value, self.llvm_context.i64_type().const_int(len, false), 1, "len").unwrap();
        Ok(llvm_value.as_basic_value_enum())
    }

    pub fn reference_type(&mut self, type_id: IRTypeId, is_mut: bool) -> Result<IRTypeId, CompilerError> {
        let ptr_type = self.type_id(IRType { type_enum: IRTypeEnum::Reference { ptr_type_id: type_id, is_mut }, llvm_type: self.llvm_context.ptr_type(AddressSpace::default()).into(), lowered_impls: None });
        self.lower_impls(ptr_type)?;
        Ok(ptr_type)
    }

    pub fn array_type(&mut self, arr_type: IRTypeId, size: u64) -> Result<IRTypeId, CompilerError> {
        let llvm_type = self.ir_type(arr_type).llvm_type.array_type(size as u32).into();
        let type_id = self.type_id(IRType { type_enum: IRTypeEnum::Array { arr_type, size }, llvm_type, lowered_impls: None });
        self.lower_impls(type_id)?;
        Ok(type_id)
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

    pub fn lower_struct(&mut self, parent_scope: IRScopeId, name: &str, templates_values: &[IRTemplateValue<'ctx>], call_span: Option<Span>) -> Result<IRTypeId, CompilerError> {
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

        let struct_llvm_type = self.llvm_context.opaque_struct_type(struct_path_string.as_str());
        self.types_table.push(IRType { type_enum: ir_type_enum, llvm_type: struct_llvm_type.into(), lowered_impls: None });

        self.ensure_templates_constraints(&mut ir_context, &templates_map, &struct_def.templates, call_span)?;

        let mut args: IRVarDeclarations = Vec::new();
        for arg in struct_def.vars.variables.iter() {
            let ir_var_dec: IRVarDeclaration = self.get_ir_var_declaration(&mut ir_context, arg)?;
            args.push(ir_var_dec);
        }
        let args_llvm_types: Vec<BasicTypeEnum> = args.iter().map(|arg| self.ir_type(arg.type_id).llvm_type).collect();
        
        struct_llvm_type.set_body(args_llvm_types.as_slice(), false);
        if let IRTypeEnum::Struct(_struct) = &mut self.types_table[struct_id].type_enum {
            _struct.args = args;
        }
        self.types_table[struct_id].llvm_type = struct_llvm_type.into();
        self.lower_impls(struct_id)?;

        Ok(struct_id)
    }

    pub fn ensure_type_matches(&mut self, type_id: IRTypeId, expected_type: Option<IRTypeId>, span: Option<Span>, coerse: bool) -> Result<IRTypeId, CompilerError> {
        let expected_type = match expected_type { Some(v) => v, None => return Ok(type_id) };
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        if type_id == expected_type || type_id == never_type {
            Ok(type_id)
        } else if coerse && let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(expected_type).type_enum && let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(type_id).type_enum && is_mut {
            Ok(expected_type)
        } else if coerse && let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = self.ir_type(expected_type).type_enum && let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = self.ir_type(type_id).type_enum && is_mut {
            Ok(expected_type)
        }
        else {
            Err(self.error(SemanticError::TypeMismatch { expected: self.format_type(expected_type), got: self.format_type(type_id) }, span))
        }
    }

    pub fn auto_reference(&mut self, ir_context: &mut IRContext<'ctx>, expr_result: IRExprResult<'ctx>, expected_type: IRTypeId, span: Span) -> Result<IRExprResult<'ctx>, CompilerError> {
        let type_id = expr_result.get_type_id();
        if type_id == expected_type {
            return Ok(expr_result);
        }
        if let IRTypeEnum::Reference { ptr_type_id, is_mut } = self.ir_type(expected_type).type_enum {
            let auto_ref_result = self.auto_reference(ir_context, expr_result, ptr_type_id, span)?;
            let referenced_value = self.address(ir_context, auto_ref_result, is_mut, span)?;
            return Ok(IRExprResult::Value(referenced_value));
        } else if let IRTypeEnum::UnsizedRef { unsized_type, is_mut } = self.ir_type(expected_type).type_enum {
            let auto_ref_result = self.auto_reference(ir_context, expr_result, unsized_type, span)?;
            let referenced_value = self.address(ir_context, auto_ref_result, is_mut, span)?;
            return Ok(IRExprResult::Value(referenced_value));
        } else {
            return Err(self.error(SemanticError::TypeMismatch { expected: self.format_type(expected_type), got: self.format_type(type_id) }, Some(span)));
        }
    }
}