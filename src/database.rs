use std::collections::HashMap;

use inkwell::llvm_context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, PointerType};
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::StructType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::VoidType;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

use crate::parser::{ConstValue, FileContext, Function, PrimitiveType, Scope, Struct, TemplatesValues, VarType};

type IRTypeId = usize;
type IRFunctionId = usize;
type IRVariables<'ctx> = Vec<IRVariable<'ctx>>;

enum IRTypeEnum<'ctx> {
    Primitive(PrimitiveType),
    Pointer { ptr_type_id: IRTypeId, is_ref: bool },
    Array { arr_type: IRTypeId, size: usize },
    Callback { args: IRVariables<'ctx>, return_type: IRTypeId },
    Struct { scope_context: IRScopeContext, args: IRVariables<'ctx>, def: &'ctx Struct }
}

#[derive(PartialEq, Clone)]
enum IRTemplateValue {
    Type(IRTypeId),
    Const(ConstValue)
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum IRTemplateKey {
    Type(String),
    Const(String, IRTypeId)
}

type IRTemplatesValues = HashMap<IRTemplateKey, IRTemplateValue>;

#[derive(PartialEq, Clone)]
pub enum IRScopePath {
    ModulePath(Vec<String>),
    Function(IRFunctionId) // add TypeImpl(...)
}

#[derive(PartialEq, Clone)]
pub struct IRScopeContext {
    path: IRScopePath,
    templates_values: IRTemplatesValues
}

struct IRVariable<'ctx> {
    name: String,
    type_id: IRTypeId,
    is_mut: bool,
    llvm_ptr: Option<PointerValue<'ctx>>
}

struct IRType<'ctx> {
    type_enum: IRTypeEnum<'ctx>,
    llvm_type: Option<AnyTypeEnum<'ctx>>
}

struct IRFunction<'ctx> {
    scope_context: IRScopeContext,
    args: IRVariables<'ctx>,
    return_type_id: IRTypeId,
    llvm_type: FunctionType<'ctx>,
    llvm_value: FunctionValue<'ctx>,
    fun_def: &'ctx Function
}

pub struct Database<'ctx> {
    ast_scope: Scope,
    file_context: FileContext,
    llvm_context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    types_table: Vec<IRType<'ctx>>,
    funs_table: Vec<IRFunction<'ctx>>,
}

impl<'ctx> Database<'ctx> {
    fn get_type(&mut self, scope_context: &IRScopeContext, var_type: &VarType) -> IRTypeId {
        if let Some(existing_id) = self._find_type(scope_context, var_type) {
            return existing_id;
        }
        let new_id = self.types_table.len();
        match &var_type {
            VarType::Primitive(primitive_type) => {
                let llvm_type = match primitive_type {
                    PrimitiveType::I8 | PrimitiveType::U8 => Some(self.llvm_context.i8_type().into()),
                    PrimitiveType::I16 | PrimitiveType::U16 => Some(self.llvm_context.i16_type().into()),
                    PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::Char => Some(self.llvm_context.i32_type().into()),
                    PrimitiveType::I64 | PrimitiveType::U64 => Some(self.llvm_context.i64_type().into()),
                    PrimitiveType::I128 | PrimitiveType::U128 => Some(self.llvm_context.i128_type().into()),
                    PrimitiveType::F16 => Some(self.llvm_context.f16_type().into()),
                    PrimitiveType::F32 => Some(self.llvm_context.f32_type().into()),
                    PrimitiveType::F64 => Some(self.llvm_context.f64_type().into()),
                    PrimitiveType::Bool => Some(self.llvm_context.bool_type().into()),
                    PrimitiveType::Void | PrimitiveType::Never => None
                };
                let ir_type = IRType { type_enum: IRTypeEnum::Primitive(*primitive_type), llvm_type };
                self.types_table.push(ir_type);
            },
            VarType::Pointer { ptr_type, is_ref } => {
                let ptr_type_id: IRTypeId = self.get_type(scope_context, ptr_type);
                let ir_type: IRType<'_> = IRType { type_enum: IRTypeEnum::Pointer { ptr_type_id, is_ref: *is_ref }, llvm_type: Some(self.llvm_context.ptr_type(inkwell::AddressSpace::from(0)).into()) };
                self.types_table.push(ir_type);
            },
            _ => todo!("impl Database::get_type")
        }
        new_id
    }
    
    fn _find_type(&self, scope_context: &IRScopeContext, var_type: &VarType) -> Option<IRTypeId> {
        for (i, ir_type) in self.types_table.iter().enumerate() {
            match &ir_type.type_enum {
                IRTypeEnum::Primitive(other_prim_type) => {
                    if let VarType::Primitive(prim_type) = var_type && other_prim_type == prim_type {
                        return Some(i);
                    }
                },
                IRTypeEnum::Pointer { ptr_type_id, is_ref } => {
                    let is_cur_ref = is_ref;
                    if let VarType::Pointer { ptr_type, is_ref } = var_type {
                        if is_cur_ref == is_ref && let Some(existing_ptr_type) = self._find_type(scope_context, ptr_type) && existing_ptr_type == *ptr_type_id {
                            return Some(i);
                        }
                    }
                }
                _ => todo!("impl Database::_find_type")
            }
        }
        None
    }
}