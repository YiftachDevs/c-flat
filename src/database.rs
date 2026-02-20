use std::collections::HashMap;

use inkwell::context::Context;
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

use crate::parser::{ConstValue, PrimitiveType, Scope, Struct, VarType};

type IRTypeId = usize;
type IRVariables<'ctx> = Vec<IRVariable<'ctx>>;

enum IRType<'ctx> {
    Primitive(PrimitiveType),
    Pointer { ptr_type_id: IRTypeId, is_ref: bool },
    Array { arr_type: IRTypeId, size: usize },
    Callback { args: IRVariables<'ctx>, return_type: IRTypeId },
    Struct { args: IRVariables<'ctx>, def: &'ctx Struct }
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
struct IRTypeName {
    var_type: VarType,
    templates_values: IRTemplatesValues
}

struct IRVariable<'ctx> {
    name: String,
    type_id: Option<IRTypeId>,
    is_mut: bool,
    ptr: Option<PointerValue<'ctx>>
}

struct Database<'ctx> {
    ast_scope: Scope,
    types_names:
    types: HashMap<IRTypeId, IRType<'ctx>>,
    llvm_types: HashMap<IRTypeId, AnyTypeEnum<'ctx>>
}

impl<'ctx> Database<'ctx> {

}