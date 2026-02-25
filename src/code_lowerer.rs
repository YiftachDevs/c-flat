use std::collections::HashMap;
use std::fmt::format;
use std::hash::Hash;
use std::path::Path;


use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, PointerType};
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::StructType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::VoidType;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

use crate::errors::{CompilerError, CompilerErrorType};
use crate::parser::{ExprNode, ExprNodeEnum, FileContext, Function, Literal, Scope, Span, Struct, Template, Templates, Variable};

pub type IRTypeId = usize;
pub type IRFunctionId = usize;
pub type IRScopeId = usize;

#[derive(PartialEq, Clone)]
pub struct IRVariable<'ctx> {
    pub name: String,
    pub type_id: IRTypeId,
    pub is_mut: bool,
    pub llvm_ptr: Option<PointerValue<'ctx>>
}

pub type IRVariables<'ctx> = Vec<IRVariable<'ctx>>;

#[derive(PartialEq)]
pub enum IRTypeEnum<'ctx> {
    Primitive(PrimitiveType),
    Pointer { ptr_type_id: IRTypeId, is_ref: bool },
    Array { arr_type: IRTypeId, size: usize },
    Callback { args: IRVariables<'ctx>, return_type: IRTypeId },
    Struct { parent_scope: IRScopeId, scope: IRScopeId, templates_values: IRTemplatesValues, args: IRVariables<'ctx>, def: &'ctx Struct, vars_built: bool }
}

#[derive(PartialEq, Clone)]
pub enum IRTemplateValue {
    Type(IRTypeId),
    Const(Literal)
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum IRTemplateKey {
    Type(String),
    Const(String, IRTypeId)
}

pub type IRTemplatesValues = HashMap<IRTemplateKey, IRTemplateValue>;

#[derive(Eq, PartialEq, Clone, Hash, Copy)]
pub enum PrimitiveType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    F16,
    F32,
    F64,
    Char,
    Bool,
    Void,
    Never
}

impl PrimitiveType {
    pub fn is_int(&self) -> bool {
        match self {
            PrimitiveType::I8 | PrimitiveType::I16 | PrimitiveType::I32 | PrimitiveType::I64 | PrimitiveType::I128 => true,
            _ => false
        }
    }
    
    pub fn is_uint(&self) -> bool {
        match self {
            PrimitiveType::U8 | PrimitiveType::U16 | PrimitiveType::U32 | PrimitiveType::U64 | PrimitiveType::U128 | PrimitiveType::Char => true,
            _ => false
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            PrimitiveType::F16 | PrimitiveType::F32 | PrimitiveType::F64 => true,
            _ => false
        }
    }
}

impl ToString for PrimitiveType {
    fn to_string(&self) -> String {
        match self {
            PrimitiveType::U8 => "u8".to_string(),
            PrimitiveType::I8 => "i8".to_string(),
            PrimitiveType::U16 => "u16".to_string(),
            PrimitiveType::I16 => "i16".to_string(),
            PrimitiveType::U32 => "u32".to_string(),
            PrimitiveType::I32 => "i32".to_string(),
            PrimitiveType::U64 => "u64".to_string(),
            PrimitiveType::I64 => "i64".to_string(),
            PrimitiveType::U128 => "u128".to_string(),
            PrimitiveType::I128 => "i128".to_string(),
            PrimitiveType::F16 => "f16".to_string(),
            PrimitiveType::F32 => "f32".to_string(),
            PrimitiveType::F64 => "f64".to_string(),
            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::Void => "()".to_string(),
            PrimitiveType::Never => "!".to_string()
        }
    }
}

#[derive(PartialEq)]
pub struct IRType<'ctx> {
    pub type_enum: IRTypeEnum<'ctx>,
    pub llvm_type: Option<BasicTypeEnum<'ctx>>
}

#[derive(PartialEq)]
pub struct IRFunction<'ctx> {
    pub parent_scope: IRScopeId,
    pub scope: IRScopeId,
    pub templates_values: IRTemplatesValues,
    pub args: IRVariables<'ctx>,
    pub return_type: IRTypeId,
    pub llvm_type: FunctionType<'ctx>,
    pub llvm_value: FunctionValue<'ctx>,
    pub ast_def: &'ctx Function
}


#[derive(PartialEq, Clone)]
pub enum IRScopePath {
    ModulePath(Vec<String>),
    Function(IRFunctionId), // add TypeImpl(...)
    Type(IRTypeId)
}

#[derive(PartialEq, Clone)]
pub struct IRScope<'ctx> {
    pub parent_scope: Option<IRScopeId>,
    pub path: IRScopePath,
    pub path_string: String,
    pub templates_values: IRTemplatesValues,
    pub ast_def: Option<&'ctx Scope>
}

pub struct IRFunContext<'ctx> {
    pub fun: IRFunctionId,
    pub vars: IRVariables<'ctx>
}

pub enum IRContext<'ctx> {
    FunContext(IRFunContext<'ctx>),
    ScopeContext(IRScopeId)
}

impl<'ctx> IRContext<'ctx> {
    pub fn into_fun_context(&mut self) -> &mut IRFunContext<'ctx> {
        match self {
            IRContext::FunContext(fun_context) => fun_context,
            _ => panic!("ensure_fun_context")
        }
    }
}

pub struct CodeLowerer<'ctx> {
    pub ast_scope: &'ctx Scope,
    pub file_context: FileContext,
    pub llvm_context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub types_table: Vec<IRType<'ctx>>,
    pub funs_table: Vec<IRFunction<'ctx>>,
    pub scopes_table: Vec<IRScope<'ctx>>,
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn new(ast_scope: &'ctx Scope, file_context: FileContext, llvm_context: &'ctx Context) -> Self {
        let module: Module = llvm_context.create_module("main_module");
        let builder: Builder<'_> = llvm_context.create_builder();
        CodeLowerer { ast_scope, file_context, llvm_context, module, builder, types_table: Vec::new(), funs_table: Vec::new(), scopes_table: Vec::new() }
    }

    pub fn ir_type(&self, type_id: IRTypeId) -> &IRType<'ctx> { &self.types_table[type_id] }
    pub fn ir_function(&self, fun_id: IRFunctionId) -> &IRFunction<'ctx> { &self.funs_table[fun_id] }
    pub fn ir_scope(&self, scope_id: IRScopeId) -> &IRScope<'ctx> { &self.scopes_table[scope_id] }

    pub fn scope_id(&mut self, ir_scope: IRScope<'ctx>) -> IRScopeId {
        if let Some((i, _)) = self.scopes_table.iter().enumerate().find(|(_, cur_ir)| **cur_ir == ir_scope) {
            return i;
        }
        let id = self.scopes_table.len();
        self.scopes_table.push(ir_scope);
        id
    }

    pub fn fun_id(&mut self, ir_fun: IRFunction<'ctx>) -> IRFunctionId {
        if let Some((i, _)) = self.funs_table.iter().enumerate().find(|(_, cur_ir)| **cur_ir == ir_fun) {
            return i;
        }
        let id = self.funs_table.len();
        self.funs_table.push(ir_fun);
        id
    }

    pub fn type_id(&mut self, ir_type: IRType<'ctx>) -> IRTypeId {
        if let Some((i, _)) = self.types_table.iter().enumerate().find(|(_, cur_ir)| **cur_ir == ir_type) {
            return i;
        }
        let id = self.types_table.len();
        self.types_table.push(ir_type);
        id
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
        Ok(self.type_id(IRType { type_enum: ir_type_enum, llvm_type }))
    }

    pub fn is_type_zero_sized(&mut self, type_id: IRTypeId) -> Result<bool, CompilerError> {
        let void_type = self.primitive_type(PrimitiveType::Void)?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        Ok(type_id == void_type || type_id == never_type)
    }

    pub fn get_templates_keys_from(&mut self, ir_context: &mut IRContext<'ctx>, ast_templates: &Templates) -> Result<Vec<IRTemplateKey>, CompilerError> {
        let mut result: Vec<IRTemplateKey> = Vec::new();
        for ast_template in ast_templates.templates.iter() {
            let template = match ast_template {
                Template::VarType(name) => IRTemplateKey::Type(name.clone())
            };
            result.push(template);
        }
        Ok(result)
    }

    pub fn get_context_parent_scope(&mut self, ir_context: &IRContext<'ctx>) -> IRScopeId {
        match ir_context {
            IRContext::FunContext(fun) => self.ir_function(fun.fun).parent_scope,
            IRContext::ScopeContext(scope) => *scope
        }
    }

    pub fn get_module_scope_in_scope(&mut self, parent_scope: IRScopeId, name: &str) -> Option<IRScopeId> {
        let ir_parent_scope: &IRScope<'_> = self.ir_scope(parent_scope);
        if let Some(ast_def) = ir_parent_scope.ast_def {
            for module in ast_def.modules.iter() {
                if module.name == name {
                    let mut new_parent_path = if let IRScopePath::ModulePath(path) = ir_parent_scope.path.clone() { path } else { panic!("CodeLowerer::get_module_scope_in_scope") };
                    new_parent_path.push(name.to_string());
                    let path_string = self.format_child_scope_path(parent_scope, name, &HashMap::new());
                    let ir_scope = IRScope { parent_scope: Some(parent_scope), path: IRScopePath::ModulePath(new_parent_path), templates_values: HashMap::new(), ast_def: Some(&module.scope), path_string };
                    return Some(self.scope_id(ir_scope));
                }
            }
        }
        if let Some(grand_parent_scope) = ir_parent_scope.parent_scope {
            self.get_module_scope_in_scope(grand_parent_scope, name)
        } else {
            None
        }
    }
    
    pub fn get_global_scope(&mut self) -> IRScopeId {
        self.scope_id(IRScope{ parent_scope: None, path: IRScopePath::ModulePath(Vec::new()), path_string: "".to_string(), templates_values: HashMap::new(), ast_def: Some(&self.ast_scope)})
    }

    pub fn format_scope_path(&self, scope: IRScopeId) -> String {
        let ir_scope: &IRScope<'_> = self.ir_scope(scope);
        match &ir_scope.path {
            IRScopePath::ModulePath(path) => {
                path.join(".")
            },
            IRScopePath::Function(fun) => {
                let ir_fun = self.ir_function(*fun);
                self.format_child_scope_path(scope, &ir_fun.ast_def.name, &self.ir_scope(ir_fun.scope).templates_values)
            },
            IRScopePath::Type(type_id) => {
                let ir_type = self.ir_type(*type_id);
                match &ir_type.type_enum {
                    IRTypeEnum::Struct { parent_scope, scope, templates_values, args, def, vars_built } => {
                        self.format_child_scope_path(*parent_scope, &def.name, &self.ir_scope(*scope).templates_values)
                    },
                    _ => todo!("format_scope_path")
                }
            }
        }
    }

    pub fn format_child_scope_path(&self, parent_scope: IRScopeId, name: &str, templates_values: &IRTemplatesValues) -> String {
        let mut parent_str = self.format_scope_path(parent_scope);
        if !parent_str.is_empty() { parent_str.push('.');}
        format!("{}{}{}", parent_str, name, self.format_templates_values(templates_values))
    }

    pub fn format_templates_values(&self, templates_values: &IRTemplatesValues) -> String {
        if templates_values.is_empty() {
            "".to_string()
        } else {
            let templates_values_str = templates_values.values().map(|value| match value {
                IRTemplateValue::Type(_type) => self.format_type(*_type),
                IRTemplateValue::Const(const_value) => const_value.to_string()
            }).collect::<Vec<String>>().join(", ");
            format!("<{}>", templates_values_str)
        }
    }

    pub fn format_type(&self, _type: IRTypeId) -> String {
        let ir_type = self.ir_type(_type);

        match ir_type.type_enum {
            IRTypeEnum::Primitive(primitive) => {
                primitive.to_string()
            },
            IRTypeEnum::Pointer { ptr_type_id, is_ref } => {
                let ptr_type_string: String = self.format_type(ptr_type_id);
                let ptr_char = if is_ref { "&" } else { "*" };
                format!("{}{}", ptr_char, ptr_type_string)
            }
            _ => todo!()
        }
    }

    pub fn error(&self, msg: &str, description: Option<String>, opt_span: Option<Span>) -> CompilerError {
        if let Some(span) =  opt_span {
            let file: String = self.file_context.get_path(span.file_id);
            let chars: &Vec<char> = &self.file_context.files[&span.file_id];
            let line_end_idx: usize = chars[span.line_index..].iter().position(|&c| c == '\n').map(|pos| span.line_index + pos).unwrap_or(chars.len());
            let line_str: String = chars[span.line_index..line_end_idx].iter().collect();
            CompilerError { err_type: CompilerErrorType::SemanticError, msg: msg.to_string(), description, file, span: Some(span), line_str }
        } else {
            CompilerError { err_type: CompilerErrorType::SemanticError, msg: msg.to_string(), description, file: "".to_string(), span: None, line_str: "".to_string() }
        }
    }

    pub fn export_ir_to_file(&self, file_path: &Path) {
        self.module.print_to_file(file_path).expect("Failed to write IR");
        self.module.print_to_stderr();
    }
}