use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::hash::Hash;
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};


use indexmap::IndexMap;
use inkwell::basic_block::BasicBlock;
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

use crate::errors::{CompilerError, CompilerErrorType, SemanticError};
use crate::expr_lowerer::{IRExprPlaceResult, IRExprResult, IRExprValueResult};
use crate::parser::{ConditionalChain, ExprNode, ExprNodeEnum, FileContext, Function, Implementation, InfixOpr, Label, Literal, PrefixOpr, Scope, Span, Statement, Struct, Template, Templates, Trait, Variable};

pub type IRTypeId = usize;
pub type IRFunctionId = usize;
pub type IRScopeId = usize;
pub type IRTraitId = usize;
pub type IRImplId = usize;
pub type IRVarId = usize;

#[derive(PartialEq, Clone)]
pub struct IRVariable<'ctx> {
    pub name: String,
    pub place: IRExprPlaceResult<'ctx>,
    pub moved: bool,
    pub is_mut: bool
}

#[derive(PartialEq, Clone)]
pub struct IRConstant<'ctx> {
    pub name: String,
    pub value: IRExprValueResult<'ctx>
}


#[derive(PartialEq, Clone)]
pub struct IRVarDeclaration {
    pub name: String,
    pub type_id: IRTypeId,
    pub is_mut: bool,
    pub is_static: bool
}

pub type IRVariables<'ctx> = Vec<IRVariable<'ctx>>;
pub type IRVarDeclarations = Vec<IRVarDeclaration>;

#[derive(PartialEq, Clone)]
pub enum IRTypeEnum<'ctx> {
    Primitive(PrimitiveType),
    Reference { ptr_type_id: IRTypeId, is_mut: bool },
    Array { arr_type: IRTypeId, size: u64 },
    Callback { args: IRVariables<'ctx>, return_type: IRTypeId },
    Slice { slice_type: IRTypeId },
    UnsizedRef { unsized_type: IRTypeId, is_mut: bool },
    Struct(IRStruct<'ctx>)
}

#[derive(PartialEq, Clone)]
pub struct IRStruct<'ctx> {
    pub parent_scope: IRScopeId,
    pub scope: IRScopeId,
    pub templates_map: IRTemplatesMap<'ctx>,
    pub args: IRVarDeclarations,
    pub def: &'ctx Struct
}

#[derive(PartialEq, Clone)]
pub enum IRTemplateValue<'ctx> {
    Type(IRTypeId),
    Const(IRExprValueResult<'ctx>)
}
pub type IRTemplateKey = String;

#[derive(PartialEq, Clone)]
pub enum IRTemplate {
    Const(IRTypeId),
    Type
}

pub enum IRTypeConstraint {
    Trait(IRTraitId),
    Or()
}

#[derive(PartialEq, Clone)]
pub enum IRExprContext<'ctx> {
    Value(Option<IRTypeId>),
    Type,
    Function,
    Trait(IRTypeId),
    Impl(IRTemplateValue<'ctx>),
    Template(IRTemplate),
    TypeConstraint(IRTypeId)
}

pub type IRTemplatesMap<'ctx> = IndexMap<IRTemplateKey, IRTemplateValue<'ctx>>;

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

    pub fn get_mem_size(&self) -> u64 {
        match self {
            Self::Void | Self::Never => 0,
            Self::I8 | Self::U8 | Self::Bool | Self::Char => 1,
            Self::I16 | Self::U16 | Self::F16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            Self::I128 | Self::U128 => 16,
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

#[derive(Clone, Copy)]
pub enum CoreOpr {
    Prefix(PrefixOpr),
    Infix(InfixOpr)
}

impl CoreOpr {
    pub fn to_string(&self) -> String {
        match self {
            Self::Prefix(prefix) => prefix.to_string(),
            Self::Infix(infix) => infix.to_string()
        }
    }
}

#[derive(PartialEq)]
pub struct IRType<'ctx> {
    pub type_enum: IRTypeEnum<'ctx>,
    pub llvm_type: BasicTypeEnum<'ctx>,
    pub lowered_impls: Option<Vec<IRImplId>>
}

#[derive(PartialEq)]
pub struct IRImpl<'ctx> {
    pub scope: IRScopeId,
    pub type_id: IRTypeId,
    pub trait_id: Option<IRTraitId>,
    pub templates_map: IRTemplatesMap<'ctx>,
    pub ast_def: &'ctx Implementation
}

#[derive(PartialEq)]
pub struct IRTrait<'ctx> {
    pub scope: IRScopeId,
    pub parent_scope: IRScopeId,
    pub self_type: IRTypeId,
    pub sub_traits: Vec<IRTraitId>,
    pub templates_map: IRTemplatesMap<'ctx>,
    pub ast_def: &'ctx Trait
}

#[derive(PartialEq)]
pub struct IRFunction<'ctx> {
    pub parent_scope: IRScopeId,
    pub scope: IRScopeId,
    pub templates_map: IRTemplatesMap<'ctx>,
    pub args: IRVarDeclarations,
    pub return_type: IRTypeId,
    pub llvm_type: FunctionType<'ctx>,
    pub llvm_value: FunctionValue<'ctx>,
    pub ast_def: &'ctx Function,
    pub has_body: bool
}

#[derive(PartialEq, Clone, Debug)]
pub enum IRScopePath {
    ModulePath(Vec<String>),
    Function(IRFunctionId),
    Type(IRTypeId),
    Trait(IRTypeId),
    Impl(IRImplId)
}

#[derive(PartialEq, Clone)]
pub struct IRScope<'ctx> {
    pub parent_scope: Option<IRScopeId>,
    pub path: IRScopePath,
    pub templates_map: IRTemplatesMap<'ctx>,
    pub ast_def: Option<&'ctx Scope>,
    pub constants: Vec<IRConstant<'ctx>>
}


#[derive(Clone)]
pub struct IRLoop<'ctx> {
    pub loop_block: BasicBlock<'ctx>,
    pub merge_block: BasicBlock<'ctx>,
    pub label: Option<Label>,
    pub span: Span,
    pub ctx_type: IRExprContext<'ctx>,
    pub phi_values: IRPhiValues<'ctx>
}

pub type IRPhiValues<'ctx> = Vec<(IRExprValueResult<'ctx>, BasicBlock<'ctx>)>;

pub struct IRFunContext<'ctx> {
    pub fun: IRFunctionId,
    pub vars: IRVariables<'ctx>,
    pub loop_stack: Vec<IRLoop<'ctx>>
}

pub enum IRContext<'ctx> {
    FunContext(IRFunContext<'ctx>),
    ScopeContext(IRScopeId),
    ImplConstraintContext(IRImplId),
    ImplDefContext(IRScopeId, IndexMap<IRTemplateKey, IRTemplate>, IRTemplatesMap<'ctx>)
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
    pub funs_table: Vec<Option<IRFunction<'ctx>>>,
    pub scopes_table: Vec<IRScope<'ctx>>,
    pub impls_table: Vec<Option<IRImpl<'ctx>>>,
    pub traits_table: Vec<IRTrait<'ctx>>,
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn new(ast_scope: &'ctx Scope, file_context: FileContext, llvm_context: &'ctx Context) -> Self {
        let module: Module = llvm_context.create_module("main_module");
        let builder: Builder<'_> = llvm_context.create_builder();
        CodeLowerer { ast_scope, file_context, llvm_context, module, builder, types_table: Vec::new(), funs_table: Vec::new(), scopes_table: Vec::new(), impls_table: Vec::new(), traits_table: Vec::new() }
    }

    pub fn reserve_function_id(&mut self) -> IRFunctionId {
        let id = self.funs_table.len();
        self.funs_table.push(None);
        id
    }

    pub fn reserve_impl_id(&mut self) -> IRImplId {
        let id = self.impls_table.len();
        self.impls_table.push(None);
        id
    }

    pub fn ir_type(&self, type_id: IRTypeId) -> &IRType<'ctx> { &self.types_table[type_id] }
    pub fn ir_function(&self, fun_id: IRFunctionId) -> &IRFunction<'ctx> { self.funs_table[fun_id].as_ref().unwrap() }
    pub fn ir_scope(&self, scope_id: IRScopeId) -> &IRScope<'ctx> { &self.scopes_table[scope_id] }
    pub fn ir_impl(&self, impl_id: IRImplId) -> &IRImpl<'ctx> { &self.impls_table[impl_id].as_ref().unwrap() }
    pub fn ir_trait(&self, trait_id: IRTraitId) -> &IRTrait<'ctx> { &self.traits_table[trait_id] }

    pub fn scope_id(&mut self, ir_scope: IRScope<'ctx>) -> Result<IRScopeId, CompilerError> {
        if let Some((i, _)) = self.scopes_table.iter().enumerate().find(|(_, cur_ir)| cur_ir.ast_def == ir_scope.ast_def && cur_ir.templates_map == ir_scope.templates_map && cur_ir.parent_scope == ir_scope.parent_scope) {
            return Ok(i);
        }
        let id = self.scopes_table.len();
        self.scopes_table.push(ir_scope);

        if let Some(ast_def) = self.ir_scope(id).ast_def {
            let mut ir_context = IRContext::ScopeContext(id);
            for statement in ast_def.statements.iter() {
                if let Statement::Const(const_def) = statement {
                    self.lower_constant(&mut ir_context, const_def)?;
                }
            }
        }

        Ok(id)
    }

    pub fn trait_id(&mut self, ir_trait: IRTrait<'ctx>) -> IRTraitId {
        if let Some((i, _)) = self.traits_table.iter().enumerate().find(|(_, cur_ir)| **cur_ir == ir_trait) {
            return i;
        }
        let id = self.traits_table.len();
        self.traits_table.push(ir_trait);
        id
    }

    pub fn fun_id(&mut self, ir_fun: IRFunction<'ctx>) -> IRFunctionId {
        if let Some((i, _)) = self.funs_table.iter().enumerate().find(|(_, opt_cur_ir)| if let Some(cur_ir) = opt_cur_ir { cur_ir.parent_scope == ir_fun.parent_scope && cur_ir.ast_def.name == ir_fun.ast_def.name } else { false }) {
            return i;
        }
        let id = self.funs_table.len();
        self.funs_table.push(Some(ir_fun));
        id
    }

    pub fn type_id(&mut self, ir_type: IRType<'ctx>) -> IRTypeId {
        if let Some((i, _)) = self.types_table.iter().enumerate().find(|(_, cur_ir)| cur_ir.type_enum == ir_type.type_enum) {
            return i;
        }
        let id = self.types_table.len();
        self.types_table.push(ir_type);
        id
    }

    pub fn impl_id(&mut self, ir_impl: IRImpl<'ctx>) -> IRImplId {
        if let Some((i, _)) = self.impls_table.iter().enumerate().find(|(_, opt_cur_ir)| if let Some(cur_ir) = opt_cur_ir { cur_ir == &ir_impl } else { false }) {
            return i;
        }
        let id = self.impls_table.len();
        self.impls_table.push(Some(ir_impl));
        id
    }

    pub fn get_templates_keys_from(&mut self, ast_templates: &Templates) -> Result<Vec<IRTemplateKey>, CompilerError> {
        let mut result: Vec<IRTemplateKey> = Vec::new();
        for ast_template in ast_templates.templates.iter() {
            let template = match ast_template {
                Template::VarType(name, _) => name.clone(),
                Template::Const(const_value) => const_value.name.clone()
            };
            result.push(template);
        }
        Ok(result)
    }

    pub fn ensure_templates_constraints(&mut self, ir_context: &mut IRContext<'ctx>, templates_map: &IRTemplatesMap<'ctx>, ast_templates: &Templates, call_span: Option<Span>) -> Result<(), CompilerError> {
        for ast_template in ast_templates.templates.iter() {
            match ast_template {
                Template::VarType(key, opt_constraint) => {
                    let type_id = match templates_map[key] { IRTemplateValue::Type(type_id) => type_id, _ => panic!() };
                    if let Some(constraint) = opt_constraint {
                        let res = self.lower_expr(ir_context, constraint, &IRExprContext::TypeConstraint(type_id))?;
                        if let IRExprResult::NoTypeConstraintMatch = res {
                            return Err(self.error(SemanticError::InvalidTemplateValue { key: key.clone(), type_str: self.format_type(type_id), templates_str: ast_templates.to_string() }, call_span));
                        }
                    }
                },
                Template::Const(const_value) => {
                    let expected_type = self.get_type(ir_context, &const_value.var_type, &IRExprContext::Type)?;
                    let value = match templates_map[&const_value.name] { IRTemplateValue::Const(const_value) => const_value, _ => panic!() };
                    self.ensure_type_matches(value.type_id, Some(expected_type), Some(const_value.span), false)?;
                }
            }
        }
        Ok(())
    }

    pub fn get_context_scope(&mut self, ir_context: &IRContext<'ctx>) -> IRScopeId {
        match ir_context {
            IRContext::FunContext(fun) => self.ir_function(fun.fun).scope,
            IRContext::ScopeContext(scope) => *scope,
            IRContext::ImplConstraintContext(impl_id) => self.ir_impl(*impl_id).scope,
            IRContext::ImplDefContext(parent_scope, _, _) => *parent_scope
        }
    }
    
    pub fn get_name_parent_scope(&self, parent_scope: IRScopeId, name: &str, opt_call_span: Option<Span>) -> Result<Option<IRScopeId>, CompilerError> {
        let ir_scope = self.ir_scope(parent_scope);
        let fun_results_len = if let Some(ast_def) = ir_scope.ast_def { ast_def.functions.iter().filter(|def| def.name == name).count() } else { 0 };
        let structs_results_len = if let Some(ast_def) = ir_scope.ast_def { ast_def.structs.iter().filter(|def| def.name == name).count() } else { 0 };
        let traits_results_len = if let Some(ast_def) = ir_scope.ast_def { ast_def.traits.iter().filter(|def| def.name == name).count() } else { 0 };
        let type_def_results_len = if let Some(ast_def) = ir_scope.ast_def { ast_def.type_defs.iter().filter(|def| def.name == name).count() } else { 0 };
        let constants_results_len = if let Some(ast_def) = ir_scope.ast_def { ast_def.statements.iter().filter(|def| if let Statement::Const(const_def) = def { const_def.name == name } else { false }).count() } else { 0 };
        let total_len = fun_results_len + structs_results_len + traits_results_len + type_def_results_len + constants_results_len;

        if total_len > 1 {
            return Err(self.error(SemanticError::CollidingNames { parent_scope: self.format_scope_path(parent_scope), name: name.to_string() }, opt_call_span));
        }

        if let Some(grand_parent_scope) = ir_scope.parent_scope && let Some(mega_grand_scope) = self.get_name_parent_scope(grand_parent_scope, name, opt_call_span)? {
            if total_len == 0 {
                Ok(Some(mega_grand_scope))
            } else {
                return Err(self.error(SemanticError::CollidingNames { parent_scope: self.format_scope_path(mega_grand_scope), name: name.to_string() }, opt_call_span));
            }
        } else {
            Ok(if total_len == 0 { None } else { Some(parent_scope) })
        }
    }
    pub fn get_module_scope_in_scope(&mut self, parent_scope: IRScopeId, name: &str) -> Result<Option<IRScopeId>, CompilerError> {
        let ir_parent_scope: &IRScope<'_> = self.ir_scope(parent_scope);
        if let Some(ast_def) = ir_parent_scope.ast_def {
            for module in ast_def.modules.iter() {
                if module.name == name {
                    let mut new_parent_path = if let IRScopePath::ModulePath(path) = ir_parent_scope.path.clone() { path } else { panic!("CodeLowerer::get_module_scope_in_scope") };
                    new_parent_path.push(name.to_string());
                    let ir_scope = IRScope { parent_scope: Some(parent_scope), path: IRScopePath::ModulePath(new_parent_path), templates_map: IndexMap::new(), ast_def: Some(&module.scope), constants: Vec::new() };
                    return Ok(Some(self.scope_id(ir_scope)?));
                }
            }
        }
        if let Some(grand_parent_scope) = ir_parent_scope.parent_scope {
            self.get_module_scope_in_scope(grand_parent_scope, name)
        } else {
            Ok(None)
        }
    }
    
    pub fn get_global_scope(&mut self) -> Result<IRScopeId, CompilerError> {
        self.scope_id(IRScope{ parent_scope: None, path: IRScopePath::ModulePath(vec![]), templates_map: IndexMap::new(), ast_def: Some(&self.ast_scope), constants: Vec::new() })
    }

    pub fn format_scope_path(&self, scope: IRScopeId) -> String {
        let ir_scope: &IRScope<'_> = self.ir_scope(scope);
        match &ir_scope.path {
            IRScopePath::ModulePath(path) => {
                path.join(".")
            },
            IRScopePath::Function(fun) => {
                let ir_fun = self.ir_function(*fun);
                self.format_child_scope_path(ir_fun.parent_scope, &ir_fun.ast_def.name, &ir_fun.templates_map)
            },
            IRScopePath::Impl(impl_id) => {
                let ir_impl = self.ir_impl(*impl_id);
                let type_str = self.format_type(ir_impl.type_id);
                type_str
                /*if let Some(trait_id) = ir_impl.trait_id {
                    format!("{}", self.format_scope_path(self.ir_trait(trait_id).scope))
                } else {
                    format!("impl {}", type_str)
                }*/
            },
            IRScopePath::Trait(trait_id) => {
                let ir_trait = self.ir_trait(*trait_id);
                self.format_child_scope_path(ir_trait.parent_scope, &ir_trait.ast_def.name, &ir_trait.templates_map)
            },
            IRScopePath::Type(type_id) => {
                let ir_type = self.ir_type(*type_id);
                match &ir_type.type_enum {
                    IRTypeEnum::Struct(_struct) => {
                        self.format_child_scope_path(_struct.parent_scope, &_struct.def.name, &_struct.templates_map)
                    },
                    _ => todo!("format_scope_path")
                }
            }
        }
    }

    pub fn format_child_scope_path(&self, parent_scope: IRScopeId, name: &str, templates_map: &IRTemplatesMap) -> String {
        let mut parent_str = self.format_scope_path(parent_scope);
        if !parent_str.is_empty() { parent_str.push(':');}
        format!("{}{}{}", parent_str, name, self.format_templates_values(templates_map))
    }

    pub fn format_templates_values(&self, templates_map: &IRTemplatesMap) -> String {
        if templates_map.is_empty() {
            "".to_string()
        } else {
            let templates_values_str = templates_map.iter().map(|(key, value)|format!("{} = {}", key, match value {
                IRTemplateValue::Type(type_id) => self.format_type(*type_id),
                IRTemplateValue::Const(const_value) => const_value.llvm_value.to_string()
            })).collect::<Vec<String>>().join(", ");
            format!("<{}>", templates_values_str)
        }
    }

    pub fn format_type(&self, _type: IRTypeId) -> String {
        let ir_type = self.ir_type(_type);

        match &ir_type.type_enum {
            IRTypeEnum::Primitive(primitive) => {
                primitive.to_string()
            },
            IRTypeEnum::Reference { ptr_type_id, is_mut } => {
                let ptr_type_string: String = self.format_type(*ptr_type_id);
                format!("{}{}", if *is_mut { "&mut " } else { "&" }, ptr_type_string)
            },
            IRTypeEnum::Struct(_struct) => {
                self.format_scope_path(_struct.scope)
            },
            IRTypeEnum::UnsizedRef { unsized_type, is_mut } => {
                let ptr_type_string: String = self.format_type(*unsized_type);
                if *is_mut { format!("&mut {}", ptr_type_string) } else { format!("&{}", ptr_type_string) }         
            }
            IRTypeEnum::Array { arr_type, size } => {
                format!("[{}; {}]", self.format_type(*arr_type), size)
            },
            IRTypeEnum::Slice { slice_type } => {
                format!("[{}]", self.format_type(*slice_type))
            }
            _ => todo!("format_type")
        }
    }

    pub fn error(&self, err_type: SemanticError, opt_span: Option<Span>) -> CompilerError {
        if let Some(span) =  opt_span {
            let file: String = self.file_context.get_path(span.file_id);
            let chars: &Vec<char> = &self.file_context.files[&span.file_id];
            let line_end_idx: usize = chars[span.line_index..].iter().position(|&c| c == '\n').map(|pos| span.line_index + pos).unwrap_or(chars.len());
            let line_str: String = chars[span.line_index..line_end_idx].iter().collect();
            CompilerError { err_type: CompilerErrorType::SemanticError(err_type), file, span: Some(span), line_str }
        } else {
            CompilerError { err_type: CompilerErrorType::SemanticError(err_type), file: "".to_string(), span: None, line_str: "".to_string() }
        }
    }

    pub fn export_ir_to_file(&self, file_path: &Path) {
        self.module.print_to_file(file_path).expect("Failed to write IR");
        // self.module.print_to_stderr();
    }
}