use crate::errors::CompilerError;
use crate::parser::*;
use std::collections::{HashMap, VecDeque};
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::types::StructType;
use inkwell::types::AnyTypeEnum;
use inkwell::values::FunctionValue;

pub struct FunctionIdent {
    name: NamePath,
    templates: Option<TemplatesValues>,
    self_type: Option<VarType>
}

pub struct StructIdent {
    ident: NamePath
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct TypeId(u32);

struct Type<'ctx> {
    llvm_type: BasicTypeEnum<'ctx>
}

struct TypeContext<'ctx> {
    ids: HashMap<NamePath, TypeId>,
    infos: HashMap<TypeId, Type<'ctx>>
}

impl<'ctx> TypeContext<'ctx> {
    pub fn get_type(&self, name: NamePath) -> &'ctx TypeId {
        if self.ids.contains_key(name) {
            return &self.ids[name];
        }
        self.ids.insert(name, self.next_type_id());
    }

    pub fn get_type_info

    fn next_type_id(&self) -> TypeId {
        static X: std::sync::Mutex<u32> = std::sync::Mutex::new(0);
        *X.lock().unwrap() += 1;
        TypeId(*X.lock().unwrap())
    }
}

impl<'ctx> TypeContext<'ctx> {
    pub fn new() -> Self {
        Self { ids: HashMap::new(), infos: HashMap::new() }
    }
}

pub struct Compiler<'ctx> {
    context: Context<'ctx>,
    main_scope: &'ctx Scope,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_context: TypeContext,
    functions: HashMap<FunctionIdent, FunctionType<'ctx>>,
    cur_function: FunctionIdent
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, main_scope: &'ctx Scope) -> Compiler {
        let module: Module<'_> = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        let default_main_function_ident: FunctionIdent = FunctionIdent { name: "main".to_string(), templates: None, self_type: None };
        Compiler { context, main_scope, module, builder, type_context: TypeContext::new(), functions: HashMap::new(), cur_function: default_main_function_ident }
    }

    pub fn compile(&self) -> Result<(), CompilerError> {
        if let Some(fun) = self.main_scope.functions.iter().find(|fuc| fuc.name == "main") {
            self.build_cur_function();
        } else {
            Err(CompilerError::SemanticError("Missing main function"))
        }
        self.module.print_to_stderr();
    }

    fn evaluate_const_expr(&self, const_expr: ExprNode, ident: FunctionIdent) -> ConstValue {
        // TODO
    }

    fn build_basic_type_from_var_type(&self, var_type: VarType) -> BasicTypeEnum {
        match var_type {
            VarType::Array { arr_type, size_expr } => {
                let size = self.evaluate_const_expr()
            }
        }
    }

    fn get_fn_type(&self, ident: FunctionIdent) -> FunctionType {
        if let Some(var_type) = fun.return_type {
            match var_type {
                VarType::Primitive(primitive_type) => {

                }
                Var
            }
        } else {
            self.context.void_type().fn_type(param_types, is_var_args)
        }
    }

    pub fn find_cur_function_ast_from_ident(&self) -> Option<&Function<'ctx>> {
        let mut cur_scope: &Scope = main_scope;
        let mut ident: FunctionIdent = self.cur_function;
        loop {
            
        }
    }

    pub fn build_cur_function(&self) {
        let return_type = self.type_context.
        let main_type = self.context.void_type().fn_type(&[], false);
        let main_value = self.module.add_function("main", main_type, None);
        let main_ident = FunctionIdentifier { name: "main", templates: None, self_type: None };
        self.functions.insert(main_ident, fun_value);
    }
}