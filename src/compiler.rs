use crate::errors::CompilerError;
use crate::parser::*;
use std::collections::{HashMap, VecDeque};
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::StructType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::VoidType;
use inkwell::values::FunctionValue;

pub struct FunctionIdent {
    name: String,
    templates: Option<TemplatesValues>,
    self_type: Option<VarType>
}

pub struct StructIdent {
    ident: NamePath
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct FunctionId(usize);


#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct TypeId(usize);

impl VarType {
    pub fn build(&mut self, templates_values: &Option<TemplatesValues>) {

    }

    pub fn to_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        
    }

    pub fn to_fn_type<'ctx>(&self, args: &Variables, context: Context) -> FunctionType<'ctx> {
        let llvm_args: Vec<BasicMetadataTypeEnum<'ctx>> = args.variables.iter().map(|var| var.var_type.to_llvm_type(context).into()).collect();
        if self.is_void() {
            return context.void_type().fn_type(llvm_args.as_slice(), false); 
        }
        return 3;
    }
}

struct BuiltFunction<'ctx> {
    llvm_type: FunctionType<'ctx>,
    fun_value: Option<FunctionValue<'ctx>>,
    templates_values: Option<TemplatesValues>,
    args: Variables,
    return_type: VarType,
    def: &'ctx Function
}

impl<'ctx> BuiltFunction<'ctx> {
    pub fn build_header(def: &'ctx Function, templates_values: &Option<TemplatesValues>, type_context: &'ctx mut TypeContext) -> BuiltFunction<'ctx> {
        let mut return_type: VarType = def.return_type.clone();
        return_type.build(templates_values);
        let return_type_id: TypeId = type_context.get_type_id(return_type);
        let mut args: Variables = def.args.clone();
        for arg in args.variables.iter_mut() {
            arg.var_type.build(&templates_values);
        }
        let fun_type = if return_type.to_llvm_type().fn_type(param_types, is_var_args)
    }
}

struct FunctionContext<'ctx> {
    ids: Vec<(NamePath, Option<NamePath>)>, // optional: [SomeModule.InnerMod.Struct<T1, 4>] value: Foo<5, 3>
    infos: HashMap<FunctionId, BuiltFunction<'ctx>>,
    work: VecDeque<FunctionId>,
    cur_fun: FunctionId
}

impl<'ctx> FunctionContext<'ctx> {
    pub fn new() -> Self {
        Self { ids: Vec::new(), infos: HashMap::new(), work: VecDeque::new(), cur_fun: FunctionId(0) }
    }

    fn find_fun_def(&self, scope: &'ctx Scope, fun_name: NamePath) -> Option<&'ctx Function> {
        // TODO
    }

    pub fn get_id(&mut self, fun_name: NamePath, opt_self_type: Option<NamePath>, scope: &'ctx Scope) -> Result<FunctionId, CompilerError> {
        for (i, (cur_name, cur_opt_self_type)) in self.ids.iter().enumerate() {
            if *cur_name == fun_name && *cur_opt_self_type == opt_self_type {
                return Ok(FunctionId(i));
            }
        }
        let id: FunctionId = FunctionId(self.ids.len());
        
        let fun_def: &Function = self.find_fun_def(scope, fun_name).ok_or(CompilerError::SemanticError(format!("Missing function '{}'", fun_name.to_string())))?;


        self.ids.push((fun_name, opt_self_type));
        id
    }

    pub fn build_fun(&mut self, name: NamePath, scope_path: NamePath, opt_self_type: Option<NamePath>, main_scope: &'ctx Scope, type_context: &'ctx mut TypeContext) -> FunctionId {
        for (i, (cur_name, cur_opt_self_type)) in self.ids.iter().enumerate() {
            if *cur_name == name && *cur_opt_self_type == opt_self_type {
                return FunctionId(i);
            }
        }
        let id: FunctionId = FunctionId(self.ids.len());
        self.ids.push((name, opt_self_type));

        if let Some(fun) = FunctionContext::find_fun_in_scope_rec(main_scope, name, scope_path) {
            self.infos.insert(id, BuiltFunction::build_header(fun));
        }
    }

    pub fn get_built_fun(&self, id: FunctionId) -> Option<&BuiltFunction<'ctx>> {
        if let Some(info) = self.infos.get(&id) {
            Some(info)
        } else {
            None
        }
    }
}

struct Type<'ctx> {
    llvm_type: BasicTypeEnum<'ctx>,
    var_type: Box<VarType>
}

struct TypeContext<'ctx> {
    ids: Vec<VarType>,
    infos: HashMap<TypeId, Type<'ctx>>
}

impl<'ctx> TypeContext<'ctx> {
    pub fn get_type_id(&mut self, var_type: VarType, context: &'ctx Context) -> TypeId {
        for (i, cur_type) in self.ids.iter().enumerate() {
            if *cur_type == var_type {
                return TypeId(i);
            }
        }
        let result: TypeId = TypeId(self.ids.len());
        self.ids.push(var_type.clone());
        self.infos.insert(result, Type { llvm_type: var_type.to_llvm_type(context), var_type: Box::new(var_type) });
        result        
    }

    pub fn get_type(&self, id: TypeId) -> Option<&Type<'ctx>> {
        if let Some(info) = self.infos.get(&id) {
            Some(info)
        } else {
            None
        }
    }
}

impl<'ctx> TypeContext<'ctx> {
    pub fn new() -> Self {
        Self { ids: Vec::new(), infos: HashMap::new() }
    }
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    main_scope: &'ctx Scope,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    type_context: TypeContext<'ctx>,
    function_context: FunctionContext<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, main_scope: &'ctx Scope) -> Compiler<'ctx> {
        let module: Module<'_> = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        Compiler { context: context, main_scope, module, builder, type_context: TypeContext::new(), function_context: FunctionContext::new() }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let main_id: FunctionId = self.function_context.get_id(NamePath::new(["main".to_string()].to_vec(), None), None);
        if let Some(fun) = self.find_fun(self.main_scope, , NamePath::new(Vec::new(), None)) {
            self.build_next_function();
        } else {
            return Err(CompilerError::SemanticError("Missing main function".to_string()));
        }
        self.module.print_to_stderr();
        Ok(())
    }

    pub fn find_fun_in_scope_rec(scope: &'ctx Scope, name_path: NamePath, scope_path: NamePath) -> Option<&'ctx Function> {
        if name_path.path.len() == 1 {
            if scope_path.path.len() > 0 {
                let mod_name: String = scope_path.path[0].clone();
                if let Some(module) = scope.find_module(mod_name) {
                    if let Some(fun) = Self::find_fun_in_scope_rec(&module.scope, name_path, scope_path.clone_pop()) {
                        return Some(fun);
                    }   
                }
            } else {
                let fun_name: String = name_path.path[0].clone();
                for fun in scope.functions.iter() {
                    if fun.name == fun_name {
                        return Some(fun);
                    }
                }
            }
            return None;
        } else {
            // TODO
            return None;
        }
    }

    fn evaluate_const_expr(&self, const_expr: ExprNode, ident: FunctionIdent) -> ConstValue {
        // TODO
        ConstValue::Void
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

    pub fn build_next_function(&self) {
        let return_type_id = self.type_context.get_type_id(self.function_context.cur_fun.re)
        let main_type = self.context.void_type().fn_type(&[], false);
        let main_value = self.module.add_function("main", main_type, None);
        let main_ident = FunctionIdentifier { name: "main", templates: None, self_type: None };
        self.functions.insert(main_ident, fun_value);
    }
}