use crate::errors::CompilerError;
use crate::parser::*;
use std::any::Any;
use std::collections::{HashMap, VecDeque};
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

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct FunctionId(usize);


#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct TypeId(i32);


struct IRFunction<'ctx> {
    fun_type: FunctionType<'ctx>,
    fun_value: FunctionValue<'ctx>,
    args: IRVariables<'ctx>,
    return_type_id: TypeId,
    fun_def: &'ctx Function
}

struct FunctionContext<'ctx> {
    ids: Vec<(NamePath, Option<TypeId>)>, // optional: [SomeModule.InnerMod.Struct<T1, 4>] value: Foo<5, 3>
    infos: HashMap<FunctionId, IRFunction<'ctx>>
}

impl<'ctx> FunctionContext<'ctx> {
    pub fn new() -> Self {
        Self { ids: Vec::new(), infos: HashMap::new() }
    }

    pub fn next_id(&mut self, fun_name: NamePath, opt_self_type: Option<TypeId>) -> FunctionId {
        for (i, (cur_name, cur_opt_self_type)) in self.ids.iter().enumerate() {
            if *cur_name == fun_name && *cur_opt_self_type == opt_self_type {
                return FunctionId(i);
            }
        }
        let id: FunctionId = FunctionId(self.ids.len());
        self.ids.push((fun_name, opt_self_type));
        id
    }

    pub fn get_name(&self, id: FunctionId) -> &(NamePath, Option<TypeId>) {
        for (i, value) in self.ids.iter().enumerate() {
            if id.0 == i {
                return value;
            }
        }
        panic!("Cannot get fun name of a non existing fun id");
    }

    pub fn get_ir_fun(&self, id: FunctionId) -> &IRFunction<'ctx> {
        if let Some(info) = self.infos.get(&id) {
            info
        } else {
            panic!("Received non existing fun id");
        }
    }
}

enum IRTypeEnum<'ctx> {
    Primitive(PrimitiveType),
    Pointer { ptr_type_id: TypeId, is_ref: bool },
    Array { arr_type: TypeId, size: usize },
    Callback { args: IRVariables<'ctx>, return_type: TypeId },
    Struct { args: IRVariables<'ctx>, def: &'ctx Struct }
}

struct IRType<'ctx> {
    llvm_type: BasicTypeEnum<'ctx>,
    is_void: bool,
    type_enum: IRTypeEnum<'ctx>
}

struct TypeContext<'ctx> {
    ids: Vec<VarType>,
    infos: HashMap<TypeId, IRType<'ctx>>
}

impl<'ctx> TypeContext<'ctx> {
    pub fn next_id(&mut self, var_type: VarType) -> TypeId {
        for (i, cur_type) in self.ids.iter().enumerate() {
            if *cur_type == var_type {
                return TypeId(i as i32);
            }
        }
        let result: TypeId = TypeId(self.ids.len() as i32);
        self.ids.push(var_type.clone());
        result        
    }

    pub fn get_type(&self, id: TypeId) -> &IRType<'ctx> {
        if let Some(info) = self.infos.get(&id) {
            info
        } else {
            panic!("Received non existing type id");
        }
    }
}

impl<'ctx> TypeContext<'ctx> {
    pub fn new() -> Self {
        Self { ids: Vec::new(), infos: HashMap::new() }
    }
}

#[derive(Clone)]
pub struct IRVariable<'ctx> {
    name: String,
    type_id: TypeId,
    is_mut: bool,
    is_resolved: bool,
    ptr: Option<PointerValue<'ctx>>
}

#[derive(Clone)]
pub struct IRVariables<'ctx> {
    vars: Vec<IRVariable<'ctx>>
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    main_scope: &'ctx Scope,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    alloca_builder: Builder<'ctx>,
    type_context: TypeContext<'ctx>,
    function_context: FunctionContext<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, main_scope: &'ctx Scope) -> Compiler<'ctx> {
        let module: Module = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        let alloca_builder: Builder<'_> = context.create_builder();
        Compiler { context: context, main_scope, module, builder, alloca_builder, type_context: TypeContext::new(), function_context: FunctionContext::new() }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let main_id: FunctionId = self.function_context.next_id(NamePath::new(["main".to_string()].to_vec(), None), None);
        self.build_fun(main_id)?;
        self.module.print_to_stderr();
        Ok(())
    }

    fn build_fun(&mut self, fun_id: FunctionId) -> Result<(), CompilerError> {
        let fun_def: &'ctx Function = self.find_fun_def(fun_id).ok_or(CompilerError::SemanticError(format!("Missing function '{}'", self.fun_id_to_string(fun_id))))?;
        let (name_path , opt_self_type) = self.function_context.get_name(fun_id).clone();
        let opt_templates_values: & Option<TemplatesValues> = &name_path.templates;
        let return_type_id: TypeId = self.build_type_id(&fun_def.return_type, &opt_templates_values);
        let mut args: IRVariables = IRVariables { vars: Vec::new() };
        let mut args_llvm_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for arg in fun_def.args.variables.iter() {
            let ir_var: IRVariable = self.build_ir_var(arg, &opt_templates_values);
            args_llvm_types.push(self.type_context.get_type(ir_var.type_id).llvm_type.into());
            args.vars.push(ir_var);
        }
        let return_ir_type = self.type_context.get_type(return_type_id);
        let fun_type = if return_ir_type.is_void {
            self.context.void_type().fn_type(args_llvm_types.as_slice(), false)
        } else {
            return_ir_type.llvm_type.fn_type(args_llvm_types.as_slice(), false)
        };

        let fun_str_name: String = self.fun_id_to_string(fun_id);
        let fun_value: FunctionValue<'_> = self.module.add_function(fun_str_name.as_str(), fun_type, None);

        let ir_fun = IRFunction {
            fun_type,
            fun_value,
            args,
            return_type_id,
            fun_def
        };
        let fun_id: FunctionId = self.function_context.next_id(name_path, opt_self_type);
        self.function_context.infos.insert(fun_id, ir_fun);

        self.build_fun_body(fun_id)?;

        Ok(())
    }

    fn build_fun_body(&mut self, fun_id: FunctionId) -> Result<(), CompilerError> {
        let ir_fun: &IRFunction<'_> = &self.function_context.infos[&fun_id];
        let fun_value: FunctionValue<'_> = ir_fun.fun_value;
        let entry_block = self.context.append_basic_block(fun_value, "entry");
        self.alloca_builder.position_at_end(entry_block);
        let mut cur_vars: IRVariables = ir_fun.args.clone();
        for arg in cur_vars.vars.iter_mut() {
            let arg_ptr = self.build_alloca(arg.type_id, arg.name.clone());
            arg.ptr = Some(arg_ptr);
        }
        self.builder.position_at_end(entry_block);
        for (i, arg) in cur_vars.vars.iter_mut().enumerate() {
            let arg_value = fun_value.get_nth_param(i as u32).unwrap();
            self.builder.build_store(arg.ptr.unwrap(), arg_value).unwrap();
        }
        let scope: &Scope = ir_fun.fun_def.scope.as_ref().unwrap();
        self.build_scope(scope, &mut cur_vars, fun_id)?;
        Ok(())
    }

    fn build_scope(&mut self, scope: &Scope, cur_vars: &mut IRVariables<'ctx>, fun_id: FunctionId) -> Result<(), CompilerError> {
        let (name_path , opt_self_type) = self.function_context.get_name(fun_id).clone();
        let opt_templates_values: &Option<TemplatesValues> = &name_path.templates;
        let ir_fun: &IRFunction<'_> = &self.function_context.infos[&fun_id];
        let fun_value: FunctionValue<'_> = ir_fun.fun_value;

        for statement in scope.statements.iter() {
            match statement {
                Statement::VarDeclaration(var) => {
                    let mut ir_var: IRVariable = self.build_ir_var(var, opt_templates_values);
                    let var_ptr = if let Some(init_expr) = var.init_expr.as_ref() {
                        let (expr_value, expr_type) = self.build_expr(init_expr);
                        if ir_var.type_id.0 != -1 && ir_var.type_id != expr_type {
                            todo!();
                        }
                        let var_ptr = self.build_alloca(expr_type, var.name.clone());
                        self.builder.build_store(var_ptr, expr_value).unwrap();
                        var_ptr
                    } else {
                        let type_id: TypeId = self.build_type_id(&var.var_type, opt_templates_values);
                        self.build_alloca(type_id, var.name.clone())
                    };
                    ir_var.ptr = Some(var_ptr);
                    cur_vars.vars.push(ir_var);
                }
                _ => { todo!() }
            }
        }
        Ok(())
    }

    fn build_alloca(&self, type_id: TypeId, name: String) -> PointerValue<'ctx> {
        let llvm_type = self.type_context.infos[&type_id].llvm_type;

        let entry_block = self.alloca_builder.get_insert_block().unwrap();
    
        match entry_block.get_first_instruction() {
            Some(first_instr) => self.alloca_builder.position_before(&first_instr),
            None => self.alloca_builder.position_at_end(entry_block),
        }

        self.alloca_builder.build_alloca(llvm_type, name.as_str()).unwrap()
    }


    fn build_expr(&mut self, expr: &ExprNode) -> (BasicValueEnum<'ctx>, TypeId) {
        (self.context.i32_type().const_int(0, false).into(), TypeId(0))
    }
    fn build_ir_var(&mut self, var: &Variable, opt_templates_values: &Option<TemplatesValues>) -> IRVariable<'ctx> {
        let type_id: TypeId = self.build_type_id(&var.var_type, opt_templates_values);
        IRVariable { name: var.name.clone(), type_id, is_mut: var.is_mut, is_resolved: var.is_resolved, ptr: None }
    }

    fn build_type_id(&mut self, var_type: &VarType, opt_templates_values: &Option<TemplatesValues>) -> TypeId {
        // TODO
        match var_type {
            VarType::Primitive(primitive_type) => {
                let id: TypeId = self.type_context.next_id(var_type.clone());
                let mut is_void: bool = false;
                let llvm_type = match primitive_type {
                    PrimitiveType::I8 | PrimitiveType::U8 => self.context.i8_type().into(),
                    PrimitiveType::I16 | PrimitiveType::U16 => self.context.i16_type().into(),
                    PrimitiveType::I32 | PrimitiveType::U32 | PrimitiveType::Char => self.context.i32_type().into(),
                    PrimitiveType::I64 | PrimitiveType::U64 => self.context.i64_type().into(),
                    PrimitiveType::I128 | PrimitiveType::U128 => self.context.i128_type().into(),
                    PrimitiveType::F16 => self.context.f16_type().into(),
                    PrimitiveType::F32 => self.context.f32_type().into(),
                    PrimitiveType::F64 => self.context.f64_type().into(),
                    PrimitiveType::F128 => self.context.f128_type().into(),
                    PrimitiveType::Bool => self.context.bool_type().into(),
                    PrimitiveType::Void => { is_void = true; self.context.bool_type().into() } // place holder
                };
                let ir_type = IRType { llvm_type, is_void, type_enum: IRTypeEnum::Primitive(*primitive_type) };
                self.type_context.infos.insert(id, ir_type);
                id
            },
            VarType::Pointer { ptr_type, is_ref } => {
                let ptr_type_id: TypeId = self.build_type_id(ptr_type, opt_templates_values);
                let id: TypeId = self.type_context.next_id(var_type.clone());
                let ir_type: IRType<'_> = IRType { llvm_type: self.context.ptr_type(inkwell::AddressSpace::from(0)).into(), is_void: false, type_enum: IRTypeEnum::Pointer { ptr_type_id, is_ref: *is_ref} };
                self.type_context.infos.insert(id, ir_type);
                id
            },
            _ => TypeId(-1) // TODO
        }
    }

    fn fun_id_to_string(&self, id: FunctionId) -> String {
        let value = self.function_context.get_name(id);
        let mut result: String = value.0.to_string();
        if let Some(type_id) = value.1 {
            result += self.type_id_to_string(type_id).as_str();
        }
        return result;
    }

    pub fn type_id_to_string(&self, id: TypeId) -> String {
        return "TODO".to_string();
    }

    pub fn find_fun_def(&self, fun_id: FunctionId) -> Option<&'ctx Function> {
        let (mut name_path , opt_self_type) = self.function_context.get_name(fun_id).clone();
        let mut cur_scope: &Scope = self.main_scope;


        while name_path.path.len() > 1 {
            let mod_name: String = name_path.path[0].clone();
            if let Some(module) = cur_scope.find_module(mod_name) {
                cur_scope = &module.scope;
                name_path = name_path.clone_pop();
            } else {
                return None;  
            }
        }

        if name_path.path.len() == 1 {
            let fun_name: String = name_path.path[0].clone();
            for fun in cur_scope.functions.iter() {
                if fun.name == fun_name {
                    return Some(fun);
                }
            }
            return None;
        } else {
            return None;
        }
    }

    /*fn evaluate_const_expr(&self, const_expr: ExprNode, ident: FunctionIdent) -> ConstValue {
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
    }*/
}