use crate::errors::{CompilerError, CompilerErrorType};
use crate::parser::*;
use std::any::Any;
use std::collections::{HashMap, VecDeque};
use std::fs::File;
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
        self.ids.get(id.0).expect("Cannot get fun name of a non existing fun id")
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
    type_id: Option<TypeId>,
    is_mut: bool,
    is_resolved: bool,
    ptr: Option<PointerValue<'ctx>>
}

#[derive(Clone)]
pub struct IRVariables<'ctx> {
    vars: Vec<IRVariable<'ctx>>
}

pub struct ExprResult<'ctx> {
    value: Option<BasicValueEnum<'ctx>>,
    type_id: TypeId
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    file_context: &'ctx FileContext,
    main_scope: &'ctx Scope,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    alloca_builder: Builder<'ctx>,
    type_context: TypeContext<'ctx>,
    function_context: FunctionContext<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, file_context: &'ctx FileContext, main_scope: &'ctx Scope) -> Compiler<'ctx> {
        let module: Module = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        let alloca_builder: Builder<'_> = context.create_builder();
        Compiler { context, file_context, main_scope, module, builder, alloca_builder, type_context: TypeContext::new(), function_context: FunctionContext::new() }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let main_id: FunctionId = self.function_context.next_id(NamePath::new(["main".to_string()].to_vec(), None), None);
        self.build_fun(main_id, None)?;
        self.module.print_to_stderr();
        Ok(())
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

    fn build_fun(&mut self, fun_id: FunctionId, fun_call_span: Option<Span>) -> Result<(), CompilerError> {
        let fun_def: &'ctx Function = self.find_fun_def(fun_id).ok_or(self.error("Called a non existing function", Some(format!("Missing function '{}'", self.fun_id_to_string(fun_id))),fun_call_span))?;
        let (name_path , opt_self_type) = self.function_context.get_name(fun_id).clone();
        let opt_templates_values: & Option<TemplatesValues> = &name_path.templates;
        let return_type_id: TypeId = self.build_type_id(&fun_def.return_type, &opt_templates_values);
        let mut args: IRVariables = IRVariables { vars: Vec::new() };
        let mut args_llvm_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for arg in fun_def.args.variables.iter() {
            let ir_var: IRVariable = self.build_ir_var(arg, &opt_templates_values);
            args_llvm_types.push(self.type_context.get_type(ir_var.type_id.unwrap()).llvm_type.into());
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
            let arg_ptr = self.build_alloca(arg.type_id.unwrap(), arg.name.clone());
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

    fn build_scope(&mut self, scope: &Scope, cur_vars: &mut IRVariables<'ctx>, fun_id: FunctionId) -> Result<Option<ExprResult>, CompilerError> {
        let (name_path , opt_self_type) = self.function_context.get_name(fun_id).clone();
        let opt_templates_values: &Option<TemplatesValues> = &name_path.templates;
        let ir_fun: &IRFunction<'_> = &self.function_context.infos[&fun_id];
        let fun_value: FunctionValue<'_> = ir_fun.fun_value;

        for statement in scope.statements.iter() {
            match statement {
                Statement::VarDeclaration(var) => {
                    let mut ir_var: IRVariable = self.build_ir_var(var, opt_templates_values);
                    let opt_var_ptr = if let Some(init_expr) = var.init_expr.as_ref() {
                        let expr_result = self.build_expr(init_expr, ir_var.type_id, false, cur_vars)?;
                        let expr_type = expr_result.type_id;
                        if let Some(ir_var_type) = ir_var.type_id && ir_var_type != expr_type {
                            let expected_type_string: String = self.type_id_to_string(ir_var_type);
                            let received_type_string: String = self.type_id_to_string(expr_type);
                            let err_description: String = format!("Type mismatch during var declaration, expected: {}, received: {}", expected_type_string, received_type_string);
                            return Err(self.error("Type mismatch", Some(err_description), Some(var.span)));
                        }
                        ir_var.type_id = Some(expr_type);
                        if let Some(expr_value) = self.build_expr_result_value(&expr_result) {
                            let var_ptr: PointerValue = self.build_alloca(expr_type, var.name.clone());
                            self.builder.build_store(var_ptr, expr_value).unwrap();
                            Some(var_ptr)
                        } else {
                            None
                        }
                    } else {
                        let ir_type = &self.type_context.infos[&ir_var.type_id.unwrap()];
                        if ir_type.is_void {
                            None
                        } else {
                            let var_ptr = self.build_alloca(ir_var.type_id.unwrap(), var.name.clone());
                            Some(var_ptr)
                        }
                    };
                    ir_var.ptr = opt_var_ptr;
                    cur_vars.vars.push(ir_var);
                },
                Statement::Expression { expr, is_final_value } => {
                    let expr_result = self.build_expr(expr, None, false, cur_vars)?;
                    if *is_final_value {
                        return Ok(Some(expr_result));
                    }
                }
                _ => { todo!() }
            }
        }
        Ok(None)
    }

    fn build_expr_result_value(&mut self, expr_result: &ExprResult<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        if let Some(expr_value) = expr_result.value {
            Some(if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.type_context.infos[&expr_result.type_id].llvm_type;
                self.builder.build_load(llvm_type, ptr_value, "side_load").unwrap()
            } else {
                expr_value
            })
        } else {
            None
        }
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

    fn build_expr(&mut self, expr: &ExprNode, ctx_type: Option<TypeId>, expects_mut: bool, cur_vars: &mut IRVariables<'ctx>) -> Result<ExprResult<'ctx>, CompilerError> {
        match &expr.value {
            ExprNodeEnum::ConstValue(const_value) => {
                match const_value {
                    ConstValue::Bool(v) => {
                        let bool_type: TypeId = self.build_type_id(&VarType::Primitive(PrimitiveType::Bool), &None);
                        Ok(ExprResult{ value: Some(self.context.bool_type().const_int(*v as u64, false).into()), type_id: bool_type})
                    },
                    ConstValue::Char(ch) => {
                        let char_type: TypeId = self.build_type_id(&VarType::Primitive(PrimitiveType::Char), &None);
                        Ok(ExprResult{ value: Some(self.context.i32_type().const_int(*ch as u64, false).into()), type_id: char_type})
                    },
                    ConstValue::UnresolvedInteger(int) => {
                        let (int_type_id, llvm_int_type) = if let Some(ctx_t) = ctx_type {
                            if let IRTypeEnum::Primitive(prim) = self.type_context.infos[&ctx_t].type_enum && (prim.is_int() | prim.is_uint()) {
                                (self.build_type_id(&VarType::Primitive(prim), &None), self.type_context.infos[&ctx_t].llvm_type.into_int_type())
                            } else {
                                return Err(self.error("Type mismatch", Some(format!("Expected a {}, received an integer", self.type_id_to_string(ctx_t))), Some(expr.span)));
                            }
                        } else {
                            (self.build_type_id(&VarType::Primitive(PrimitiveType::I32), &None), self.context.i32_type())
                        };
                        Ok(ExprResult{ value: Some(llvm_int_type.const_int(*int as u64, false).into()), type_id: int_type_id})
                    },
                    ConstValue::Float(float) => {
                        let (float_type_id, llvm_float_type) = if let Some(ctx_t) = ctx_type {
                            if let IRTypeEnum::Primitive(prim) = self.type_context.infos[&ctx_t].type_enum && prim.is_float() {
                                (self.build_type_id(&VarType::Primitive(prim), &None), self.type_context.infos[&ctx_t].llvm_type.into_float_type())
                            } else {
                                return Err(self.error("Type mismatch", Some(format!("Expected a {}, received a float", self.type_id_to_string(ctx_t))), Some(expr.span)));
                            }
                        } else {
                            (self.build_type_id(&VarType::Primitive(PrimitiveType::F32), &None), self.context.f32_type())
                        };
                        Ok(ExprResult{ value: Some(llvm_float_type.const_float(*float).into()), type_id: float_type_id})
                    },
                    _ => todo!()
                }
            },
            ExprNodeEnum::InfixOpr(opr, left_expr, right_expr) => {
                self.build_infix_opr_block(*opr, left_expr, right_expr, cur_vars, ctx_type, expr.span)
            },
            ExprNodeEnum::NamePath(name_path) => {
                if let Some(ir_var) = cur_vars.vars.iter().find(|v| v.name == name_path.path[0]) {
                    if expects_mut && !ir_var.is_mut {
                        return Err(self.error("Cannot mutate immutable variable", None, Some(expr.span)))
                    }
                    let value: Option<BasicValueEnum<'_>> = if let Some(ptr_value) = ir_var.ptr { Some(ptr_value.into()) } else { None };
                    return Ok(ExprResult{ value, type_id: ir_var.type_id.unwrap()});
                }
                todo!();
            }
            _ => todo!()
        }
    }

    fn is_impl_trait(&self, type_id: TypeId) -> bool { false } // todo

    fn build_prim_infix_opr_block(&mut self, opr: InfixOpr, prim: PrimitiveType, left_value: &BasicValueEnum<'ctx>, right_value: &BasicValueEnum<'ctx>, left_type: TypeId, right_type: TypeId, span: Span) -> Result<ExprResult<'ctx>, CompilerError> {
        let left_ir_type: &IRType<'ctx> = &self.type_context.infos[&left_type];
        let right_ir_type: &IRType<'ctx> = &self.type_context.infos[&right_type];
        let left_type_str = self.type_id_to_string(left_type);
        let right_type_str = self.type_id_to_string(right_type);

        match opr {
            InfixOpr::Add => {
                let err = self.error("Invalid addition", Some(format!("{} does not implement the Add trait", left_type_str)), Some(span));
                let value = match prim {
                    _ if prim.is_int() | prim.is_uint() => self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_tmp").unwrap().into(),
                    _ if prim.is_float() => self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_tmp").unwrap().into(),
                    _ => return Err(err)
                };
                Ok(ExprResult{value, type_id: left_type})
            },
            InfixOpr::Sub => {
                let err = self.error("Invalid subtraction", Some(format!("{} does not implement the Add trait", left_type_str)), Some(span));
                let value = match prim {
                    _ if prim.is_int() | prim.is_uint() => self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_tmp").unwrap().into(),
                    _ if prim.is_float() => self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_tmp").unwrap().into(),
                    _ => return Err(err)
                };
                Ok(ExprResult{value, type_id: left_type})
            },
            _ => todo!()
        }
    }

    fn build_infix_opr_block(&mut self, opr: InfixOpr, left_expr: &ExprNode, right_expr: &ExprNode, cur_vars: &mut IRVariables<'ctx>, ctx_type: Option<TypeId>, span: Span) -> Result<ExprResult<'ctx>, CompilerError>  {
        let left = self.build_expr(left_expr, ctx_type, false, cur_vars)?;
            
        if self.is_impl_trait(left.type_id) {
            todo!()
        }

        let right_ctx_type = if opr.is_shift() { None } else { Some(left.type_id) };
        let right = self.build_expr(right_expr, right_ctx_type, true, cur_vars)?;

        let left_type_str = self.type_id_to_string(left.type_id);
        let right_type_str = self.type_id_to_string(right.type_id);

        if left.type_id != right.type_id {
            return Err(self.error("Type mismatch", Some(format!("Operator {} expectes values of the same type, instead {} and {} were given", opr.to_string(), left_type_str, right_type_str)), Some(span)));
        }

        let right_value = self.build_expr_result_value(&right);

        if let InfixOpr::Asn = opr {
            return if let Some(left_value) = left.value {
                if let BasicValueEnum::PointerValue(ptr_value) = left_value {
                    self.builder.build_store(ptr_value, right_value).unwrap();
                    Ok(ExprResult{value: self.context.void_type(), type_id: left.type_id})
                } else {
                    Err(self.error("Invalid assignment", Some("Cannot assign a value to a value".to_string()), Some(span)))
                }
            } else {

            }
        }
        let left_value = self.build_expr_result_value(&left);
        let left_ir_type: &IRType<'ctx> = &self.type_context.infos[&left.type_id];

        match left_ir_type.type_enum {
            IRTypeEnum::Primitive(prim) => self.build_prim_infix_opr_block(opr, prim, &left_value, &right_value, left.type_id, right.type_id, span),
            _ => todo!()
        }
        /*match opr {
            InfixOpr::Add => {
                let err = self.error("Invalid addition", Some(format!("{} does not implement the Add trait", left_type_str)), Some(span));
                if let IRTypeEnum::Primitive(prim) = left_ir_type.type_enum {
                    let value = match prim {
                        _ if prim.is_int() | prim.is_uint() => self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_tmp").unwrap().into(),
                        _ if prim.is_float() => self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_tmp").unwrap().into(),
                        _ => return Err(err)
                    };
                    Ok(ExprResult{value, type_id: left.type_id})
                } else {
                    Err(err)
                }
            }
            _ => todo!()
        }*/

        /*match left_ir_type.type_enum {
            IRTypeEnum::Primitive(primitive) => {
                match primitive {
                    _ if primitive.is_int() => {
                        let int_type = left.type_id;
                        let right_value = match right { ExprResult::Value(value, type_id) => value.into_int_value(), _ => panic!() };
                        if let InfixOpr::Asn = opr {
                            return if let ExprResult::Pointer(ptr, type_id) = left {
                                self.builder.build_store(ptr, right_value).unwrap();
                                Ok(ExprResult::Pointer(ptr, type_id))
                            } else {
                                Err(self.error("Invalid assignment", Some("Cannot assign a value to a value".to_string()), Some(span)))
                            }
                        }
                        let left_value = self.build_expr_result_value(left).into_int_value();
                        println!("{:?}", right_value);
                        
                        let result = match opr {
                            InfixOpr::Add => self.builder.build_int_add(left_value, right_value, "add_tmp").unwrap(),
                            InfixOpr::Sub => self.builder.build_int_sub(left_value, right_value, "sub_tmp").unwrap(),
                            InfixOpr::Mul => self.builder.build_int_mul(left_value, right_value, "mul_tmp").unwrap(),
                            InfixOpr::Div => self.builder.build_int_signed_div(left_value, right_value, "div_tmp").unwrap(),
                            InfixOpr::Mod => self.builder.build_int_signed_rem(left_value, right_value, "mod_tmp").unwrap(),
                            InfixOpr::Eq => self.builder.build_int_compare(inkwell::IntPredicate::EQ, left_value, right_value, "eq_tmp").unwrap(),
                            _ => todo!()
                        };
                        Ok(ExprResult::Value(result.into(), if opr.is_boolean() { bool_type } else { int_type }))
                    },
                    _ => todo!()
                }
                
            }
            _ => todo!()
        }*/
    }

    fn build_ir_var(&mut self, var: &Variable, opt_templates_values: &Option<TemplatesValues>) -> IRVariable<'ctx> {
        let type_id: TypeId = self.build_type_id(&var.var_type, opt_templates_values);
        IRVariable { name: var.name.clone(), type_id: if type_id.0 != -1 { Some(type_id) } else { None }, is_mut: var.is_mut, is_resolved: var.is_resolved, ptr: None }
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
            _ => TypeId(-1)
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
        let ir_type = &self.type_context.infos[&id];

        match ir_type.type_enum {
            IRTypeEnum::Primitive(primitive) => {
                primitive.to_string()
            },
            IRTypeEnum::Pointer { ptr_type_id, is_ref } => {
                let ptr_type_string: String = self.type_id_to_string(ptr_type_id);
                let ptr_char = if is_ref { "&" } else { "*" };
                format!("{}{}", ptr_char, ptr_type_string)
            }
            _ => todo!()
        }
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