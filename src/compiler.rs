use crate::errors::{CompilerError, CompilerErrorType};
use crate::parser::*;
use std::any::Any;
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::path::Path;
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

type IRFunctionId = usize;
type IRTypeId = usize;

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

#[derive(PartialEq, Eq, Hash, Clone)]
struct IRScopeName {
    modules: Vec<String>,
    self_type: Option<IRTypeId>,
}

impl IRScopeName {
    pub fn new() -> Self {
        Self { modules: Vec::new(), self_type: None }
    }
}

#[derive(PartialEq, Clone)]
struct IRFunctionName {
    name: String,
    templates_values: IRTemplatesValues,
    scope_name: IRScopeName
}

struct IRFunction<'ctx> {
    fun_type: FunctionType<'ctx>,
    fun_value: FunctionValue<'ctx>,
    args: IRVariables<'ctx>,
    return_type_id: IRTypeId,
    fun_def: &'ctx Function
}

struct FunctionsHandler<'ctx> {
    names: Vec<IRFunctionName>, // optional: [SomeModule.InnerMod.Struct<T1, 4>] value: Foo<5, 3>
    funs: Vec<IRFunction<'ctx>>
}

impl<'ctx> FunctionsHandler<'ctx> {
    pub fn new() -> Self {
        Self { names: Vec::new(), funs: Vec::new() }
    }

    pub fn add_fun(&mut self, name: IRFunctionName, fun: IRFunction<'ctx>) -> IRFunctionId {
        let id: IRFunctionId = self.names.len();
        self.names.push(name);
        self.funs.push(fun);
        id
    }

    pub fn get_id(&self, name: IRFunctionName) -> Option<IRFunctionId> {
        match self.names.iter().enumerate().find(|(_, n)| **n == name) {
            Some((i, _)) => Some(i),
            None => None
        }
    }

    pub fn get_name(&self, id: IRFunctionId) -> &IRFunctionName {
        self.names.get(id).expect("Cannot get fun name of a non existing fun id")
    }

    pub fn get_fun(&self, id: IRFunctionId) -> &IRFunction<'ctx> {
        self.funs.get(id).expect("Cannot get fun of a non existing fun id")
    }
}

enum IRTypeEnum<'ctx> {
    Primitive(PrimitiveType),
    Pointer { ptr_type_id: IRTypeId, is_ref: bool },
    Array { arr_type: IRTypeId, size: usize },
    Callback { args: IRVariables<'ctx>, return_type: IRTypeId },
    Struct { args: IRVariables<'ctx>, def: &'ctx Struct }
}

#[derive(PartialEq, Clone)]
struct IRTypeName {
    var_type: VarType,
    templates_values: IRTemplatesValues
}

struct IRType<'ctx> {
    llvm_type: BasicTypeEnum<'ctx>,
    is_void: bool,
    type_enum: IRTypeEnum<'ctx>
}

struct TypesHandler<'ctx> {
    names: Vec<IRTypeName>,
    types: Vec<IRType<'ctx>>
}

impl<'ctx> TypesHandler<'ctx> {
    pub fn add_type(&mut self, name: IRTypeName, ir_type: IRType<'ctx>) -> IRTypeId {
        let id: IRTypeId = self.names.len();
        self.names.push(name);
        self.types.push(ir_type);
        id
    }

    pub fn get_id(&self, name: &IRTypeName) -> Option<IRTypeId> {
        match self.names.iter().enumerate().find(|(_, n)| **n == *name) {
            Some((i, _)) => Some(i),
            None => None
        }
    }

    pub fn get_type(&self, id: IRTypeId) -> &IRType<'ctx> {
        self.types.get(id).expect("Cannot get type of a non existing type id")
    }
}

impl<'ctx> TypesHandler<'ctx> {
    pub fn new() -> Self {
        Self { names: Vec::new(), types: Vec::new() }
    }
}

#[derive(Clone)]
pub struct IRVariable<'ctx> {
    name: String,
    type_id: Option<IRTypeId>,
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
    type_id: IRTypeId
}

pub struct IRContext<'ctx> {
    cur_fun: IRFunctionId,
    cur_vars: IRVariables<'ctx>,
    templates_values: IRTemplatesValues
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    file_context: &'ctx FileContext,
    main_scope: &'ctx Scope,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    alloca_builder: Builder<'ctx>,
    type_context: TypesHandler<'ctx>,
    function_context: FunctionsHandler<'ctx>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, file_context: &'ctx FileContext, main_scope: &'ctx Scope) -> Compiler<'ctx> {
        let module: Module = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        let alloca_builder: Builder<'_> = context.create_builder();
        Compiler { context, file_context, main_scope, module, builder, alloca_builder, type_context: TypesHandler::new(), function_context: FunctionsHandler::new() }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let main_name: IRFunctionName = IRFunctionName { name: "main".to_string(), templates_values: HashMap::new(), scope_name: IRScopeName::new() };
        self.build_fun(main_name)?;

        let file_path = Path::new("output.ll");
        self.module.print_to_file(file_path).expect("Failed to write IR");
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

    fn call_fun(&mut self, fun_id: IRFunctionId, fun_call_span: Option<Span>) -> Result<(), CompilerError> {
        todo!()
    }

    fn build_fun(&mut self, fun_name: IRFunctionName) -> Result<(), CompilerError> {
        let fun_def = self.find_fun_def(&fun_name).expect("Cannot find fun def, invalid name");
        let templates_values: &IRTemplatesValues = &fun_name.templates_values;
        let return_type_id: IRTypeId = self.build_var_type_id(&fun_def.return_type, &templates_values);
        let mut args: IRVariables = IRVariables { vars: Vec::new() };
        let mut args_llvm_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for arg in fun_def.args.variables.iter() {
            let ir_var: IRVariable = self.build_ir_var(arg, &templates_values);
            args_llvm_types.push(self.type_context.get_type(ir_var.type_id.unwrap()).llvm_type.into());
            args.vars.push(ir_var);
        }
        let return_ir_type = self.type_context.get_type(return_type_id);
        let fun_type = if return_ir_type.is_void {
            self.context.void_type().fn_type(args_llvm_types.as_slice(), false)
        } else {
            return_ir_type.llvm_type.fn_type(args_llvm_types.as_slice(), false)
        };

        let fun_str_name: String = self.fun_name_to_string(&fun_name);
        let fun_value: FunctionValue<'_> = self.module.add_function(fun_str_name.as_str(), fun_type, None);

        let ir_fun = IRFunction {
            fun_type,
            fun_value,
            args,
            return_type_id,
            fun_def
        };
        let fun_id = self.function_context.add_fun(fun_name, ir_fun);
        self.build_fun_body(fun_id)
    }

    fn build_fun_body(&mut self, fun_id: IRFunctionId) -> Result<(), CompilerError> {
        let ir_fun: &IRFunction<'_> = self.function_context.get_fun(fun_id);
        let fun_name: &IRFunctionName = self.function_context.get_name(fun_id);
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

        let total_templates_values = fun_name.templates_values.clone();
        if let Some(self_type) = fun_name.scope_name.self_type {

        }

        let mut scope_context = IRContext { cur_fun: fun_id, cur_vars, templates_values: total_templates_values };
        let return_type_id = ir_fun.return_type_id;
        let return_result: ExprResult<'_> = self.build_scope(scope, &mut scope_context, Some(ir_fun.return_type_id))?;
        if return_type_id != return_result.type_id {
            return Err(self.error("Type mismatch", Some(format!("Expected type {}, received type {} instead", self.type_id_to_string(return_type_id), self.type_id_to_string(return_result.type_id))), Some(scope.span)));
        }
        if let Some(value) = self.build_expr_result_value(&return_result) {
            self.builder.build_return(Some(&value)).expect("Return build failed");
        }
        Ok(())
    }

    fn build_scope(&mut self, scope: &Scope, ir_context: &mut IRContext<'ctx>, context_type: Option<IRTypeId>) -> Result<ExprResult<'ctx>, CompilerError> {
        let fun_name: IRFunctionName = self.function_context.get_name(ir_context.cur_fun).clone();
        let templates_values: &IRTemplatesValues = &fun_name.templates_values;

        for statement in scope.statements.iter() {
            match statement {
                Statement::VarDeclaration(var) => {
                    let mut ir_var: IRVariable<'ctx> = self.build_ir_var(var, templates_values);
                    let opt_var_ptr = if let Some(init_expr) = var.init_expr.as_ref() {
                        let expr_result = self.build_expr(init_expr, false, ir_context, ir_var.type_id)?;
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
                        let ir_type = self.type_context.get_type(ir_var.type_id.unwrap());
                        if ir_type.is_void {
                            None
                        } else {
                            let var_ptr = self.build_alloca(ir_var.type_id.unwrap(), var.name.clone());
                            Some(var_ptr)
                        }
                    };
                    ir_var.ptr = opt_var_ptr;
                    ir_context.cur_vars.vars.push(ir_var);
                },
                Statement::Expression { expr, is_final_value } => {
                    let expr_result = self.build_expr(expr, false, ir_context, context_type)?;
                    if *is_final_value {
                        return Ok(expr_result);
                    }
                }
                _ => { todo!() }
            }
        }

        let scope_type = self.get_primitive_type_id(PrimitiveType::Void);
        Ok(ExprResult { value: None, type_id: scope_type })
    }

    fn build_expr_result_value(&mut self, expr_result: &ExprResult<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        if let Some(expr_value) = expr_result.value {
            Some(if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.type_context.get_type(expr_result.type_id).llvm_type;
                self.builder.build_load(llvm_type, ptr_value, "side_load").unwrap()
            } else {
                expr_value
            })
        } else {
            None
        }
    }

    fn build_alloca(&self, type_id: IRTypeId, name: String) -> PointerValue<'ctx> {
        let llvm_type = self.type_context.get_type(type_id).llvm_type;

        let entry_block = self.alloca_builder.get_insert_block().unwrap();
    
        match entry_block.get_first_instruction() {
            Some(first_instr) => self.alloca_builder.position_before(&first_instr),
            None => self.alloca_builder.position_at_end(entry_block),
        }

        self.alloca_builder.build_alloca(llvm_type, name.as_str()).unwrap()
    }

    fn get_primitive_type_id(&mut self, prim: PrimitiveType) -> IRTypeId {
        let templates_values = HashMap::new();
        match prim {
            PrimitiveType::Void => self.build_var_type_id(&VarType::Primitive(PrimitiveType::Void), &templates_values),
            PrimitiveType::Bool => self.build_var_type_id(&VarType::Primitive(PrimitiveType::Bool), &templates_values),
            PrimitiveType::Char => self.build_var_type_id(&VarType::Primitive(PrimitiveType::Char), &templates_values),
            PrimitiveType::U8 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::U8), &templates_values),
            PrimitiveType::I8 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::I8), &templates_values),
            PrimitiveType::U16 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::U16), &templates_values),
            PrimitiveType::I16 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::I16), &templates_values),
            PrimitiveType::F16 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::F16), &templates_values),
            PrimitiveType::U32 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::U32), &templates_values),
            PrimitiveType::I32 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::I32), &templates_values),
            PrimitiveType::F32 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::F32), &templates_values),
            PrimitiveType::U64 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::U64), &templates_values),
            PrimitiveType::I64 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::I64), &templates_values),
            PrimitiveType::F64 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::F64), &templates_values),
            PrimitiveType::U128 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::U128), &templates_values),
            PrimitiveType::I128 => self.build_var_type_id(&VarType::Primitive(PrimitiveType::I128), &templates_values)
        }
    }

    fn build_expr(&mut self, expr: &ExprNode, expects_mut: bool, ir_context: &mut IRContext<'ctx>, context_type: Option<IRTypeId>) -> Result<ExprResult<'ctx>, CompilerError> {
        match &expr.value {
            ExprNodeEnum::ConstValue(const_value) => {
                match const_value {
                    ConstValue::Bool(v) => {
                        Ok(ExprResult{ value: Some(self.context.bool_type().const_int(*v as u64, false).into()), type_id: self.get_primitive_type_id(PrimitiveType::Bool)})
                    },
                    ConstValue::Char(ch) => {
                        Ok(ExprResult{ value: Some(self.context.i32_type().const_int(*ch as u64, false).into()), type_id: self.get_primitive_type_id(PrimitiveType::Char)})
                    },
                    ConstValue::UnresolvedInteger(int) => {
                        let default = (self.get_primitive_type_id(PrimitiveType::I32), self.context.i32_type());
                        let (int_type_id, llvm_int_type) = if let Some(ctx_t) = context_type {
                            if let IRTypeEnum::Primitive(prim) = self.type_context.get_type(ctx_t).type_enum && (prim.is_int() | prim.is_uint()) {
                                (self.build_var_type_id(&VarType::Primitive(prim), &ir_context.templates_values), self.type_context.get_type(ctx_t).llvm_type.into_int_type())
                            } else {
                                default
                            }
                        } else {
                            default
                        };
                        Ok(ExprResult{ value: Some(llvm_int_type.const_int(*int as u64, false).into()), type_id: int_type_id})
                    },
                    ConstValue::Float(float) => {
                        let default = (self.get_primitive_type_id(PrimitiveType::F32), self.context.f32_type());
                        let (float_type_id, llvm_float_type) = if let Some(ctx_t) = context_type {
                            if let IRTypeEnum::Primitive(prim) = self.type_context.get_type(ctx_t).type_enum && prim.is_float() {
                                (self.build_var_type_id(&VarType::Primitive(prim), &ir_context.templates_values), self.type_context.get_type(ctx_t).llvm_type.into_float_type())
                            } else {
                                default
                            }
                        } else {
                            default
                        };
                        Ok(ExprResult{ value: Some(llvm_float_type.const_float(*float).into()), type_id: float_type_id})
                    },
                    _ => todo!()
                }
            },
            ExprNodeEnum::InfixOpr(opr, left_expr, right_expr) => {
                self.build_infix_opr_block(*opr, left_expr, right_expr, expr.span, ir_context, context_type)
            },
            ExprNodeEnum::Name(name) => {
                let scope_name = &self.function_context.get_name(ir_context.cur_fun).scope_name;
                if let Some(ir_var) = ir_context.cur_vars.vars.iter().find(|v| v.name == *name) {
                    if expects_mut && !ir_var.is_mut {
                        return Err(self.error("Cannot mutate immutable variable", None, Some(expr.span)))
                    }
                    let value: Option<BasicValueEnum<'_>> = if let Some(ptr_value) = ir_var.ptr { Some(ptr_value.into()) } else { None };
                    return Ok(ExprResult{ value, type_id: ir_var.type_id.unwrap()});
                }
                todo!();
            },
            ExprNodeEnum::Scope(scope) => {
                self.build_scope(scope, ir_context, context_type)
            },
            _ => todo!()
        }
    }

    fn is_impl_trait(&self, type_id: IRTypeId) -> bool { false } // todo

    fn build_prim_infix_opr_block(&mut self, opr: InfixOpr, prim: PrimitiveType, left_value: &BasicValueEnum<'ctx>, right_value: &BasicValueEnum<'ctx>, left_type: IRTypeId, right_type: IRTypeId, span: Span) -> Result<ExprResult<'ctx>, CompilerError> {
        let left_ir_type: &IRType<'ctx> = &self.type_context.get_type(left_type);
        let right_ir_type: &IRType<'ctx> = &self.type_context.get_type(right_type);
        let left_type_str = self.type_id_to_string(left_type);
        let right_type_str = self.type_id_to_string(right_type);

        match opr {
            InfixOpr::Add => {
                let err = self.error("Invalid addition", Some(format!("{} does not implement the Add trait", left_type_str)), Some(span));
                let value = Some(match prim {
                    _ if prim.is_int() | prim.is_uint() => self.builder.build_int_add(left_value.into_int_value(), right_value.into_int_value(), "add_tmp").unwrap().into(),
                    _ if prim.is_float() => self.builder.build_float_add(left_value.into_float_value(), right_value.into_float_value(), "add_tmp").unwrap().into(),
                    _ => return Err(err)
                });
                Ok(ExprResult{value, type_id: left_type})
            },
            InfixOpr::Sub => {
                let err = self.error("Invalid subtraction", Some(format!("{} does not implement the Sub trait", left_type_str)), Some(span));
                let value = Some(match prim {
                    _ if prim.is_int() | prim.is_uint() => self.builder.build_int_sub(left_value.into_int_value(), right_value.into_int_value(), "sub_tmp").unwrap().into(),
                    _ if prim.is_float() => self.builder.build_float_sub(left_value.into_float_value(), right_value.into_float_value(), "sub_tmp").unwrap().into(),
                    _ => return Err(err)
                });
                Ok(ExprResult{value, type_id: left_type})
            },
            InfixOpr::Mul => {
                let err = self.error("Invalid multiplication", Some(format!("{} does not implement the Mul trait", left_type_str)), Some(span));
                let value = Some(match prim {
                    _ if prim.is_int() | prim.is_uint() => self.builder.build_int_mul(left_value.into_int_value(), right_value.into_int_value(), "mul_tmp").unwrap().into(),
                    _ if prim.is_float() => self.builder.build_float_mul(left_value.into_float_value(), right_value.into_float_value(), "mul_tmp").unwrap().into(),
                    _ => return Err(err)
                });
                Ok(ExprResult{value, type_id: left_type})
            },
            _ => todo!()
        }
    }


    fn build_infix_opr_block(&mut self, opr: InfixOpr, left_expr: &ExprNode, right_expr: &ExprNode, span: Span, ir_context: &mut IRContext<'ctx>, context_type: Option<IRTypeId>) -> Result<ExprResult<'ctx>, CompilerError>  {
        let is_asn: bool = opr == InfixOpr::Asn;
        let left = self.build_expr(left_expr, is_asn, ir_context, context_type)?;

        if self.is_impl_trait(left.type_id) {
            todo!()
        }

        let right_context_type = if opr.is_shift() { None } else { Some(left.type_id) };
        let right = self.build_expr(right_expr,  false, ir_context, right_context_type)?;

        let left_type_str = self.type_id_to_string(left.type_id);
        let right_type_str = self.type_id_to_string(right.type_id);

        if left.type_id != right.type_id {
            return Err(self.error("Type mismatch", Some(format!("Operator {} expectes values of the same type, instead {} and {} were given", opr.to_string(), left_type_str, right_type_str)), Some(span)));
        }

        let right_value = self.build_expr_result_value(&right);
        let void_type: IRTypeId = self.build_var_type_id(&VarType::Primitive(PrimitiveType::Void), &HashMap::new());

        if is_asn {
            return if let Some(left_value) = left.value {
                if let BasicValueEnum::PointerValue(ptr_value) = left_value {
                    self.builder.build_store(ptr_value, right_value.unwrap()).unwrap();
                    Ok(ExprResult{value: None, type_id: void_type})
                } else {
                    Err(self.error("Invalid assignment", Some("Cannot assign a value to a value".to_string()), Some(span)))
                }
            } else {
                Ok(ExprResult{value: None, type_id: void_type})
            }
        }

        if left.type_id == void_type || right.type_id == void_type {
            return Err(self.error("What are you doing, my guy?", Some(format!("Cannot apply operator {} between void types", opr.to_string())), Some(span)));
        }

        let left_value = self.build_expr_result_value(&left).unwrap();
        let left_ir_type: &IRType<'ctx> = &self.type_context.get_type(left.type_id);

        match left_ir_type.type_enum {
            IRTypeEnum::Primitive(prim) => self.build_prim_infix_opr_block(opr, prim, &left_value, &right_value.unwrap(), left.type_id, right.type_id, span),
            _ => todo!()
        }
    }
    fn build_ir_var(&mut self, var: &Variable, templates_values: &IRTemplatesValues) -> IRVariable<'ctx> {
        let type_id: Option<IRTypeId> = if var.var_type == VarType::UnresolvedInitExpr { None } else { Some(self.build_var_type_id(&var.var_type, templates_values)) };
        IRVariable { name: var.name.clone(), type_id, is_mut: var.is_mut, is_resolved: var.is_resolved, ptr: None }
    }

    fn build_var_type_id(&mut self, var_type: &VarType, templates_values: &IRTemplatesValues) -> IRTypeId {
        let type_name = IRTypeName { var_type: var_type.clone(), templates_values: templates_values.clone() };
        if let Some(type_id) = self.type_context.get_id(&type_name) {
            return type_id;
        }
        match var_type {
            VarType::Primitive(primitive_type) => {
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
                self.type_context.add_type(type_name, ir_type)
            },
            VarType::Pointer { ptr_type, is_ref } => {
                let ptr_type_id: IRTypeId = self.build_var_type_id(ptr_type, templates_values);
                let ir_type: IRType<'_> = IRType { llvm_type: self.context.ptr_type(inkwell::AddressSpace::from(0)).into(), is_void: false, type_enum: IRTypeEnum::Pointer { ptr_type_id, is_ref: *is_ref } };
                self.type_context.add_type(type_name, ir_type)
            },
            _ => todo!("build_var_type_id for other var types")
        }
    }

    fn fun_name_to_string(&self, name: &IRFunctionName) -> String {
        let mut result: String = self.scope_name_to_string(&name.scope_name) + "." + name.name.as_str();
        result += self.templates_values_to_string(&name.templates_values).as_str();
        result
    }

    fn scope_name_to_string(&self, scope_name: &IRScopeName) -> String {
        let mut result = scope_name.modules.join(".");
        if let Some(type_id) = scope_name.self_type {
            result += format!(".{}", self.type_id_to_string(type_id)).as_str();
        }
        result
    }

    fn templates_values_to_string(&self, templates_values: &IRTemplatesValues) -> String {
        if templates_values.is_empty() {
            return "".to_string();
        }

        templates_values.values().map(|value| match value {
            IRTemplateValue::Type(type_id) => self.type_id_to_string(*type_id),
            IRTemplateValue::Const(const_value) => const_value.to_string()
        }).collect::<Vec<String>>().join(", ")
    }

    pub fn type_id_to_string(&self, id: IRTypeId) -> String {
        let ir_type = self.type_context.get_type(id);

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

    pub fn find_fun_def(&self, fun_name: &IRFunctionName) -> Option<&'ctx Function> {
        let mut cur_scope: &Scope = self.main_scope;

        for mod_name in fun_name.scope_name.modules.iter() {
            if let Some(module) = cur_scope.find_module(mod_name.as_str()) {
                cur_scope = &module.scope;
            } else {
                return None;  
            }
        }

        for fun in cur_scope.functions.iter() {
            if fun.name == fun_name.name {
                return Some(fun);
            }
        }
        return None;
    }

    /*fn evaluate_const_expr(&self, const_expr: ExprNode, ident: IRFunctionIdent) -> ConstValue {
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

    fn get_fn_type(&self, ident: IRFunctionIdent) -> FunctionType {
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
        let mut ident: IRFunctionIdent = self.cur_function;
        loop {
            
        }
    }

    pub fn build_next_function(&self) {
        let return_type_id = self.type_context.get_type_id(self.function_context.cur_fun.re)
        let main_type = self.context.void_type().fn_type(&[], false);
        let main_value = self.module.add_function("main", main_type, None);
        let main_ident = IRFunctionIdentifier { name: "main", templates: None, self_type: None };
        self.functions.insert(main_ident, fun_value);
    }*/
}