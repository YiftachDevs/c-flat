use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};

use crate::{code_lowerer::*, errors::CompilerError, function_lowerer::IRFunScope, parser::{ExprNode, ExprNodeEnum, Function, Span}};

pub struct IRExprResult<'ctx> {
    pub type_id: IRTypeId,
    pub llvm_value: Option<BasicValueEnum<'ctx>>
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn r_value(&mut self, expr_result: &IRExprResult<'ctx>) -> Option<BasicValueEnum<'ctx>> { // might cause bugs
        if let Some(expr_value) = expr_result.llvm_value {
            Some(if let BasicValueEnum::PointerValue(ptr_value) = expr_value {
                let llvm_type = self.ir_type(expr_result.type_id).llvm_type.unwrap();
                self.builder.build_load(llvm_type, ptr_value, "side_load").unwrap()
            } else {
                expr_value
            })
        } else {
            None
        }
    }

    pub fn get_type_from_expr(&self, ctx_scope: IRScopeId, expr: &ExprNode) -> Result<IRTypeId, CompilerError> {
        let ir_ctx_scope: &IRScope<'_> = self.ir_scope(ctx_scope);
        match &expr.value {
            ExprNodeEnum::Name(name) => { // could be either template, module, struct, enum. (also fun name if after comes a struct / enum or recusively another fun name)
                for template_key in ir_ctx_scope.templates_values.keys() {
                    if let IRTemplateKey::Type(type_template_key_name) = template_key {
                        if type_template_key_name == name && let IRTemplateValue::Type(type_id) = ir_ctx_scope.templates_values[template_key] {
                            return Ok(type_id);
                        }
                    }
                }
                todo!("CodeLowerer::get_type_from_expr 1")
            },
            ExprNodeEnum::PostfixOpr(opr, left_expr, right_expr) => {
                todo!("CodeLowerer::get_type_from_expr 2")

            },
            _ => todo!("CodeLowerer::get_type_from_expr 3")
        }
    }

    pub fn lower_scope(&mut self, ir_fun_scope: &mut IRFunScope<'ctx>) -> Result<IRExprResult<'ctx>, CompilerError> {
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
}