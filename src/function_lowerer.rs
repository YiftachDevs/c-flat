
use inkwell::{types::{BasicMetadataTypeEnum, BasicType}, values::{FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::CompilerError, parser::{Function, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn get_ir_var(&mut self, ir_context: &mut IRContext<'ctx>, var: &Variable) -> Result<IRVariable<'ctx>, CompilerError> {
        let type_id = self.get_type(ir_context, var.var_type.as_ref().unwrap(), None)?;
        Ok(IRVariable { name: var.name.clone(), type_id, is_mut: var.is_mut, llvm_value: None })
    }

    pub fn get_alloca(&self, type_id: IRTypeId, name: &str) -> PointerValue<'ctx> {
        let current_block = self.builder.get_insert_block();

        if let Some(entry_block) = current_block {
            match entry_block.get_first_instruction() {
                Some(first_instr) => self.builder.position_before(&first_instr),
                None => self.builder.position_at_end(entry_block),
            }
        }
        let alloca =  self.builder.build_alloca(self.ir_type(type_id).llvm_type.unwrap(), name).unwrap();
        if let Some(entry_block) = current_block {
            self.builder.position_at_end(entry_block);
        }
        alloca
    }

    pub fn find_fun_def_in_scope(&self, parent_scope: IRScopeId, name: &str) -> Option<(IRScopeId, &'ctx Function)> {
        let ir_scope = self.ir_scope(parent_scope);
        for fun in ir_scope.ast_def.unwrap().functions.iter() {
            if fun.name == name {
                return Some((parent_scope, fun));
            }
        }
        if let Some(grand_parent_scope) = ir_scope.parent_scope {
            self.find_fun_def_in_scope(grand_parent_scope, name)
        } else {
            None
        }
    }

    fn get_fun(&mut self, parent_scope: IRScopeId, name: &str, templates_map: &IRTemplatesMap) -> Option<IRFunctionId> {
        for (i, fun) in self.funs_table.iter().enumerate() {
            if fun.parent_scope == parent_scope && fun.ast_def.name == name && fun.templates_map == *templates_map {
                return Some(i);
            }
        }
        None
    }

    pub fn lower_fun(&mut self, parent_scope: IRScopeId, name: &str, templates_map: IRTemplatesMap, call_span: Option<Span>) -> Result<IRFunctionId, CompilerError> {
        if let Some(id) = self.get_fun(parent_scope, name, &templates_map) {
            return Ok(id);
        }
        let (_, fun_def) = if let Some(def) = self.find_fun_def_in_scope(parent_scope, name) {
            def
        } else {
            return Err(self.error("Missing function", Some(format!("Called a non existing function '{}'", name)), call_span));
        };
        let fun_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_map);
        let fun_id = self.funs_table.len();

        let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
        new_templates_map.extend(templates_map.clone());

        let fun_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Function(fun_id), path_string: fun_path_string.clone(), templates_map: new_templates_map, ast_def: Some(fun_def.scope.as_ref().unwrap()) });

        let mut ir_context = IRContext::ScopeContext(fun_scope);
        let return_type: IRTypeId = if let Some(return_t) = &fun_def.return_type { self.get_type(&mut ir_context, return_t, None)? } else { self.primitive_type(PrimitiveType::Void)? };
        let mut args: IRVariables = Vec::new();
        let mut args_llvm_types: Vec<BasicMetadataTypeEnum> = Vec::new();
        for arg in fun_def.args.variables.iter() {
            let ir_var: IRVariable = self.get_ir_var(&mut ir_context, arg)?;
            if let Some(llvm_type) = self.ir_type(ir_var.type_id).llvm_type {
                args_llvm_types.push(llvm_type.into()); 
                args.push(ir_var);
            }
        }

        let fun_llvm_type = if let Some(llvm_return_type) = self.ir_type(return_type).llvm_type {
            llvm_return_type.fn_type(args_llvm_types.as_slice(), false)
        } else {
            self.llvm_context.void_type().fn_type(args_llvm_types.as_slice(), false)
        };

        let fun_llvm_value: FunctionValue<'_> = self.module.add_function(fun_path_string.as_str(), fun_llvm_type, None);

        let ir_fun = IRFunction {
            parent_scope: parent_scope,
            scope: fun_scope,
            templates_map: templates_map.clone(),
            args: args,
            return_type: return_type,
            llvm_type: fun_llvm_type,
            llvm_value: fun_llvm_value,
            ast_def: fun_def
        };
        self.funs_table.push(ir_fun);

        self.lower_fun_scope(fun_id)?;

        Ok(fun_id)
    }

    fn lower_fun_scope(&mut self, fun: IRFunctionId) -> Result<(), CompilerError> {
        let ir_fun: &IRFunction<'_> = self.ir_function(fun);
        let fun_path_string = self.ir_scope(ir_fun.scope).path_string.clone();
        let return_type = ir_fun.return_type;
        let llvm_value: FunctionValue<'_> = ir_fun.llvm_value;
        let entry_block = self.llvm_context.append_basic_block(llvm_value, "entry");

        let original_block = self.builder.get_insert_block();
        self.builder.position_at_end(entry_block);

        let mut ir_fun_scope = IRFunContext { fun: fun, vars: Vec::new() };

        for (i, arg) in ir_fun.args.iter().enumerate() {
            let arg_value = llvm_value.get_nth_param(i as u32).unwrap();
            arg_value.set_name(&arg.name);
            let mut new_arg = arg.clone();
            new_arg.llvm_value = Some(arg_value);
            ir_fun_scope.vars.push(new_arg);
        }

        let mut ir_context = IRContext::FunContext(ir_fun_scope);
        let result = self.lower_scope(&mut ir_context, ir_fun.ast_def.scope.as_ref().unwrap(), Some(return_type))?;
        let never_type = self.primitive_type(PrimitiveType::Never)?;
        if result.type_id != never_type && result.type_id != return_type {
            return Err(self.error("Type mismatch", Some(format!("Function {} returns type {}, yet {} was found", fun_path_string, self.format_type(return_type), self.format_type(result.type_id))), None));
        }

        if let Some(value) = self.r_value(&result) {
            self.builder.build_return(Some(&value)).expect("Return build failed");
        } else {
            self.builder.build_return(None).expect("Return build failed");
        }

        if let Some(block) = original_block {
            self.builder.position_at_end(block);
        }
        Ok(())
    }
}