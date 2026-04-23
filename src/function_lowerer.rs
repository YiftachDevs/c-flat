
use inkwell::{AddressSpace, module::Linkage, types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, SemanticError}, expr_lowerer::IRExprPlaceResult, parser::{Function, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn get_ir_var_declaration(&mut self, ir_context: &mut IRContext<'ctx>, var: &Variable) -> Result<IRVarDeclaration, CompilerError> {
        let type_id = self.get_type(ir_context, var.var_type.as_ref().unwrap(), &IRExprContext::Type)?;
        Ok(IRVarDeclaration { name: var.name.clone(), type_id, is_mut: var.is_mut, is_static: var.is_static })
    }

    pub fn alloc_var(&self, ir_fun_context: &mut IRFunContext<'ctx>, var_dec: &IRVarDeclaration, opt_init_value: Option<BasicValueEnum<'ctx>>, span: Option<Span>) -> Result<IRVarId, CompilerError> {
        if ir_fun_context.vars.iter().any(|v| v.name == var_dec.name) {
            return Err(self.error(SemanticError::CollidingNames { parent_scope: self.format_scope_path(self.ir_function(ir_fun_context.fun).scope), name: var_dec.name.clone() }, span));
        }
        let result: usize = ir_fun_context.vars.len();
        let ptr_value = if var_dec.is_static {
            let global = self.module.add_global(self.ir_type(var_dec.type_id).llvm_type, Some(AddressSpace::default()), format!("[c-flat]:{}", var_dec.name).as_str());
            global.set_constant(!var_dec.is_mut);
            global.set_linkage(Linkage::Internal);
            if let Some(init_value) = opt_init_value {
                global.set_initializer(&init_value);
            } else {
                return Err(self.error(SemanticError::UninitializedStatic, span));
            }
            global.as_pointer_value()
        } else {
            let current_block = self.builder.get_insert_block();
            if let Some(entry_block) = self.ir_function(ir_fun_context.fun).llvm_value.get_first_basic_block() {
                match entry_block.get_first_instruction() {
                    Some(first_instr) => self.builder.position_before(&first_instr),
                    None => self.builder.position_at_end(entry_block),
                }
            }
            let alloca =  self.builder.build_alloca(self.ir_type(var_dec.type_id).llvm_type, &var_dec.name).unwrap();
            if let Some(entry_block) = current_block {
                self.builder.position_at_end(entry_block);
            }
            if let Some(init_value) = opt_init_value {
                self.builder.build_store(alloca, init_value).unwrap();
            }
            alloca
        };
        ir_fun_context.vars.push(IRVariable { name: var_dec.name.clone(), moved: false, is_mut: var_dec.is_mut, place: IRExprPlaceResult { type_id: var_dec.type_id, ptr_value: ptr_value.into(), owner: Some(result), is_mut: var_dec.is_mut }});
        Ok(result)
    }

    pub fn find_fun_def_in_scope(&self, parent_scope: IRScopeId, name: &str, opt_call_span: Option<Span>) -> Result<Option<(IRScopeId, &'ctx Function)>, CompilerError> {
        if let Some(actual_scope) = self.get_name_parent_scope(parent_scope, name, opt_call_span)? {
            let ir_scope = self.ir_scope(actual_scope);
            if let Some(def) = ir_scope.ast_def.unwrap().functions.iter().find(|fun| fun.name == name) {
                return Ok(Some((actual_scope, def)));
            }
        }
        Ok(None)
    }

    fn get_fun(&mut self, parent_scope: IRScopeId, name: &str, templates_map: &IRTemplatesMap) -> Option<IRFunctionId> {
        for (i, opt_fun) in self.funs_table.iter().enumerate() {
            if let Some(fun) = opt_fun && fun.parent_scope == parent_scope && fun.ast_def.name == name && fun.templates_map == *templates_map {
                return Some(i);
            }
        }
        None
    }

    pub fn lower_fun(&mut self, parent_scope: IRScopeId, name: &str, templates_values: &[IRTemplateValue<'ctx>], call_span: Option<Span>) -> Result<IRFunctionId, CompilerError> {
        let (parent_scope, fun_def) = self.find_fun_def_in_scope(parent_scope, name, call_span)?.unwrap();

        let templates_keys = self.get_templates_keys_from(&fun_def.templates)?;
        let templates_map = self.merge_templates_keys_values(&templates_keys, templates_values, call_span)?;
        if let Some(id) = self.get_fun(parent_scope, name, &templates_map) {
            return Ok(id);
        }
        let fun_path_string: String = self.format_child_scope_path(parent_scope, name, &templates_map);
        let fun_id = self.reserve_function_id();

        let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
        new_templates_map.extend(templates_map.clone());

        let fun_scope = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Function(fun_id), templates_map: new_templates_map, ast_def: fun_def.scope.as_ref(), constants: Vec::new() })?;

        let mut ir_context = IRContext::ScopeContext(fun_scope);

        self.ensure_templates_constraints(&mut ir_context, &templates_map, &fun_def.templates, call_span)?;

        let return_type: IRTypeId = if let Some(return_t) = &fun_def.return_type { self.get_type(&mut ir_context, return_t, &IRExprContext::Type)? } else { self.primitive_type(PrimitiveType::Void)? };
        
        let mut args: IRVarDeclarations = Vec::new();
        for arg in fun_def.args.variables.iter() {
            let ir_var_dec: IRVarDeclaration = self.get_ir_var_declaration(&mut ir_context, arg)?;
            args.push(ir_var_dec);
        }
        let args_llvm_types: Vec<BasicMetadataTypeEnum> = args.iter().map(|arg| self.ir_type(arg.type_id).llvm_type.into()).collect();
        let fun_llvm_type = self.ir_type(return_type).llvm_type.fn_type(args_llvm_types.as_slice(), false);

        let has_body = if let Some(_) = &fun_def.scope { true } else { fun_def.external };

        let llvm_fun_name = if fun_def.external || name == "main" { fun_path_string } else { format!("[c-flat]:{}", fun_path_string) };
        let fun_llvm_value = self.module.add_function(llvm_fun_name.as_str(), fun_llvm_type, None);

        let ir_fun = IRFunction {
            parent_scope: parent_scope,
            scope: fun_scope,
            templates_map: templates_map.clone(),
            args: args,
            return_type: return_type,
            llvm_type: fun_llvm_type,
            llvm_value: fun_llvm_value,
            ast_def: fun_def,
            has_body
        };
        self.funs_table[fun_id] = Some(ir_fun);

        if let Some(_) = &fun_def.scope {
            self.lower_fun_scope(fun_id)?;
        }

        Ok(fun_id)
    }

    fn lower_fun_scope(&mut self, fun: IRFunctionId) -> Result<(), CompilerError> { 
        let ir_fun: &IRFunction<'_> = self.ir_function(fun);
        let fun_def = ir_fun.ast_def;
        let fun_name =  self.ir_function(fun).ast_def.name.clone();
        let return_type = ir_fun.return_type;
        let llvm_value: FunctionValue<'_> = ir_fun.llvm_value;
        let entry_block = self.llvm_context.append_basic_block(llvm_value, "entry");

        let original_block = self.builder.get_insert_block();
        self.builder.position_at_end(entry_block);

        let mut ir_fun_context = IRFunContext { fun: fun, vars: Vec::new(), loop_stack: Vec::new() };

        for (i, arg_dec) in ir_fun.args.iter().enumerate() {
            let arg_llvm_value = llvm_value.get_nth_param(i as u32).unwrap();
            self.alloc_var(&mut ir_fun_context, arg_dec, Some(arg_llvm_value), Some(ir_fun.ast_def.args.variables[i].span))?;
        }

        let mut ir_context = IRContext::FunContext(ir_fun_context);
        let result = self.lower_scope(&mut ir_context, fun_def.scope.as_ref().unwrap(), &IRExprContext::Value(Some(return_type)))?;

        if result.type_id != self.primitive_type(PrimitiveType::Never)? {
            if fun_name != "drop" {
                self.drop_vars(&mut ir_context, 0, fun_def.span)?;
            }
            self.builder.build_return(Some(&result.llvm_value)).expect("Return build failed");
        }
        
        if let Some(block) = original_block {
            self.builder.position_at_end(block);
        }
        Ok(())
    }

    pub fn save_vars(&mut self, ir_fun_context: &mut IRFunContext<'ctx>) -> IRVariables<'ctx> {
        ir_fun_context.vars.clone()
    }

    pub fn load_vars(&mut self, ir_fun_context: &mut IRFunContext<'ctx>, vars: IRVariables<'ctx>) {
        ir_fun_context.vars = vars;
    }
}