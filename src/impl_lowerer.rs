
use indexmap::IndexMap;
use inkwell::{attributes::{Attribute, AttributeLoc}, module::Linkage, types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Implementation, InfixOpr, Span, Variable}};

impl<'ctx> CodeLowerer<'ctx> {
    pub fn find_impl_of_fun(&mut self, type_id: IRTypeId, fun_name: &str, call_span: Option<Span>) -> Result<Option<IRImplId>, CompilerError> {
        self.lower_impls(type_id)?;
        let mut found_impls_ids = Vec::new();
        for lowered_impl_id in self.ir_type(type_id).lowered_impls.as_ref().unwrap() {
            let ir_impl = self.ir_impl(*lowered_impl_id);
            if let Some(scope) = &ir_impl.ast_def.scope && let Some(_) = scope.functions.iter().find(|fun| fun.name == fun_name) {
                found_impls_ids.push(*lowered_impl_id);
            }
        }
        
        match found_impls_ids.len() {
            2.. => Err(self.error(SemanticError::CollidingImpl { type_str: self.format_type(type_id), fun_name: fun_name.to_string() }, call_span)),
            1 => {
                Ok(Some(found_impls_ids[0]))
            },
            0 => Ok(None)
        }
    }
    
    pub fn find_impls(&mut self, parent_scope: IRScopeId, type_id: IRTypeId) -> Result<Vec<IRImplId>, CompilerError> {
        let mut impls_ids = Vec::new();
        for impl_def in self.ir_scope(parent_scope).ast_def.unwrap().implementations.iter() {
            let impl_templates_keys = self.get_templates_keys_from(&impl_def.templates)?;
            if let Some(templates_map) = self.match_impl_type(parent_scope, impl_templates_keys, &impl_def.target_type, type_id)? {
                let mut new_templates_map = self.ir_scope(parent_scope).templates_map.clone();
                new_templates_map.insert("Self".to_string(), type_id );
                new_templates_map.extend(templates_map.clone());

                let impl_id = self.impls_table.len();
                let scope_id = self.scope_id(IRScope { parent_scope: Some(parent_scope), path: IRScopePath::Impl(impl_id), templates_map: new_templates_map, ast_def: impl_def.scope.as_ref() });
                let mut ir_context = IRContext::ScopeContext(scope_id);

                let opt_trait_id = if let Some(trait_expr) = &impl_def.opt_trait {
                    let trait_id = self.get_trait(&mut ir_context, trait_expr, &IRContextType::Type(type_id))?;
                    Some(trait_id)
                } else { None };

                let ir_impl = IRImpl { scope: scope_id, type_id, trait_id: opt_trait_id, templates_map: templates_map, ast_def: impl_def };
                self.impls_table.push(ir_impl);

                impls_ids.push(impl_id);
            }
        }
        for module in self.ir_scope(parent_scope).ast_def.unwrap().modules.iter() {
            let scope = self.get_module_scope_in_scope(parent_scope, &module.name).unwrap();
            impls_ids.extend(self.find_impls(scope, type_id)?);
        }
        Ok(impls_ids)
    }

    pub fn lower_impls(&mut self, type_id: IRTypeId) -> Result<(), CompilerError> {
        if let Some(_) = self.types_table[type_id].lowered_impls {
            return Ok(());
        }
        let global_scope = self.get_global_scope();
        self.types_table[type_id].lowered_impls = Some(Vec::new());
        self.types_table[type_id].lowered_impls = Some(self.find_impls(global_scope, type_id)?);
        self.filter_impls_constraints(type_id)?;
        self.ensure_valid_traits_implementations(type_id)?;
        self.impl_core_if_primitive(type_id)?;
        Ok(())
    }

    fn filter_impls_constraints(&mut self, type_id: IRTypeId) -> Result<(), CompilerError> {
        loop {
            let mut idx = None;
            let impls = self.types_table[type_id].lowered_impls.as_ref().unwrap().clone();
            for (i, impl_id) in impls.iter().enumerate() {
                let ir_impl = self.ir_impl(*impl_id);
                let mut ir_context = IRContext::ScopeContext(ir_impl.scope);
                if let Err(err) = self.ensure_templates_constraints(&mut ir_context, &ir_impl.templates_map.clone(), &ir_impl.ast_def.templates, None) {
                    if let CompilerErrorType::SemanticError(SemanticError::InvalidTemplateValue { key, type_str, templates_str }) = err.err_type {
                        idx = Some(i);
                        break;
                    } else {
                        return Err(err);
                    }
                }
            }
            if let Some(i) = idx {
                self.types_table[type_id].lowered_impls.as_mut().unwrap().remove(i);
            } else {
                break;
            }
        }
        Ok(())
    }

    fn ensure_valid_traits_implementations(&mut self, type_id: IRTypeId) -> Result<(), CompilerError> {
        let impls = self.types_table[type_id].lowered_impls.as_ref().unwrap().clone();
        for impl_id in impls.iter() {
            let ir_impl = self.ir_impl(*impl_id);
            if let Some(trait_id) = ir_impl.trait_id {
                let ir_trait = self.ir_trait(trait_id);
                if let Some(scope) = &ir_trait.ast_def.scope {
                    let impl_scope = self.ir_scope(ir_impl.scope).ast_def;
                    for trait_fun in scope.functions.iter() {
                        if impl_scope == None || !impl_scope.unwrap().functions.iter().any(|def| def.name == trait_fun.name) {
                            return Err(self.error(SemanticError::MissingImpl { type_str: self.format_type(type_id), trait_str: self.format_scope_path(ir_trait.scope), fun_name: trait_fun.name.clone() }, None));
                        }
                    }
                }
                if let Some(scope) = &ir_impl.ast_def.scope {
                    let trait_scope = self.ir_scope(ir_trait.scope).ast_def;
                    for fun in scope.functions.iter() {
                        if trait_scope == None || !trait_scope.unwrap().functions.iter().any(|def| def.name == fun.name) {
                            return Err(self.error(SemanticError::NonExistingImpl { type_str: self.format_type(type_id), trait_str: self.format_scope_path(ir_trait.scope), fun_name: fun.name.clone() }, Some(fun.span)));
                        }
                    }
                }
                for sub_trait in ir_trait.sub_traits.iter() {
                    if !self.type_impls_trait(type_id, *sub_trait)? {
                        return Err(self.error(SemanticError::MissingTrait { type_str: self.format_type(type_id), trait_str: self.format_scope_path(self.ir_trait(*sub_trait).scope) }, None));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn ensure_trait_fun_valid(&mut self, impl_id: IRImplId, fun_id: IRFunctionId, call_span: Option<Span>) -> Result<(), CompilerError> {
        if let Some(trait_id) = self.ir_impl(impl_id).trait_id {
            let templates_values = self.ir_function(fun_id).templates_map.iter().map(|(_, value)| *value).collect::<Vec<IRTypeId>>();
            let trait_fun_result = self.lower_fun(self.ir_trait(trait_id).scope, &self.ir_function(fun_id).ast_def.name, &templates_values, call_span);
            let err = self.error(SemanticError::IncorrectImpl { type_str: self.format_type(self.ir_impl(impl_id).type_id), trait_str: self.format_scope_path(self.ir_trait(trait_id).scope), fun_name: self.ir_function(fun_id).ast_def.name.clone() }, Some(self.ir_function(fun_id).ast_def.span));
            if let Err(_) = trait_fun_result {
                return Err(err);
            }
            let impl_fun = self.ir_function(fun_id);
            let trait_fun = self.ir_function(trait_fun_result.unwrap());
            if impl_fun.args.len() != trait_fun.args.len() { return Err(err); }
            for i in 0..trait_fun.args.len() {
                let trait_arg = &trait_fun.args[i];
                let impl_arg = &impl_fun.args[i];
                if trait_arg.type_id != impl_arg.type_id { return Err(err); }
            }
            if impl_fun.return_type != trait_fun.return_type { return Err(err);}
        }
        Ok(())
    }

    pub fn match_impl_type(&mut self, parent_scope: IRScopeId, templates_keys: Vec<IRTemplateKey>, expr: &ExprNode, target_type: IRTypeId) -> Result<Option<IRTemplatesMap>, CompilerError> {
        let mut ir_context = IRContext::ImplDefContext(parent_scope, templates_keys, IndexMap::new());
        let expr_result = self.lower_expr(&mut ir_context, expr, &IRContextType::Impl(target_type))?;
        if let IRExprResult::Type(result_type) = expr_result && result_type == target_type && let IRContext::ImplDefContext(_, _, map) = ir_context {
            return Ok(Some(map));
        }
        Ok(None)
    }

    pub fn type_impls_trait(&self, type_id: IRTypeId, trait_id: IRTraitId) -> Result<bool, CompilerError> {
        for impl_id in self.ir_type(type_id).lowered_impls.as_ref().unwrap() {
            if let Some(cur_trait) = self.ir_impl(*impl_id).trait_id && cur_trait == trait_id {
                return Ok(true);
            }
        }
        Ok(false)
    }
}