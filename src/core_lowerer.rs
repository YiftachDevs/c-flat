use std::ops::Add;

use crate::{code_lowerer::PrimitiveType, expr_lowerer::IRExprValueResult, parser::{ExprNodeEnum, PrefixOpr}};
use inkwell::{FloatPredicate, IntPredicate, attributes::{Attribute, AttributeLoc}, module::Linkage, types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Implementation, InfixOpr, Span, Variable}};

#[derive(PartialEq, Eq)]
pub enum CoreTraitFun {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lss,
    Gtr,
    Not,
    Deref,
    Drop,
    AsRef,
    Len,
    MemSize
}

impl CoreTraitFun {
    pub fn get_trait_name(&self) -> &'static str {
        match self {
            Self::Add => "Add",
            Self::Sub => "Sub",
            Self::Mul => "Mul",
            Self::Div => "Div",
            Self::Mod => "Mod",
            Self::Eq => "Eq",
            Self::Lss => "Ord",
            Self::Gtr => "Ord",
            Self::Not => "Not",
            Self::Deref => "Deref",
            Self::Drop => "Drop",
            Self::AsRef => "AsRef",
            _ => panic!()
        }
    }

    pub fn get_fun_name(&self) -> &'static str {
        match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Eq => "eq",
            Self::Lss => "lss",
            Self::Gtr => "gtr",
            Self::Not => "not",
            Self::Deref => "deref",
            Self::Drop => "drop",
            Self::AsRef => "as_ref",
            Self::Len => "len",
            Self::MemSize => "mem_size"
        }
    }
}

impl<'ctx> CodeLowerer<'ctx> {
    pub fn impl_core(&mut self, type_id: IRTypeId) -> Result<(), CompilerError> {
        let core_scope = self.get_core_scope();
        let ir_type = self.ir_type(type_id);
        if let IRTypeEnum::Primitive(primitive_type) = ir_type.type_enum {
            if primitive_type == PrimitiveType::Never {
                return Ok(());
            }
            let primitive_trait = self.lower_trait(core_scope, "Primitive", &Vec::new(), type_id, None)?;
            let numeral_trait = self.lower_trait(core_scope, "Numeral", &Vec::new(), type_id, None)?;
            let integer_trait = self.lower_trait(core_scope, "Integer", &Vec::new(), type_id, None)?;
            if self.type_impls_trait(type_id, numeral_trait)? {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Add)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Sub)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Mul)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Div)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Mod)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Lss)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Gtr)?;
            }
            if self.type_impls_trait(type_id, primitive_trait)? {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Eq)?;
            }
            if self.type_impls_trait(type_id, integer_trait)? || type_id == self.primitive_type(PrimitiveType::Bool)? {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Not)?;
            }
        } else if let IRTypeEnum::Slice { slice_type } = ir_type.type_enum {
            self.build_core_trait_funs(type_id, &CoreTraitFun::AsRef)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::Len)?;
        }
        if !self.is_type_unsized(type_id)? {
            self.build_core_trait_funs(type_id, &CoreTraitFun::MemSize)?;
        }
        Ok(())
    }

    pub fn call_core_trait(&mut self, ir_context: &mut IRContext<'ctx>, result: IRExprResult<'ctx>, args_expr: Option<&Box<ExprNode>>, core_trait: &CoreTraitFun, span: Span) -> Result<IRExprValueResult<'ctx>, CompilerError>  {
        let type_id: usize = result.get_type_id();
        if let Some(trait_impl_id) = self.find_impl_of_core_trait(type_id, core_trait)? {
            let fun = self.lower_impl_fun(trait_impl_id, core_trait.get_fun_name(), &Vec::new(), Some(span))?;
            let args_expr = if let Some(args_expr) = args_expr { args_expr } else { &Box::new(ExprNode { value: ExprNodeEnum::Empty, span }) };
            let result = self.lower_postfix_opr_invoke(ir_context, IRExprResult::Function(fun, Some(Box::new(result))), args_expr, span)?;
            Ok(result)
        } else {
            Err(self.error(SemanticError::MissingTrait { type_str: self.format_type(type_id), trait_str: core_trait.get_trait_name().to_string() }, Some(span)))
        }
    }

    pub fn get_core_scope(&mut self) -> IRScopeId {
        let global_scope = self.get_global_scope();
        global_scope
    }

    pub fn find_impl_of_core_trait(&mut self, type_id: IRTypeId, core_trait: &CoreTraitFun) -> Result<Option<IRImplId>, CompilerError> {
        let core_scope = self.get_core_scope();
        for lowered_impl_id in self.ir_type(type_id).lowered_impls.as_ref().unwrap() {
            if let Some(trait_id) = self.ir_impl(*lowered_impl_id).trait_id && self.ir_trait(trait_id).ast_def.name == core_trait.get_trait_name() && self.ir_trait(trait_id).parent_scope == core_scope {
                return Ok(Some(*lowered_impl_id));
            }
        }
        Ok(None)
    }

    fn build_core_trait_funs(&mut self, type_id: IRTypeId, core_trait: &CoreTraitFun) -> Result<(), CompilerError> {
        let impl_id = self.find_impl_of_fun(type_id, core_trait.get_fun_name(), None)?.unwrap();
        let fun_def = self.ir_impl(impl_id).ast_def.scope.as_ref().unwrap().functions.iter().find(|f| f.name == core_trait.get_fun_name()).unwrap();
        let trait_fun_name = fun_def.name.clone();
        let fun = self.lower_impl_fun(impl_id, &trait_fun_name, &Vec::new(), None)?;
        let fun_value = self.ir_function(fun).llvm_value;
        if !self.ir_function(fun).has_body {
            let entry_block = self.llvm_context.append_basic_block(fun_value, "entry");
            let original_block = self.builder.get_insert_block();
            self.builder.position_at_end(entry_block);
            let result = self.build_core_trait_fun_body(type_id, fun_value, core_trait);
            self.builder.build_return(Some(&result)).unwrap();
            if let Some(block) = original_block {
                self.builder.position_at_end(block);
            }
            self.funs_table[fun].as_mut().unwrap().has_body = true;
        }
        let inline_kind = Attribute::get_named_enum_kind_id("alwaysinline");
        let inline_attr = self.llvm_context.create_enum_attribute(inline_kind, 0);
        fun_value.add_attribute(AttributeLoc::Function, inline_attr);
        fun_value.set_linkage(Linkage::Internal);
        Ok(())
    }

    pub fn build_core_trait_fun_body(&mut self, self_type: IRTypeId, fun_value: FunctionValue<'ctx>, core_trait: &CoreTraitFun) -> BasicValueEnum<'ctx> {
        if core_trait == &CoreTraitFun::MemSize {
            let size = self.get_type_mem_size(self_type);
            return self.llvm_context.i64_type().const_int(size as u64, false).into();
        }
        let self_value = fun_value.get_nth_param(0).unwrap();
        let other_value = fun_value.get_nth_param(1);
        if let IRTypeEnum::Slice { slice_type } = self.ir_type(self_type).type_enum {
            if core_trait == &CoreTraitFun::AsRef {
                let ref_value = self.builder.build_extract_value(self_value.into_struct_value(), 0, "ref").unwrap();
                return ref_value;
            }
            if core_trait == &CoreTraitFun::Len {
                let ref_value = self.builder.build_extract_value(self_value.into_struct_value(), 1, "len").unwrap();
                return ref_value;
            }
        }
        let primitive_type = match self.ir_type(self_type).type_enum { IRTypeEnum::Primitive(prim) => prim, _ => panic!() };
        match core_trait {
            CoreTraitFun::Not => self.builder.build_not(self_value.into_int_value(), "tmp").unwrap().into(),
            CoreTraitFun::Add => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_add(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_add(self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Sub => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_sub(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_sub(self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Mul => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_mul(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_mul(self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Div => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_div(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_div(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_div(self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Mod => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_rem(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_rem(self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_rem(self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Eq => {
                if primitive_type.is_int() || primitive_type.is_uint() || primitive_type == PrimitiveType::Bool || primitive_type == PrimitiveType::Char {
                    self.builder.build_int_compare(IntPredicate::EQ, self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_compare(FloatPredicate::OEQ, self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    self.llvm_context.bool_type().const_all_ones().into()
                }
            },
            CoreTraitFun::Lss | CoreTraitFun::Gtr => {
                if primitive_type.is_int() {
                    let predicate = match core_trait { CoreTraitFun::Lss => IntPredicate::SLT, CoreTraitFun::Gtr => IntPredicate::SGT, _ => panic!() };
                    self.builder.build_int_compare(predicate, self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    let predicate = match core_trait { CoreTraitFun::Lss => IntPredicate::ULT, CoreTraitFun::Gtr => IntPredicate::UGT, _ => panic!() };
                    self.builder.build_int_compare(predicate, self_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    let predicate = match core_trait { CoreTraitFun::Lss => FloatPredicate::OLT, CoreTraitFun::Gtr => FloatPredicate::OGT, _ => panic!() };
                    self.builder.build_float_compare(predicate, self_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            _ => panic!()
        }
    }
}