use std::ops::Add;

use crate::{code_lowerer::PrimitiveType, expr_lowerer::{IRExprPlaceResult, IRExprValueResult}, parser::{ExprNodeEnum, PostfixOpr, PrefixOpr}};
use inkwell::{FloatPredicate, IntPredicate, attributes::{Attribute, AttributeLoc}, module::Linkage, types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Implementation, InfixOpr, Span, Variable}};

#[derive(PartialEq, Eq)]
pub enum CoreTraitFun {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Eq,
    Lss,
    Gtr,
    Not,
    Or,
    And,
    Xor,
    Deref,
    DerefMut,
    Drop,
    AsRef,
    Len,
    MemSize,
    Index,
    IndexMut,
    FromRawParts,
    Copy,
    Next,
    HasNext,
    UpTo,
    Shl,
    Shr
}

impl CoreTraitFun {
    pub fn get_trait_name(&self) -> &'static str {
        match self {
            Self::Add => "Add",
            Self::Sub => "Sub",
            Self::Mul => "Mul",
            Self::Div => "Div",
            Self::Mod => "Mod",
            Self::Neg => "Neg",
            Self::Eq => "Eq",
            Self::Lss => "Ord",
            Self::Gtr => "Ord",
            Self::Not => "Bitwise",
            Self::Or => "Bitwise",
            Self::And => "Bitwise",
            Self::Xor => "Bitwise",
            Self::Deref => "Deref",
            Self::DerefMut => "Deref",
            Self::Drop => "Drop",
            Self::AsRef => "AsRef",
            Self::Index => "Index",
            Self::IndexMut => "Index",
            Self::Copy => "Copy",
            Self::Next => "Iter",
            Self::HasNext => "Iter",
            Self::UpTo => "Up_To",
            Self::Shl => "Bitshift",
            Self::Shr => "Bitshift",
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
            Self::Neg => "neg",
            Self::Eq => "eq",
            Self::Lss => "lss",
            Self::Gtr => "gtr",
            Self::Not => "not",
            Self::And => "and",
            Self::Xor => "xor",
            Self::Or => "or",
            Self::Deref => "deref",
            Self::DerefMut => "deref_mut",
            Self::Drop => "drop",
            Self::AsRef => "as_ref",
            Self::Len => "len",
            Self::MemSize => "mem_size",
            Self::Index => "index",
            Self::IndexMut => "index_mut",
            Self::FromRawParts => "from_raw_parts",
            Self::Next => "next",
            Self::HasNext => "has_next",
            Self::UpTo => "up_to",
            Self::Shl => "shl",
            Self::Shr => "shr",
            _ => panic!()
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
            let numeral_trait = self.lower_trait(core_scope, "Numeral", &Vec::new(), type_id, None)?;
            let integer_trait = self.lower_trait(core_scope, "Integer", &Vec::new(), type_id, None)?;
            let is_integer = self.type_impls_trait(type_id, integer_trait)?;
            if self.type_impls_trait(type_id, numeral_trait)? {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Add)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Sub)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Mul)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Div)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Neg)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Mod)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Lss)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Gtr)?;
            }
            if is_integer || type_id == self.primitive_type(PrimitiveType::Bool)? {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Not)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Or)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::And)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Xor)?;
            }
            if is_integer {
                self.build_core_trait_funs(type_id, &CoreTraitFun::Shl)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::Shr)?;
                self.build_core_trait_funs(type_id, &CoreTraitFun::UpTo)?;
            }
        } else if let IRTypeEnum::Slice { slice_type } = ir_type.type_enum {
            self.build_core_trait_funs(type_id, &CoreTraitFun::AsRef)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::Len)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::Index)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::IndexMut)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::FromRawParts)?;
        } else if let IRTypeEnum::Array { arr_type, size } = ir_type.type_enum {
            self.build_core_trait_funs(type_id, &CoreTraitFun::Index)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::IndexMut)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::Deref)?;
            self.build_core_trait_funs(type_id, &CoreTraitFun::DerefMut)?;
        } else if let IRTypeEnum::Struct(_) = ir_type.type_enum {
            // self.build_core_trait_funs(type_id, &CoreTraitFun::Drop)?;
        }
        self.build_core_trait_funs(type_id, &CoreTraitFun::Drop)?;
        self.build_core_trait_funs(type_id, &CoreTraitFun::Eq)?;
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
            for impl_id in self.ir_type(type_id).lowered_impls.as_ref().unwrap() {
                if let Some(t) = self.ir_impl(*impl_id).trait_id {
                    println!("!{}", self.format_type(self.ir_trait(t).scope));
                }
                println!("!{}", self.format_type(self.ir_impl(*impl_id).scope));
            }
            if let Some(_) = self.ir_type(type_id).lowered_impls {
                println!("!");
            }
            Err(self.error(SemanticError::MissingTrait { type_str: self.format_type(type_id), trait_str: core_trait.get_trait_name().to_string() }, Some(span)))
        }
    }

    pub fn get_core_scope(&mut self) -> IRScopeId {
        let global_scope = self.get_global_scope();
        global_scope
    }

    pub fn find_impl_of_core_trait(&mut self, type_id: IRTypeId, core_trait: &CoreTraitFun) -> Result<Option<IRImplId>, CompilerError> {
        self.lower_impls(type_id)?;
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
        if let Some(_) = fun_def.scope {
            return Ok(());
        }
        let fun = self.lower_impl_fun(impl_id, &trait_fun_name, &Vec::new(), None)?;
        
        let fun_value = self.ir_function(fun).llvm_value;
        if !self.ir_function(fun).has_body {
            let entry_block = self.llvm_context.append_basic_block(fun_value, "entry");
            let original_block = self.builder.get_insert_block();
            self.builder.position_at_end(entry_block);
            let mut ir_fun_context = IRFunContext { fun: fun, vars: Vec::new(), loop_stack: Vec::new() };
            for (i, arg_dec) in self.ir_function(fun).args.iter().enumerate() {
                let arg_llvm_value = fun_value.get_nth_param(i as u32).unwrap();
                self.alloc_var(&mut ir_fun_context, arg_dec, Some(arg_llvm_value), Some(self.ir_function(fun).ast_def.args.variables[i].span))?;
            }
            let first_value = fun_value.get_nth_param(0);
            let other_value = fun_value.get_nth_param(1);
            let result = self.build_core_trait_fun_body(&mut IRContext::FunContext(ir_fun_context), type_id, first_value, other_value, core_trait)?;
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

    pub fn build_core_trait_fun_body(&mut self, ir_context: &mut IRContext<'ctx>, self_type: IRTypeId, first_value: Option<BasicValueEnum<'ctx>>, other_value: Option<BasicValueEnum<'ctx>>, core_trait: &CoreTraitFun) -> Result<BasicValueEnum<'ctx>, CompilerError> {
        if core_trait == &CoreTraitFun::MemSize {
            let size = self.get_type_mem_size(self_type);
            return Ok(self.llvm_context.i64_type().const_int(size as u64, false).into());
        }
        let first_value = first_value.unwrap();
        if let IRTypeEnum::Slice { slice_type } = self.ir_type(self_type).type_enum {
            if core_trait == &CoreTraitFun::FromRawParts {
                let unsized_ref_type = self.unsized_ref_type(self_type, true)?;
                let mut llvm_value = self.ir_type(unsized_ref_type).llvm_type.into_struct_type().get_undef().into();
                llvm_value = self.builder.build_insert_value(llvm_value, first_value, 0, "ptr").unwrap();
                llvm_value = self.builder.build_insert_value(llvm_value, other_value.unwrap(), 1, "len").unwrap();
                return Ok(llvm_value.as_basic_value_enum());
            }
            if core_trait == &CoreTraitFun::AsRef {
                let ref_value = self.builder.build_extract_value(first_value.into_struct_value(), 0, "ref").unwrap();
                return Ok(ref_value);
            }
            if core_trait == &CoreTraitFun::Len {
                let len_value = self.builder.build_extract_value(first_value.into_struct_value(), 1, "len").unwrap();
                return Ok(len_value);
            }
            if core_trait == &CoreTraitFun::Index || core_trait == &CoreTraitFun::IndexMut {
                let ref_value = self.builder.build_extract_value(first_value.into_struct_value(), 0, "ref").unwrap();
                let index_res = unsafe { self.builder.build_gep(self.ir_type(slice_type).llvm_type, ref_value.into_pointer_value(), &[other_value.unwrap().into_int_value()], "tmp_index").unwrap() };
                return Ok(index_res.into());
            }
        } else if let IRTypeEnum::Array { arr_type, size } = self.ir_type(self_type).type_enum {
            if core_trait == &CoreTraitFun::Index || core_trait == &CoreTraitFun::IndexMut {
                let index_res = unsafe { self.builder.build_gep(self.ir_type(arr_type).llvm_type, first_value.into_pointer_value(), &[other_value.unwrap().into_int_value()], "tmp_index").unwrap() };
                return Ok(index_res.into());
            }
            if core_trait == &CoreTraitFun::Deref || core_trait == &CoreTraitFun::DerefMut {
                let slice_type = self.slice_type(arr_type)?;
                let unsized_ref_type = self.unsized_ref_type(slice_type, true)?;
                let mut llvm_value = self.ir_type(unsized_ref_type).llvm_type.into_struct_type().get_undef().into();
                llvm_value = self.builder.build_insert_value(llvm_value, first_value, 0, "ptr").unwrap();
                llvm_value = self.builder.build_insert_value(llvm_value, self.llvm_context.i64_type().const_int(size, false), 1, "len").unwrap();
                return Ok(llvm_value.as_basic_value_enum());
            }
        } else if let IRTypeEnum::Struct(ir_struct) = self.ir_type(self_type).type_enum.clone() {
            if core_trait == &CoreTraitFun::Drop {
                for (i, var_dec) in ir_struct.args.iter().enumerate().rev() {
                    let arg = self.builder.build_extract_value(first_value.into_struct_value(), i as u32, "tmp").unwrap();
                    self.call_core_trait(ir_context, IRExprResult::Value(IRExprValueResult { type_id: var_dec.type_id, llvm_value: arg }),None, &CoreTraitFun::Drop, Span::dummy())?;
                }
            }
            if core_trait == &CoreTraitFun::Eq {
                let bool_type = self.primitive_type(PrimitiveType::Bool)?;
                let mut result = self.ir_type(bool_type).llvm_type.into_int_type().const_all_ones().as_basic_value_enum();
                for (i, var_dec) in ir_struct.args.iter().enumerate().rev() {
                    let arg_ptr_value = self.builder.build_struct_gep(self.ir_type(self_type).llvm_type, first_value.into_pointer_value(), i as u32, "tmp").unwrap().into();
                    let other_expr_node = &Box::new(ExprNode { span: Span::dummy(), value: ExprNodeEnum::PrefixOpr(PrefixOpr::Addr { is_mut: false },
                        Box::new(ExprNode { span: Span::dummy(), value: ExprNodeEnum::PostfixOpr(PostfixOpr::Mem,
                            Box::new(ExprNode{ value: ExprNodeEnum::Name("other".to_string()), span: Span::dummy()}),
                            Box::new(ExprNode{ value: ExprNodeEnum::Name(var_dec.name.clone()), span: Span::dummy()}))}))});
                    let eq_res = self.call_core_trait(ir_context, IRExprResult::Place(IRExprPlaceResult { type_id: var_dec.type_id, ptr_value: arg_ptr_value, is_mut: false, owner: None }),Some(other_expr_node), &CoreTraitFun::Eq, Span::dummy())?;
                    result = self.builder.build_and(result.into_int_value(), eq_res.llvm_value.into_int_value(), "tmp").unwrap().into();
                }
                return Ok(result);
            }
        }
        if core_trait == &CoreTraitFun::Drop {
            let void_type = self.primitive_type(PrimitiveType::Void)?;
            return Ok(self.get_type_zero(void_type).llvm_value)
        }

        let primitive_type = match self.ir_type(self_type).type_enum { IRTypeEnum::Primitive(prim) => prim, _ => panic!() };
        Ok(match core_trait {
            CoreTraitFun::Shr => {
                if primitive_type.is_int() {
                    self.builder.build_right_shift(first_value.into_int_value(), other_value.unwrap().into_int_value(), true, "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_right_shift(first_value.into_int_value(), other_value.unwrap().into_int_value(), false, "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Shl => {
                self.builder.build_left_shift(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
            }
            CoreTraitFun::UpTo => {
                let global_scope = self.get_global_scope();
                let struct_id = self.lower_struct(global_scope, "Range", &[IRTemplateValue::Type(self_type)], None)?;
                self.range_value(struct_id, first_value.into_int_value(), other_value.unwrap().into_int_value())?
            },
            CoreTraitFun::Not => self.builder.build_not(first_value.into_int_value(), "tmp").unwrap().into(),
            CoreTraitFun::And => self.builder.build_and(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into(),
            CoreTraitFun::Or => self.builder.build_or(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into(),
            CoreTraitFun::Xor => self.builder.build_xor(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into(),
            CoreTraitFun::Neg => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_neg(first_value.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_neg(first_value.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Add => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_add(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_add(first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Sub => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_sub(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_sub(first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Mul => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_mul(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_mul(first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Div => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_div(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_div(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_div(first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Mod => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_rem(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_rem(first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_rem(first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            CoreTraitFun::Eq => {
                let ptr_llvm_type = self.ir_type(self_type).llvm_type;
                let first_value = self.builder.build_load(ptr_llvm_type, first_value.into_pointer_value(), "tmp").unwrap();
                let other_value = self.builder.build_load(ptr_llvm_type, other_value.unwrap().into_pointer_value(), "tmp").unwrap();
                if primitive_type.is_int() || primitive_type.is_uint() || primitive_type == PrimitiveType::Bool || primitive_type == PrimitiveType::Char {
                    self.builder.build_int_compare(IntPredicate::EQ, first_value.into_int_value(), other_value.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_compare(FloatPredicate::OEQ, first_value.into_float_value(), other_value.into_float_value(), "tmp").unwrap().into()
                } else {
                    self.llvm_context.bool_type().const_all_ones().into()
                }
            },
            CoreTraitFun::Lss | CoreTraitFun::Gtr => {
                if primitive_type.is_int() {
                    let predicate = match core_trait { CoreTraitFun::Lss => IntPredicate::SLT, CoreTraitFun::Gtr => IntPredicate::SGT, _ => panic!() };
                    self.builder.build_int_compare(predicate, first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    let predicate = match core_trait { CoreTraitFun::Lss => IntPredicate::ULT, CoreTraitFun::Gtr => IntPredicate::UGT, _ => panic!() };
                    self.builder.build_int_compare(predicate, first_value.into_int_value(), other_value.unwrap().into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    let predicate = match core_trait { CoreTraitFun::Lss => FloatPredicate::OLT, CoreTraitFun::Gtr => FloatPredicate::OGT, _ => panic!() };
                    self.builder.build_float_compare(predicate, first_value.into_float_value(), other_value.unwrap().into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            }
            _ => panic!()
        })
    }
}