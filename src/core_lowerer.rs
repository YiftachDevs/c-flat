use crate::code_lowerer::PrimitiveType;
use inkwell::{FloatPredicate, IntPredicate, attributes::{Attribute, AttributeLoc}, module::Linkage, types::{BasicMetadataTypeEnum, BasicType}, values::{BasicValueEnum, FunctionValue, PointerValue}};
use crate::{code_lowerer::*, errors::{CompilerError, CompilerErrorType, SemanticError}, expr_lowerer::IRExprResult, parser::{ExprNode, Function, Implementation, InfixOpr, Span, Variable}};


impl<'ctx> CodeLowerer<'ctx> {
    pub fn impl_core_if_primitive(&mut self, type_id: IRTypeId) -> Result<(), CompilerError> {
        let ir_type = self.ir_type(type_id);
        if let IRTypeEnum::Primitive(primitive_type) = ir_type.type_enum {
            if primitive_type == PrimitiveType::Void || primitive_type == PrimitiveType::Never {
                return Ok(());
            }
            let core_scope = self.get_core_scope();
            let numeral_trait = self.lower_trait(core_scope, "Numeral", &Vec::new(), type_id, None)?;
            if self.type_impls_trait(type_id, numeral_trait)? {
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Add)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Sub)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Mul)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Div)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Mod)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Lss)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Leq)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Gtr)?;
                self.build_core_infix_opr_fun_body(type_id, InfixOpr::Geq)?;
            }
            self.build_core_infix_opr_fun_body(type_id, InfixOpr::Eq)?;
            self.build_core_infix_opr_fun_body(type_id, InfixOpr::Neq)?;
        }
        Ok(())
    }

    pub fn get_core_trait_fun(&mut self, type_id: IRTypeId, opr: InfixOpr, span: Option<Span>) -> Result<IRFunctionId, CompilerError> {
        let (trait_name, trait_fun_name) = CodeLowerer::get_infix_opr_trait_name(opr);
        if let Some(impl_id) = self.find_impl_of_core_trait(type_id, trait_name)? && let Some(_) = self.find_impl_of_fun(type_id, trait_fun_name, span)? {
            let ir_impl = self.ir_impl(impl_id);
            let fun = self.lower_fun(ir_impl.scope, trait_fun_name, &Vec::new(), span)?;
            self.ensure_trait_fun_valid(impl_id, fun, span)?;
            Ok(fun)
        } else {
            return Err(self.error(SemanticError::MissingTrait { type_str: self.format_type(type_id), trait_str: trait_name.to_string() }, span));
        }  
    }

    fn get_core_scope(&mut self) -> IRScopeId {
        let global_scope = self.get_global_scope();
        global_scope
    }

    fn find_impl_of_core_trait(&mut self, type_id: IRTypeId, trait_name: &str) -> Result<Option<IRImplId>, CompilerError> {
        self.lower_impls(type_id)?;
        let core_scope = self.get_core_scope();
        for lowered_impl_id in self.ir_type(type_id).lowered_impls.as_ref().unwrap() {
            if let Some(trait_id) = self.ir_impl(*lowered_impl_id).trait_id && self.ir_trait(trait_id).ast_def.name == trait_name && self.ir_trait(trait_id).parent_scope == core_scope {
                return Ok(Some(*lowered_impl_id));
            }
        }
        Ok(None)
    }


    fn get_infix_opr_trait_name(opr: InfixOpr) -> (&'static str, &'static str) {
        match opr {
            InfixOpr::Add => ("Add", "add"),
            InfixOpr::Sub => ("Sub", "sub"),
            InfixOpr::Mul => ("Mul", "mul"),
            InfixOpr::Div => ("Div", "div"),
            InfixOpr::Mod => ("Mod", "mod"),
            InfixOpr::Eq => ("Eq", "eq"),
            InfixOpr::Neq => ("Eq", "neq"),
            InfixOpr::Lss => ("Ord", "lss"),
            InfixOpr::Leq => ("Ord", "leq"),
            InfixOpr::Gtr => ("Ord", "gtr"),
            InfixOpr::Geq => ("Ord", "geq"),
            _ => todo!("get_infix_opr_trait_name")
        }
    }

    fn build_core_infix_opr_fun_body(&mut self, type_id: IRTypeId, opr: InfixOpr) -> Result<(), CompilerError> {
        if let IRTypeEnum::Primitive(primitive_type) = self.ir_type(type_id).type_enum {
            let fun = self.get_core_trait_fun(type_id, opr, None)?;
            let fun_value = self.ir_function(fun).llvm_value;
            let entry_block = self.llvm_context.append_basic_block(fun_value, "entry");
            let original_block = self.builder.get_insert_block();
            self.builder.position_at_end(entry_block);
            let left_arg_value = fun_value.get_nth_param(0).unwrap();
            let right_arg_value = fun_value.get_nth_param(1).unwrap();
            let res = self.build_core_opr(primitive_type, opr, left_arg_value, right_arg_value);
            self.builder.build_return(Some(&res)).unwrap();

            let inline_kind = Attribute::get_named_enum_kind_id("alwaysinline");
            let inline_attr = self.llvm_context.create_enum_attribute(inline_kind, 0);
            fun_value.add_attribute(AttributeLoc::Function, inline_attr);
            fun_value.set_linkage(Linkage::Internal);

            if let Some(block) = original_block {
                self.builder.position_at_end(block);
            }

            self.funs_table[fun].as_mut().unwrap().has_body = true;
        }
        Ok(())
    }

    fn build_core_opr(&mut self, primitive_type: PrimitiveType, opr: InfixOpr, arg_1: BasicValueEnum<'ctx>, arg_2: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match opr {
            InfixOpr::Add => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_add(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_add(arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Sub => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_sub(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_sub(arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Mul => {
                if primitive_type.is_int() || primitive_type.is_uint() {
                    self.builder.build_int_mul(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_mul(arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Div => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_div(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_div(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_div(arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Mod => {
                if primitive_type.is_int() {
                    self.builder.build_int_signed_rem(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    self.builder.build_int_unsigned_rem(arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_rem(arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Eq => {
                if primitive_type.is_int() || primitive_type.is_uint() || primitive_type == PrimitiveType::Bool || primitive_type == PrimitiveType::Char {
                    self.builder.build_int_compare(IntPredicate::EQ, arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_compare(FloatPredicate::OEQ, arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Neq => {
                if primitive_type.is_int() || primitive_type.is_uint() || primitive_type == PrimitiveType::Bool || primitive_type == PrimitiveType::Char {
                    self.builder.build_int_compare(IntPredicate::NE, arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    self.builder.build_float_compare(FloatPredicate::ONE, arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            },
            InfixOpr::Lss | InfixOpr::Leq | InfixOpr::Gtr | InfixOpr::Geq => {
                if primitive_type.is_int() {
                    let predicate = match opr { InfixOpr::Lss => IntPredicate::SLT, InfixOpr::Leq => IntPredicate::SLE, InfixOpr::Gtr => IntPredicate::SGT, InfixOpr::Geq => IntPredicate::SGE, _ => panic!() };
                    self.builder.build_int_compare(predicate, arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_uint() {
                    let predicate = match opr { InfixOpr::Lss => IntPredicate::ULT, InfixOpr::Leq => IntPredicate::ULE, InfixOpr::Gtr => IntPredicate::UGT, InfixOpr::Geq => IntPredicate::UGE, _ => panic!() };
                    self.builder.build_int_compare(predicate, arg_1.into_int_value(), arg_2.into_int_value(), "tmp").unwrap().into()
                } else if primitive_type.is_float() {
                    let predicate = match opr { InfixOpr::Lss => FloatPredicate::OLT, InfixOpr::Leq => FloatPredicate::OLE, InfixOpr::Gtr => FloatPredicate::OGT, InfixOpr::Geq => FloatPredicate::OGE, _ => panic!() };
                    self.builder.build_float_compare(predicate, arg_1.into_float_value(), arg_2.into_float_value(), "tmp").unwrap().into()
                } else {
                    panic!("build_core_opr")
                }
            }
            _ => todo!("build_core_opr")
        }
    }
}