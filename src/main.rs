mod parser;
mod errors;
mod code_lowerer;
mod function_lowerer;
mod expr_lowerer;
mod type_lowerer;
mod impl_lowerer;

use indexmap::IndexMap;
use inkwell::context::Context;

use colored::*;
use std::collections::HashMap;
use std::hash::Hash;
use std::{env, path::Path};
use crate::code_lowerer::{CodeLowerer, IRTemplateKey, IRTemplateValue};
use crate::code_lowerer::IRScope;
use crate::code_lowerer::IRScopePath;
use crate::parser::*;

fn main() {
    let str_path: &str = "src/test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        eprintln!("Failed to open working directory '{}', {}", str_path, e);
        return;
    }

    let mut file_context = FileContext::new();
    let mut parser: Parser = Parser::new(&mut file_context);
    let mut main_scope: Scope = Scope::new();

    if let Err(err) = parser.parse_file("main.cf".to_string(), &mut main_scope) {
        eprintln!("{}", err);
        return;
    }

    println!("{}", main_scope.to_string());
    
    let llvm_context: Context = Context::create();
    let mut code_lowerer = CodeLowerer::new(&main_scope, file_context, &llvm_context);

    let global_scope = code_lowerer.get_global_scope();
    let main_fun_result = code_lowerer.lower_fun(global_scope, "main", IndexMap::new(), None);

    if let Err(err) = main_fun_result {
        eprintln!("{}", err);
        return;
    }

    code_lowerer.export_ir_to_file(Path::new("output.ll"));

    println!("{}", "Done!".green());
}