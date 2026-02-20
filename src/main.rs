mod parser;
mod compiler;
mod errors;
mod database;

use inkwell::context::Context;

use colored::*;
use std::{env, path::Path};
use crate::parser::*;
use crate::compiler::*;

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
    
    let context: Context = Context::create();
    let mut compiler: Compiler<'_> = Compiler::new(&context, &file_context, &main_scope);

    if let Err(err) = compiler.compile() {
        eprintln!("{}", err);
        return;
    }

    println!("{}", "Done!".green());
}