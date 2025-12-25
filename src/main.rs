mod parser;
mod compiler;
mod errors;

use inkwell::context::Context;

use crate::errors::CompilerError;
use std::{env, path::Path};
use crate::parser::*;
use crate::compiler::*;

fn main() -> Result<(), CompilerError> {
    let str_path: &str = "test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        return Err(CompilerError::LinkerError(format!("Failed to open working directory '{}', {}", str_path, e)));
    }

    let mut parser: Parser = Parser::new();
    let mut main_scope: Scope = Scope::new();

    if let Err(err) = parser.parse_file("main.cf", &mut main_scope) {
        return Err(parser.index_error(err));
    }

    println!("{}", main_scope.to_string());
    
    let context: Context = Context::create();
    let compiler: Compiler<'_> = Compiler::new(&context, &main_scope);

    compiler.compile();

    println!("Done!");
    Ok(())
}