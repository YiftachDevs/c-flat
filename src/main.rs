mod parser;
mod compiler;
mod errors;

use inkwell::context::Context;

use crate::errors::CompilerError;
use std::{env, path::Path};
use crate::parser::*;
use crate::compiler::*;

fn main() -> Result<(), CompilerError> {
    let str_path: &str = "src/test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        return Err(CompilerError::LinkerError(format!("Failed to open working directory '{}', {}", str_path, e)));
    }

    let mut file_context = FileContext::new();
    let mut parser: Parser = Parser::new(&mut file_context);
    let mut main_scope: Scope = Scope::new();

    if let Err(err) = parser.parse_file("main.cf".to_string(), &mut main_scope) {
        return Err(parser.index_error(err));
    }

    println!("{}", main_scope.to_string());
    
    let context: Context = Context::create();
    let mut compiler: Compiler<'_> = Compiler::new(&context, &file_context, &main_scope);

    compiler.compile()?;

    println!("Done!");
    Ok(())
}