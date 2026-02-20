use inkwell::context::Context;

use crate::{database::Database, parser::{FileContext, Scope}};


pub struct CodeGenerator<'ctx> {
    database: Database<'ctx>,
    file_context: &'ctx FileContext,
}

/*
    ast_scope: Scope,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    types_table: Vec<IRType<'ctx>>,
    funs_table: Vec<IRFunction<'ctx>>,
*/

impl<'ctx> CodeGenerator<'ctx> {
    
    pub fn new(ast_scope: Scope, llvm_context: &'ctx Context, file_context: &'ctx FileContext) -> Compiler<'ctx> {
        let module: Module = context.create_module("main_module");
        let builder: Builder<'_> = context.create_builder();
        let database = Database { ast_scope, llvm_context,  };
        let alloca_builder: Builder<'_> = context.create_builder();
        Compiler { context, file_context, main_scope, module, builder, alloca_builder, type_context: TypesHandler::new(), function_context: FunctionsHandler::new() }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let main_name: IRFunctionName = IRFunctionName { name: "main".to_string(), templates_values: HashMap::new(), scope_name: IRScopeName::Path(Vec::new()) };
        self.build_fun(main_name)?;

        let file_path = Path::new("output.ll");
        self.module.print_to_file(file_path).expect("Failed to write IR");
        self.module.print_to_stderr();
        Ok(())
    }

    pub fn error(&self, msg: &str, description: Option<String>, opt_span: Option<Span>) -> CompilerError {
        if let Some(span) =  opt_span {
            let file: String = self.file_context.get_path(span.file_id);
            let chars: &Vec<char> = &self.file_context.files[&span.file_id];
            let line_end_idx: usize = chars[span.line_index..].iter().position(|&c| c == '\n').map(|pos| span.line_index + pos).unwrap_or(chars.len());
            let line_str: String = chars[span.line_index..line_end_idx].iter().collect();
            CompilerError { err_type: CompilerErrorType::SemanticError, msg: msg.to_string(), description, file, span: Some(span), line_str }
        } else {
            CompilerError { err_type: CompilerErrorType::SemanticError, msg: msg.to_string(), description, file: "".to_string(), span: None, line_str: "".to_string() }
        }
    }
}