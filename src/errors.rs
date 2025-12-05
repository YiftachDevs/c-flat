use std::fmt;

#[derive(Debug)]
pub enum CompilerError {
    SyntaxError(String),
    SemanticError(String),
    LinkerError(String)
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilerError::SyntaxError(err) => write!(f, "Syntax error: {}", err),
            CompilerError::SemanticError(err) => write!(f, "Semantic error: {}", err),
            CompilerError::LinkerError(err) => write!(f, "Linker error: {}", err),
        }
    }
}

impl std::error::Error for CompilerError {}