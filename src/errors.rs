use std::fmt;
use colored::*;

use crate::parser::{Span};

#[derive(Debug)]
pub enum CompilerErrorType {
    LinkerError,
    SyntaxError,
    SemanticError
}

#[derive(Debug)]
pub struct CompilerError {
    pub err_type: CompilerErrorType,
    pub msg: String,
    pub description: Option<String>,
    pub file: String,
    pub span: Option<Span>,
    pub line_str: String
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_type_str = match self.err_type {
            CompilerErrorType::LinkerError => "Linker Error:",
            CompilerErrorType::SyntaxError => "Syntax Error:",
            CompilerErrorType::SemanticError => "Semantic Error:"
        };
        write!(f, "{} {} {}", err_type_str.red(), self.msg, format!("({})", self.file).white().dimmed())?;
        if let Some(desc) = self.description.as_ref() {
            write!(f, "\n{} {}", "Description:".white().dimmed(), desc)?;
        }
        if let Some(s) = self.span {
            let pos_str: String = format!("Ln {} Col {}:", s.line_start + 1, s.col_start + 1);
            let before_str = &self.line_str[..s.col_start];
            let span_str = &self.line_str[s.col_start..s.col_end];
            let after_str = &self.line_str[s.col_end..];
            let final_str: String = format!("{}{}{}", before_str, span_str.on_red(), after_str).trim().to_string();
            write!(f, "\n{} {}", pos_str.white().dimmed(), final_str)?;
        }
        Ok(())
    }
}

impl std::error::Error for CompilerError {}