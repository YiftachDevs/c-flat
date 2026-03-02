use std::fmt;
use colored::*;

use crate::{code_lowerer::{self, CodeLowerer, IRTypeId}, parser::Span};

#[derive(Debug)]
pub enum CompilerErrorType {
    LinkerError(LinkerError),
    SyntaxError(SyntaxError),
    SemanticError(SemanticError)
}

#[derive(Debug)]
pub enum LinkerError {
    ImportFailed(String, String)
}

impl LinkerError {
    fn to_string(&self) -> (&'static str, Option<String>) {
        match self {
            Self::ImportFailed(file_path, err) => ("Import Failed", Some(format!("Failed to import file '{}': {}", file_path, err))) 
        }
    }
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedInfixOpr,
    ExpectedNumeral,
    ExpectedPrimaryExpression,
    UndeducibleType,
    ScriptEndedTooEarly,
    ExpectedLowercaseName,
    ExpectedUppercaseName,
    ExpectedToken(&'static str)
}

impl SyntaxError {
    fn to_string(&self) -> (&'static str, Option<String>) {
        match self {
            Self::ExpectedInfixOpr => ("Expected Infix Operator", None),
            Self::ExpectedNumeral => ("Expected Numeral", None),
            Self::ExpectedPrimaryExpression => ("Expected Primary Expression", None),
            Self::UndeducibleType => ("Undeducible Type", None),
            Self::ScriptEndedTooEarly => ("Script Ended Too Early", None),
            Self::ExpectedLowercaseName => ("Expected Lowercase Name", None),
            Self::ExpectedUppercaseName => ("Expected Uppercase Name", None),
            Self::ExpectedToken(tok) => ("Expected Specific Token", Some(format!("Expected '{}'", tok))),
        }
    }
}

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch { expected: String, got: String },
    ExpectedValueExpr,
    ExpectedTypeExpr,
    UnmatchedArgCount{ expected: usize },
    UnrecognizedName,
    ExpectedName,
    ExpectedStruct,
    ExpectedFunction
}

impl SemanticError {
    fn to_string(&self) -> (&'static str, Option<String>) {
        match self {
            Self::TypeMismatch { expected, got } => ("Type Mismatch", Some(format!("Expected '{}', got '{}'", expected, got))),
            Self::ExpectedValueExpr => ("Expected Value Expression", None),
            Self::ExpectedTypeExpr => ("Expected Type Expression", None),
            Self::UnmatchedArgCount { expected } => ("Unmatched Arg Count", Some(format!("Expected {} args", expected))),
            Self::UnrecognizedName => ("Unrecognized Name", None),
            Self::ExpectedName => ("Expected Name", None),
            Self::ExpectedStruct => ("Expected Struct", None),
            Self::ExpectedFunction => ("Expected Function", None)
        }
    }
}

#[derive(Debug)]
pub struct CompilerError {
    pub err_type: CompilerErrorType,
    pub file: String,
    pub span: Option<Span>,
    pub line_str: String
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (err_type_str, (err_msg, err_description)) = match &self.err_type {
            CompilerErrorType::LinkerError(err) => ("Linker Error:", err.to_string()),
            CompilerErrorType::SyntaxError(err) => ("Syntax Error:", err.to_string()),
            CompilerErrorType::SemanticError(err) => ("Semantic Error:", err.to_string())
        };
        write!(f, "{} {} {}", err_type_str.red(), err_msg, format!("({})", self.file).white().dimmed())?;
        if let Some(desc) = err_description {
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