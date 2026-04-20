use std::{collections::HashMap, env::var, fmt::format, fs::{self, File}};
use crate::errors::*;

#[derive(PartialEq, Clone)]
pub struct Templates {
    pub templates: Vec<Template>
}

impl Templates {
    pub fn is_from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut templates: Vec<Template> = Vec::new();
        if !parser.is_next("<") { return Ok(Templates { templates }); }
        loop {
            templates.push(Template::from_def(parser)?);
            if parser.is_next(">") { break; }
            parser.ensure_next(",")?;
        }
        return Ok(Templates { templates });   
    }
}

impl ToString for Templates {
    fn to_string(&self) -> String {
        if self.templates.is_empty() { return "".to_string(); }
        let str_vec: Vec<String> = self.templates.iter().map(|v| v.to_string()).collect();
        return format!("<{}>", str_vec.join(", "));
    }
}

#[derive(PartialEq, Clone)]
pub enum Template {
    Const(Const),
    VarType(String, Option<ExprNode>)
}

impl Template {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if let Some(const_value) = Const::is_from_def(parser)? {
            return Ok(Self::Const(const_value));
        }
        let key = parser.next_name()?;
        let mut contraints = None;
        if parser.is_next(":") {
            contraints = Some(ExprNode::primary_from_def(parser)?);
        }
        return Ok(Template::VarType(key, contraints));
    }
}

impl ToString for Template {
    fn to_string(&self) -> String {
        match self {
            Template::Const(const_value) => {
                const_value.to_string()
            }
            Template::VarType(name, opt_contraints) => {
                if let Some(constraints) = opt_contraints {
                    format!("{}: {}", name, constraints.to_string())
                } else {
                    name.clone()
                }
            }
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Literal {
    UnresolvedInteger(u64),
    Int(u64),
    Size(u64),
    Bool(bool),
    Char(char),
    Float(f64),
    String(String),
    Void
}

#[derive(PartialEq, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Option<ExprNode>,
    pub init_expr: Option<ExprNode>,
    pub is_mut: bool,
    pub is_static: bool,
    pub span: Span
}

#[derive(PartialEq, Clone)]
pub struct Const {
    pub name: String,
    pub var_type: ExprNode,
    pub init_expr: Option<ExprNode>,
    pub span: Span
}

#[derive(PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub templates: Templates,
    pub args: Variables,
    pub return_type: Option<ExprNode>,
    pub scope: Option<Scope>,
    pub span: Span,
    pub external: bool
}

#[derive(PartialEq, Clone)]
pub enum Statement {
    Expression{ expr: ExprNode, is_final_value: bool},
    VarDeclaration(Variable),
    Const(Const),
    ConditionalChain(ConditionalChain),
    ControlFlow(ControlFlow)
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum InfixOpr {
    As,
    Is,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Lss,
    Leq,
    Gtr,
    Geq,
    Eq,
    Neq,
    And,
    Xor,
    Or,
    LAnd,
    LOr,
    Asn,
    AsnAdd,
    AsnSub,
    AsnMul,
    AsnDiv,
    AsnMod,
    Range,
    Com
}

impl InfixOpr {
    pub fn is_boolean(&self) -> bool {
        match self {
            InfixOpr::Eq | InfixOpr::Neq | InfixOpr::Lss | InfixOpr::Leq | InfixOpr::Gtr | InfixOpr::Geq | InfixOpr::Add | InfixOpr::Xor | InfixOpr::Or => true,
            _ => false
        }
    }

    pub fn is_shift(&self) -> bool {
        match self {
            InfixOpr::Shl | InfixOpr::Shr => true,
            _ => false
        }
    }
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum PrefixOpr {
    Not,
    Addr { is_mut: bool },
    Deref,
    UMin
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub enum PostfixOpr {
    Inv,
    Idx,
    Mem, // vec.x
    Sep, // Vec:len(&vec)
    Con, // eg: Vec_2 { 4, 3 }
    Tmp // (template eg <>)
}

#[derive(Eq, PartialEq, Clone, Hash)]
pub enum Conditional {
    If,
    For,
    While,
    Loop,
    When,
    Else
}

#[derive(PartialEq, Clone)]
pub struct ConditionalChain {
    pub kind: Conditional,
    pub label: Option<Label>,
    pub iter_name: Option<String>,
    pub cond_expr: Option<ExprNode>,
    pub then_scope: Scope,
    pub else_node: Option<Box<ConditionalChain>>,
    pub span: Span
}

impl ToString for ConditionalChain {
    fn to_string(&self) -> String {
        let kind_str = match self.kind {
            Conditional::If => "if",
            Conditional::For => "for",
            Conditional::While => "while",
            Conditional::Loop => "loop",
            Conditional::When => "when",
            Conditional::Else => ""
        };
        let mut result = kind_str.to_string();
        if let Some(cond_expr) = self.cond_expr.as_ref() {
            result += format!(" {} ", cond_expr.to_string()).as_str();
        }
        result += self.then_scope.to_string().as_str();
        if let Some(else_node) = self.else_node.as_ref() {
            result += format!(" else {}", else_node.to_string()).as_str();
        }
        result
    }
}

#[derive(PartialEq, Clone)]
pub enum ControlFlow {
    Return(ExprNode),
    Skip { label: Option<Label>, span: Span },
    Break { label: Option<Label>, expr: ExprNode, span: Span }
}

#[derive(PartialEq, Clone)]
pub struct Label {
    pub label: String,
    pub span: Span
}

impl Label {
    fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let mut span = parser.get_span_start()?;
        if !parser.is_next("@") {
            return Ok(None);
        }
        parser.end_span(&mut span);
        Ok(Some(Self { label: parser.next_name()?, span }))
    }

    fn to_string(&self) -> String {
        format!("@{}", self.label)
    }
}

impl ToString for ControlFlow {
    fn to_string(&self) -> String {
        match self {
            ControlFlow::Return(expr) => {
                let mut result: String = "return".to_string();
                result += format!(" {}", expr.to_string()).as_str();
                result
            }
            ControlFlow::Break { label, expr, span } => {
                let mut result: String = "break".to_string();
                if let Some(label) = label {
                    result += format!(" {}", label.to_string()).as_str();
                }
                result += format!(" {}", expr.to_string()).as_str();
                result
            }
            ControlFlow::Skip { label, span } => {
                let mut result = "skip".to_string();
                if let Some(label) = label {
                    result += format!(" {}", label.to_string()).as_str();
                }
                result
            }
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Scope {
    pub variables: Vec<Variable>,
    pub statements: Vec<Statement>,
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub type_defs: Vec<TypeDef>,
    pub modules: Vec<Module>,
    pub implementations: Vec<Implementation>,
    pub traits: Vec<Trait>,
    pub return_type: Option<ExprNode>,
    pub span: Span
}

#[derive(PartialEq, Clone)]
pub enum ExprNodeEnum {
    Empty,
    Brackets(Box<ExprNode>),
    Void,
    InfixOpr(InfixOpr, Box<ExprNode>, Box<ExprNode>),
    PrefixOpr(PrefixOpr, Box<ExprNode>),
    PostfixOpr(PostfixOpr, Box<ExprNode>, Box<ExprNode>),
    Name(String),
    Literal(Literal),
    VarDeclaration(Box<Variable>),
    Scope(Box<Scope>),
    ConditionalChain(Box<ConditionalChain>),
    Array(Box<ExprNode>, Option<Box<ExprNode>>) // [1, 2, 3] or [4; 5]
}

#[derive(PartialEq, Clone)]
pub struct ExprNode {
    pub value: ExprNodeEnum,
    pub span: Span
}

#[derive(PartialEq, Clone)]
pub struct Module {
    pub name: String,
    pub scope: Scope
}

#[derive(PartialEq, Clone)]
pub struct Struct {
    pub name: String,
    pub templates: Templates,
    pub vars: Variables
}

#[derive(PartialEq, Clone)]
pub struct Enum {
    pub name: String,
    pub templates: Templates,
    pub structs: Structs
}

#[derive(PartialEq, Clone)]
pub struct TypeDef {
    pub name: String,
    pub expr: ExprNode
}

#[derive(PartialEq, Clone)]
pub struct Implementation {
    pub templates: Templates,
    pub opt_trait: Option<ExprNode>,
    pub target_type: ExprNode,
    pub scope: Option<Scope>
}

#[derive(PartialEq, Clone)]
pub struct Trait {
    pub name: String,
    pub templates: Templates,
    pub sub_traits: Option<Box<ExprNode>>,
    pub scope: Option<Scope>
}

impl ConditionalChain {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let mut span = parser.get_span_start()?;
        let label = Label::is_from_def(parser)?;
        let kind: Conditional = if parser.is_next("if ") {
            Conditional::If
        } else if parser.is_next("for ") {
            Conditional::For
        } else if parser.is_next("while ") {
            Conditional::While
        } else if parser.is_next("loop ") {
            Conditional::Loop
        } else if parser.is_next("when ") {
            Conditional::When
        } else {
            return Ok(None);
        };
        parser.end_span(&mut span);

        let iter_name = if kind == Conditional::For {
            let name = parser.next_name()?;
            parser.ensure_next("in")?;
            Some(name)
        } else { None };

        let cond_expr: Option<ExprNode> = if kind != Conditional::Loop { Some(ExprNode::from_def(parser, true)?) } else { None };

        let then_scope: Scope = Scope::from_def(parser)?;
        let else_node: Option<Box<ConditionalChain>> = if kind != Conditional::Loop && parser.is_next("else") {
            if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
                Some(Box::new(cond_chain))
            } else {
                let else_scope: Scope = Scope::from_def(parser)?;
                Some(Box::new(ConditionalChain { kind: Conditional::Else, label: label.clone(), cond_expr: None, then_scope: else_scope, iter_name: iter_name.clone(), else_node: None, span }))
            }
        } else { None };
        return Ok(Some(ConditionalChain { kind, label, cond_expr, then_scope, iter_name, else_node, span }));
    }
}

impl ControlFlow {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let mut span = parser.get_span_start()?;
        let result: ControlFlow = if parser.is_next("skip") {
            parser.end_span(&mut span);
            let label = Label::is_from_def(parser)?;
            ControlFlow::Skip { label, span }
        } else if parser.is_next("return") {
            ControlFlow::Return(ExprNode::from_def(parser, true)?)
        } else if parser.is_next("break") {
            parser.end_span(&mut span);
            let label = Label::is_from_def(parser)?;
            ControlFlow::Break { label, expr: ExprNode::from_def(parser, true)?, span }
        } else {
            return Ok(None);
        };
        Ok(Some(result))
    }
}

impl InfixOpr {
    pub fn is_from_def(parser: &mut Parser,is_value_expr: bool) -> Result<Option<Self>, CompilerError> {
        parser.skip_whitespace()?;
        let ch: char = parser.cur_char()?;
        if !is_value_expr && ch != ',' {
            return Ok(None);
        }
        parser.index += 1;
        let result: InfixOpr = match ch {
            '.' => if parser.cur_char()? == '.' {
                parser.index += 1; InfixOpr::Range
            } else { parser.index -= 1; return Ok(None); }
            'a' => if parser.cur_char()? == 's' {
                parser.index += 1; InfixOpr::As
            } else {
                parser.index -= 1;
                return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedInfixOpr)));
            },
            'i' => if parser.cur_char()? == 's' {
                parser.index += 1; InfixOpr::Is
            } else {
                parser.index -= 1;
                return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedInfixOpr)));
            },
            '*' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::AsnMul } else { InfixOpr::Mul },
            '/' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::AsnDiv } else { InfixOpr::Div },
            '%' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::AsnMod } else { InfixOpr::Mod },
            '+' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::AsnAdd } else { InfixOpr::Add },
            '-' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::AsnSub } else { InfixOpr::Sub },
            '<' => match parser.cur_char()? {
                '<' => { parser.index += 1; InfixOpr::Shl },
                '=' => { parser.index += 1; InfixOpr::Leq },
                _ => InfixOpr::Lss
            },
            '>' => match parser.cur_char()? {
                '>' => { parser.index += 1; InfixOpr::Shr },
                '=' => { parser.index += 1; InfixOpr::Geq },
                _ => InfixOpr::Gtr
            },
            '!' => if parser.cur_char()? == '=' {
                parser.index += 1; InfixOpr::Neq
            } else {
                parser.index -= 1;
                return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedInfixOpr)));
            },
            '&' => if parser.cur_char()? == '&' { parser.index += 1; InfixOpr::LAnd } else { InfixOpr::And },
            '^' => InfixOpr::Xor,
            '|' => if parser.cur_char()? == '|' { parser.index += 1; InfixOpr::LOr } else { InfixOpr::Or },
            '=' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::Eq } else { InfixOpr::Asn },
            ',' => InfixOpr::Com,
            _ => { parser.index -= 1; return Ok(None); }
        };
        Ok(Some(result))
    }

    pub fn precedence(&self) -> i32 {
        match self {
            InfixOpr::As | InfixOpr::Is => 0,
            InfixOpr::Mul | InfixOpr::Div | InfixOpr::Mod => 1,
            InfixOpr::Add | InfixOpr::Sub => 2,
            InfixOpr::Shl | InfixOpr::Shr => 3,
            InfixOpr::Lss | InfixOpr::Leq | InfixOpr::Gtr | InfixOpr::Geq => 4,
            InfixOpr::Eq | InfixOpr::Neq => 5,
            InfixOpr::And => 6,
            InfixOpr::Xor => 7,
            InfixOpr::Or => 8,
            InfixOpr::LAnd => 9,
            InfixOpr::LOr => 10,
            InfixOpr::Range => 11,
            InfixOpr::Asn | InfixOpr::AsnAdd | InfixOpr::AsnDiv | InfixOpr::AsnMod | InfixOpr::AsnMul | InfixOpr::AsnSub => 12,
            InfixOpr::Com => 13
        }
    }

    pub fn is_left_to_right(&self) -> bool {
        if *self == InfixOpr::Asn || *self == InfixOpr::Com { false } else { true }
    }
}

impl ToString for InfixOpr {
    fn to_string(&self) -> String {
        match self {
            InfixOpr::As  => "as".to_string(),
            InfixOpr::Is  => "is".to_string(),
            InfixOpr::Mul  => "*".to_string(),
            InfixOpr::Div  => "/".to_string(),
            InfixOpr::Mod  => "%".to_string(),
            InfixOpr::Add  => "+".to_string(),
            InfixOpr::Sub  => "-".to_string(),
            InfixOpr::Shl  => "<<".to_string(),
            InfixOpr::Shr  => ">>".to_string(),
            InfixOpr::Lss  => "<".to_string(),
            InfixOpr::Leq  => "<=".to_string(),
            InfixOpr::Gtr  => ">".to_string(),
            InfixOpr::Geq  => ">=".to_string(),
            InfixOpr::Eq   => "==".to_string(),
            InfixOpr::Neq  => "!=".to_string(),
            InfixOpr::And  => "&".to_string(),
            InfixOpr::Xor  => "^".to_string(),
            InfixOpr::Or   => "|".to_string(),
            InfixOpr::LAnd => "&&".to_string(),
            InfixOpr::LOr  => "||".to_string(),
            InfixOpr::Range  => "..".to_string(),
            InfixOpr::Asn  => "=".to_string(),
            InfixOpr::AsnAdd  => "+=".to_string(),
            InfixOpr::AsnSub  => "-=".to_string(),
            InfixOpr::AsnMul  => "*=".to_string(),
            InfixOpr::AsnDiv  => "/=".to_string(),
            InfixOpr::AsnMod  => "%=".to_string(),
            InfixOpr::Com  => ",".to_string(),
        }
    }
}

impl PrefixOpr {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let result = match ch {
            '!' => PrefixOpr::Not,
            '&' => if parser.is_next("mut") { PrefixOpr::Addr { is_mut: true } } else { PrefixOpr::Addr { is_mut: false } },
            '*' => PrefixOpr::Deref,
            '-' => PrefixOpr::UMin,
            _ => { parser.index -= 1; return Ok(None); }
        };
        Ok(Some(result))
    }
}

impl ToString for PrefixOpr {
    fn to_string(&self) -> String {
        match self {
            PrefixOpr::Not   => "!".to_string(),
            PrefixOpr::Addr { is_mut }  => if *is_mut { "&mut "} else { "&" }.to_string() ,
            PrefixOpr::Deref => "*".to_string(),
            PrefixOpr::UMin  => "-".to_string()
        }
    }
}

impl PostfixOpr {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<(Self, ExprNode)>, CompilerError> {
        let expr_result: ExprNode;
        let cur_char = parser.cur_char()?;
        parser.index += 1;
        let result = if cur_char == '[' {
            expr_result = ExprNode::from_def(parser, true)?;
            parser.ensure_next("]")?;
            PostfixOpr::Idx
        } else if cur_char == '(' {
            expr_result = ExprNode::from_def(parser, true)?;
            parser.ensure_next(")")?;
            PostfixOpr::Inv
        } else if cur_char == '<' {
            expr_result = ExprNode::from_def(parser, false)?;
            parser.ensure_next(">")?;
            PostfixOpr::Tmp  
        } else if cur_char == '.' {
            if parser.cur_char()? == '.' {
                parser.index -= 1;
                return Ok(None);
            }
            let mut span: Span = parser.get_span_start()?;
            let name: String = parser.next_name()?;
            parser.end_span(&mut span);
            expr_result = ExprNode { value: ExprNodeEnum::Name(name), span }; 
            PostfixOpr::Mem
        } else if cur_char == ':' {
            let mut span: Span = parser.get_span_start()?;
            if parser.is_name_start()? {
                let name: String = parser.next_name()?;
                parser.end_span(&mut span);
                expr_result = ExprNode { value: ExprNodeEnum::Name(name), span };
            } else {
                return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedName)));
            }
            PostfixOpr::Sep
        } else if cur_char == '{' {
            expr_result = ExprNode::from_def(parser, true)?;
            parser.ensure_next("}")?;
            PostfixOpr::Con
        } else {
            parser.index -= 1;
            return Ok(None);
        };
        Ok(Some((result, expr_result)))
    }
}

impl ToString for PostfixOpr {
    fn to_string(&self) -> String {
        match self {
            PostfixOpr::Idx => "[]",
            PostfixOpr::Inv => "()",
            PostfixOpr::Tmp => "<>",
            PostfixOpr::Con => "{}",
            PostfixOpr::Mem => ".",
            PostfixOpr::Sep => ":"
        }.to_string()
    }
}

impl Literal {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if parser.is_next("false") {
            return Ok(Some(Literal::Bool(false)));
        } else if parser.is_next("true") {
            return Ok(Some(Literal::Bool(true)));
        } else if parser.is_next("()") {
            return Ok(Some(Literal::Void));
        } else if parser.is_next("'") {
            let ch: char = Literal::char_from_def(parser)?;
            parser.ensure_next("'")?;
            return Ok(Some(Literal::Char(ch)));
        } else if parser.is_next("\"") {
            let str: String = Literal::string_from_def(parser)?;
            return Ok(Some(Literal::String(str)));
        } else if parser.is_num_start()? {
            let const_value: Literal = Literal::numeral_from_def(parser)?;
            return Ok(Some(const_value));
        }
        return Ok(None);
    }

    fn string_from_def(parser: &mut Parser) -> Result<String, CompilerError> {
        let mut result: String = String::new();
        loop {
            let ch: char = Literal::char_from_def(parser)?;
            if ch == '\"' {
                break;
            }
            result.push(ch);
        }
        return Ok(result);
    }

    fn char_to_string(ch: char) -> String {
        const _r: char = 0x0D as char;
        const _b: char = 0x08 as char;
        const _a: char = 0x07 as char;
        const _n: char = 0x0A as char;
        const _t: char = 0x09 as char;
        const _v: char = 0x0B as char;
        const _0: char = 0x00 as char;
        let str_ch: String = ch.to_string();
        match ch {
            _r => "\\r",
            _b => "\\b",
            _a => "\\a",
            _n => "\\n",
            _t => "\\t",
            _v => "\\v",
            _0 => "\\0",
            '\\' => "\\\\",
            '\'' => "\\\'",
            '\"' => "\\\"",
            _ => str_ch.as_str()
        }.to_string()
    }

    fn string_to_string(str: String) -> String {
        str.into_bytes().iter().map(|ch| Self::char_to_string(*ch as char)).collect::<Vec<String>>().join("")
    }

    fn char_from_def(parser: &mut Parser) -> Result<char, CompilerError> {
        let result: char;
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        if ch == '\\' {
            let ch2: char = parser.cur_char()?;
            parser.index += 1;
            result = match ch2 {
                'r' => 0x0D as char,
                'b' => 0x08 as char,
                'a' => 0x07 as char,
                'n' => 0x0A as char,
                't' => 0x09 as char,
                'v' => 0x0B as char,
                '0' => 0x00 as char,
                '\\' => '\\',
                '\'' => '\'',
                '\"' => '\"',
                _ => { parser.index -= 1; '\\' }
            }
        } else {
            result = ch;
        }
        return Ok(result);
    }

    fn numeral_from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        parser.skip_whitespace()?;
        let ch: char = parser.cur_char()?;
        if !(ch >= '0' && ch <= '9') {
            return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedNumeral)));
        }
        #[derive(PartialEq, Clone)]
        enum NumType { Dec, Hex, Bin }
        let mut num_type: NumType = NumType::Dec;
        if ch == '0' {
            parser.index += 1;
            let ch: char = parser.cur_char()?;
            if ch == 'x' || ch == 'X' {
                num_type = NumType::Hex; 
                parser.index += 1;
            } else if ch == 'b' || ch == 'B' {
                num_type = NumType::Bin;
                parser.index += 1;
            } else {
                parser.index -= 1;
            }
        }
        let mut value: u64 = 0;
        loop {
            let ch: char = parser.cur_char()?;
            match num_type {
                NumType::Dec => {
                    if ch >= '0' && ch <= '9' {
                        value = value * 10 + (ch as u64 - '0' as u64);
                    } else {
                        break
                    }
                }
                NumType::Hex => {
                    if ch >= '0' && ch <= '9' {
                        value = value * 16 + (ch as u64 - '0' as u64);
                    } else if ch >= 'a' && ch <= 'f' {
                        value = value * 16 + (ch as u64 - 'a' as u64 + 10);
                    } else if ch >= 'A' && ch <= 'F' {
                        value = value * 16 + (ch as u64 - 'A' as u64 + 10);
                    } else {
                        break
                    }
                }
                NumType::Bin => {
                    if ch >= '0' && ch <= '1' {
                        value = value * 2 + (ch as u64 - '0' as u64);
                    } else {
                        break
                    }
                }
            }
            parser.index += 1;
        }
        if num_type == NumType::Dec && parser.cur_char()? == '.' {
            parser.index += 1;
            if !parser.is_num_start()? {
                parser.index -= 1;
                return Ok(Literal::UnresolvedInteger(value));
            }
            let mut fractional_value: f64 = 0.0;
            let mut i: u32 = 1;
            loop {
                let ch: char = parser.cur_char()?;
                if ch >= '0' && ch <= '9' {
                    fractional_value += (ch as u64 - '0' as u64) as f64 / (10 as u64).pow(i) as f64;
                    i += 1;
                    parser.index += 1;
                } else {
                    break;
                }
            }
            return Ok(Literal::Float(value as f64 + fractional_value));
        }
        return Ok(Literal::UnresolvedInteger(value));
    }
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Bool(b) => (if *b { "true" } else { "false" }).to_string(),
            Literal::Char(ch) => format!("'{}'", Self::char_to_string(*ch)),
            Literal::UnresolvedInteger(int) => int.to_string(),
            Literal::Int(int) => int.to_string(),
            Literal::Float(num) => num.to_string(),
            Literal::Size(size) => size.to_string(),
            Literal::Void => "()".to_string(),
            Literal::String(str) => format!("\"{}\"", Self::string_to_string(str.clone())),
            _ => "?unknown?".to_string()
        }
    }
}

impl ExprNode {
    /*pub fn is_from_def(parser: &mut Parser, is_value_expr: bool) -> Result<Option<Self>, CompilerError> {
        let ch: char = parser.cur_char()?;
        if ch == ';' || ch == ')' || ch == '}' {
            return Ok(None);
        }
        return Ok(Some(ExprNode::from_def(parser, is_value_expr)?));
    }*/

    pub fn from_def(parser: &mut Parser, is_value_expr: bool) -> Result<Self, CompilerError> {
        let mut root: Box<ExprNode> = Box::new(ExprNode::primary_from_def(parser)?);
        if root.value == ExprNodeEnum::Empty {
            return Ok(*root);
        }
        loop {
            parser.skip_whitespace()?;
            let mut infix_span = parser.get_span_start()?;
            let infix_opr = match InfixOpr::is_from_def(parser,is_value_expr)? {
                Some(opr) => opr,
                None => {
                    break;
                } 
            };
            parser.end_span(&mut infix_span);
            let prec = infix_opr.precedence();
            let is_left_to_right = infix_opr.is_left_to_right();
            let next_node = ExprNode::primary_from_def(parser)?;
            let mut cur_node: &mut Box<ExprNode> = &mut root;

            loop {
                let cur_prec: i32 = if let ExprNodeEnum::InfixOpr(ref cur_infix_opr, _, _) = cur_node.value {
                    cur_infix_opr.precedence()
                } else {
                    -1
                };

                let go_right: bool = (is_left_to_right && cur_prec > prec) || (!is_left_to_right && cur_prec >= prec);

                if go_right {
                    if let ExprNodeEnum::InfixOpr(_, _, ref mut right) = cur_node.value {
                        cur_node = right;
                        continue;
                    }
                }
                let temp: ExprNode = ExprNode { value: ExprNodeEnum::Literal(Literal::UnresolvedInteger(0)), span: cur_node.span };

                let old_box: Box<ExprNode> = std::mem::replace(
                    cur_node,
                    Box::new(temp)
                );

                *cur_node = Box::new(ExprNode { value: ExprNodeEnum::InfixOpr(
                    infix_opr,
                    old_box,
                    Box::new(next_node),
                ), span: infix_span });
                break;
            }
        }
        return Ok(*root);
    }

    pub fn primary_from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        parser.skip_whitespace()?;
        let mut result_span: Span = parser.get_span_start()?;
        let mut result_enum: ExprNodeEnum;
        let ch: char = parser.cur_char()?;
        if ch == ';' || ch == ')' || ch == '}' || ch == '>' || ch == ']' {
            parser.index += 1;
            parser.end_span(&mut result_span);
            parser.index -= 1;
            result_enum = ExprNodeEnum::Empty;
            return Ok(ExprNode { value: result_enum, span: result_span });
        } else if parser.is_next("()") {
            result_enum = ExprNodeEnum::Void;
        } else if let Some(prefix_opr) = PrefixOpr::is_from_def(parser)? {
            result_enum = ExprNodeEnum::PrefixOpr(prefix_opr, Box::new(ExprNode::primary_from_def(parser)?));
        } else if let Some(new_var) = Variable::is_from_def(parser)? {
            result_enum = ExprNodeEnum::VarDeclaration(Box::new(new_var));
        } else if let Some(const_value) = Literal::is_from_def(parser)? {
            result_enum = ExprNodeEnum::Literal(const_value);
        } else if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
            result_enum = ExprNodeEnum::ConditionalChain(Box::new(cond_chain));
        } else if let Some(scope) = Scope::is_from_def(parser)? {
            result_enum = ExprNodeEnum::Scope(Box::new(scope));
        } else if parser.is_next("(") {
            result_enum = ExprNodeEnum::Brackets(Box::new(ExprNode::from_def(parser, true)?));
            parser.ensure_next(")")?;
        } else if parser.is_next("[") {
            let arr_expr: ExprNode = ExprNode::from_def(parser, true)?;
            let size = if parser.is_next(";") {
                Some(Box::new(ExprNode::from_def(parser, true)?))
            } else { None };
            parser.ensure_next("]")?;
            result_enum = ExprNodeEnum::Array(Box::new(arr_expr), size);
        } else if parser.is_name_start()? {
            let name: String = parser.next_name()?;
            result_enum = ExprNodeEnum::Name(name);
        } else {
            return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedPrimaryExpression)));
        }
        parser.end_span(&mut result_span);

        while let Some((postfix_opr, expr_node)) = PostfixOpr::is_from_def(parser)? {
            result_enum = ExprNodeEnum::PostfixOpr(postfix_opr, Box::new(ExprNode { value: result_enum, span: result_span }), Box::new(expr_node));
            parser.end_span(&mut result_span);
        }
        Ok(ExprNode { value: result_enum, span: result_span })
    }
}

impl ToString for ExprNode {
    fn to_string(&self) -> String {
        match &self.value {
            ExprNodeEnum::Empty => "".to_string(),
            ExprNodeEnum::Brackets(expr) => format!("({})", expr.to_string()),
            ExprNodeEnum::Void => "()".to_string(),
            ExprNodeEnum::PrefixOpr(prefix_opr, node) => {
                format!("{}{}", prefix_opr.to_string(), node.to_string())
            },
            ExprNodeEnum::InfixOpr(infix_opr, left, right) => {
                format!("({}) {} ({})", left.to_string(), infix_opr.to_string(), right.to_string())
            },
            ExprNodeEnum::Literal(const_value) => {
                const_value.to_string()
            },
            ExprNodeEnum::Name(name) => {
                name.clone()
            },
            ExprNodeEnum::PostfixOpr(postfix_opr, left, right) => {
                match postfix_opr {
                    PostfixOpr::Idx => format!("{}[{}]", left.to_string(), right.to_string()),
                    PostfixOpr::Inv => format!("{}({})", left.to_string(), right.to_string()),
                    PostfixOpr::Tmp => format!("{}<{}>", left.to_string(), right.to_string()),
                    PostfixOpr::Con => format!("{} {{ {} }}", left.to_string(), right.to_string()),
                    PostfixOpr::Mem => format!("{}.{}", left.to_string(), right.to_string()),
                    PostfixOpr::Sep => format!("{}:{}", left.to_string(), right.to_string())
                }
            },
            ExprNodeEnum::VarDeclaration(var_box) => format!("let {}", var_box.to_string()),
            ExprNodeEnum::Scope(scope) => scope.to_string(),
            ExprNodeEnum::ConditionalChain(cond_chain) => cond_chain.to_string(),
            ExprNodeEnum::Array(values, opt_size_literal) => {
                if let Some(size_literal) = opt_size_literal {
                    format!("[{}; {}]", values.to_string(), size_literal.to_string())
                } else {
                    format!("[{}]", values.to_string())
                }
            }
        }
    }
}

impl Variable {
    pub fn arg_from_def(parser: &mut Parser, is_static: bool) -> Result<Self, CompilerError> {
        let mut span: Span = parser.get_span_start()?;
        let is_mut: bool = parser.is_next("mut");
        let mut refs = vec![];
        loop { if parser.is_next("&mut") { refs.push(true); } else if parser.is_next("&") { refs.push(false);} else { break; }}
        let name: String = parser.next_name()?;
        if name == "self".to_string() {
            parser.end_span(&mut span);
            let mut expr_enum = ExprNode { value: ExprNodeEnum::Name("Self".to_string()), span };
            for is_mut_ref in refs {
                expr_enum = ExprNode { value: ExprNodeEnum::PrefixOpr(PrefixOpr::Addr { is_mut: is_mut_ref }, Box::new(expr_enum)), span };
            }
            return Ok(Self { name: name, var_type: Some(expr_enum), init_expr: None, is_mut, span, is_static });
        }
        let mut var_type: Option<ExprNode> = None;
        let mut init_expr: Option<ExprNode> = None;
        if parser.is_next(":") {
            var_type = Some(ExprNode::primary_from_def(parser)?);
        }
        if parser.is_next("=") {
            let expr: ExprNode = ExprNode::from_def(parser, true)?;
            init_expr = Some(expr);
        } else if var_type == None {
            return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::UndeducibleType)));
        }
        parser.end_span(&mut span);
        return Ok(Self { name: name, var_type: var_type, init_expr: init_expr, is_mut: is_mut, span, is_static });
    }

    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let mut span: Span = parser.get_span_start()?;
        let is_let = parser.is_next("let");
        let is_static = parser.is_next("static");
        if !is_let && !is_static {
            return Ok(None);
        }
        let mut result: Variable = Self::arg_from_def(parser, is_static)?;
        parser.end_span(&mut span);
        result.span = span;
        Ok(Some(result))
    }
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        let mut result: String = (if self.is_mut { "mut " } else { "" }).to_string();
        result += self.name.as_str();
        if let Some(var_type) = &self.var_type {
            result += format!(": {}", var_type.to_string()).as_str();
        }
        if let Some(init_expr) = &self.init_expr {
            result += format!(" = {}", init_expr.to_string()).as_str();
        }
        result
    }
}


impl Const {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let mut span: Span = parser.get_span_start()?;
        if !parser.is_next("const") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        parser.ensure_next(":")?;
        let var_type = ExprNode::primary_from_def(parser)?;
        let init_expr = if parser.is_next("=") { Some(ExprNode::from_def(parser, true)?) } else { None };
        parser.end_span(&mut span);
        return Ok(Some(Self { name: name, var_type: var_type, init_expr: init_expr, span }));
    }
}

impl ToString for Const {
    fn to_string(&self) -> String {
        if let Some(init_expr) = &self.init_expr {
            format!("const {}: {} = {}", self.name, self.var_type.to_string(), init_expr.to_string())
        } else {
            format!("const {}: {}", self.name, self.var_type.to_string())
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Variables {
    pub variables: Vec<Variable>
}

impl Variables {
    pub fn is_from_def(parser: &mut Parser, curly_brackets: bool) -> Result<Option<Self>, CompilerError> {
        parser.skip_whitespace()?;
        let opening_bracket = if curly_brackets { '{' } else { '(' };
        if parser.cur_char()? != opening_bracket {
            return Ok(None);
        }
        Ok(Some(Self::from_def(parser, curly_brackets)?))
    }

    pub fn from_def(parser: &mut Parser, curly_brackets: bool) -> Result<Self, CompilerError> {
        let mut variables: Vec<Variable> = Vec::new();
        let (opening_bracket, closing_bracket) = if curly_brackets { ("{", "}") } else { ("(", ")") };
        parser.ensure_next(opening_bracket)?;
        if !parser.is_next(closing_bracket) {
            variables.push(Variable::arg_from_def(parser, false)?);
            while parser.is_next(",") {
                variables.push(Variable::arg_from_def(parser, false)?);
            }
            parser.ensure_next(closing_bracket)?;
        }
        Ok(Self { variables })
    }
}

impl ToString for Variables {
    fn to_string(&self) -> String {
        format!("({})", self.variables.iter().map(|v: &Variable| v.to_string()).collect::<Vec<String>>().join(", ").as_str())
    }
}

#[derive(PartialEq, Clone)]
struct Structs {
    structs: Vec<Struct>
}

impl Structs {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("(") {
            return Ok(None);
        }
        Ok(Some(Self::from_def(parser)?))
    }

    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut structs: Vec<Struct> = Vec::new();
        parser.ensure_next("(")?;
        if !parser.is_next(")") {
            structs.push(Struct::from_after_def(parser)?);
            while parser.is_next(",") {
                structs.push(Struct::from_after_def(parser)?);                 
            }
            parser.ensure_next(")")?;
        }
        Ok(Self { structs })
    }
}

impl ToString for Structs {
    fn to_string(&self) -> String {
        format!("(\n\t{}\n)", self.structs.iter().map(|v: &Struct| v.to_string()).collect::<Vec<String>>().join(",\n\t").as_str())
    }
}

impl Function {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let prev_parser_idx = parser.stamp_index();
        let external = parser.is_next("extern");
        if !parser.is_next("fun") {
            if external { parser.set_index(prev_parser_idx); }
            return Ok(None);
        }
        let mut span: Span = parser.get_span_start()?;
        let name: String = parser.next_name()?;
        parser.end_span(&mut span);
        let templates: Templates = Templates::is_from_def(parser)?;
        let args: Variables = Variables::from_def(parser, false)?;
        let return_type = if parser.is_next("->") {
            Some(ExprNode::primary_from_def(parser)?)
        } else {
            None
        };
        let scope: Option<Scope> = if parser.is_next(";") { None } else { Some(Scope::from_def(parser)?) };
        Ok(Some(Self { name, templates, args, return_type, scope, span, external }))
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        let mut result = format!("fun {}", self.name);
        result += self.templates.to_string().as_str();
        result += self.args.to_string().as_str();
        if let Some(return_type) = self.return_type.as_ref() {
            result += format!(" -> {}", return_type.to_string()).as_str();
        }
        if let Some(scope) = self.scope.as_ref() {
            result += format!(" {}", scope.to_string()).as_str();
        }
        result
    }
}

impl Struct {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("struct") {
            return Ok(None);
        }
        Ok(Some(Self::from_after_def(parser)?))
    }

    pub fn from_after_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let name: String = parser.next_name()?;
        let templates: Templates = Templates::is_from_def(parser)?;
        let vars: Variables = if parser.is_next(";") { Variables { variables: Vec::new() } } else { Variables::from_def(parser, true)? };
        Ok(Self { name, templates, vars })
    }
}

impl ToString for Struct {
    fn to_string(&self) -> String {
        return format!("{}{}{}",
            self.name,
            self.templates.to_string(),
            self.vars.to_string()
        )
    }
}

impl Enum {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("enum") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        let templates: Templates = Templates::is_from_def(parser)?;
        let structs: Structs = Structs::from_def(parser)?;
        Ok(Some(Self { name, templates, structs }))
    }
}

impl ToString for Enum {
    fn to_string(&self) -> String {
        return format!("enum {}{}{};",
            self.name,
            self.templates.to_string(),
            self.structs.to_string()
        )
    }
}

impl TypeDef {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("type") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        parser.ensure_next("=")?;
        let expr = ExprNode::from_def(parser, false)?;
        parser.ensure_next(";");
        Ok(Some(Self { name, expr }))
    }
}

impl Implementation {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("impl") {
            return Ok(None);
        }
        let templates: Templates = Templates::is_from_def(parser)?;
        let mut target_type: ExprNode = ExprNode::primary_from_def(parser)?;
        let mut opt_trait: Option<ExprNode> = None;
        if parser.is_next("for") {
            opt_trait = Some(target_type);
            target_type = ExprNode::primary_from_def(parser)?;
        }
        let scope = if parser.is_next(";") { None } else { Some(Scope::from_def(parser)?) };
        Ok(Some(Self{templates, opt_trait, target_type, scope}))
    }
}

impl ToString for Implementation {
    fn to_string(&self) -> String {
        let mut result: String = format!("impl{} ", self.templates.to_string());
        let scope_string = if let Some(scope) = &self.scope { scope.to_string() } else { "".to_string() };
        result = if let Some(opt_trait) = self.opt_trait.as_ref() {
           format!("{}{} for {} {}", result, opt_trait.to_string(), self.target_type.to_string(), scope_string)
        } else {
            format!("{}{} {}", result, self.target_type.to_string(), scope_string)
        };
        result
    }
}

impl Trait {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("trait") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        let templates: Templates = Templates::is_from_def(parser)?;
        let sub_traits = if parser.is_next(":") {
            Some(Box::new(ExprNode::primary_from_def(parser)?))
        } else { None };
        let scope = if parser.is_next(";") { None } else { Some(Scope::from_def(parser)?) };
        Ok(Some(Self{name, templates, scope, sub_traits}))
    }
}

impl ToString for Trait {
    fn to_string(&self) -> String {
        format!("trait {}{} {}", self.name, self.templates.to_string(), if let Some(scope) = &self.scope { scope.to_string() } else { "".to_string() })
    }
}

impl Statement {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if let Some(new_var) = Variable::is_from_def(parser)? {
            parser.ensure_next(";")?;
            return Ok(Statement::VarDeclaration(new_var));
        } else if let Some(const_value) = Const::is_from_def(parser)? {
            parser.ensure_next(";")?;
            return Ok(Statement::Const(const_value));              
        } else if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
            return Ok(Statement::ConditionalChain(cond_chain));
        } else if let Some(control_flow) = ControlFlow::is_from_def(parser)? {
            parser.ensure_next(";")?;
            return Ok(Statement::ControlFlow(control_flow));
        } else {
            let expr: ExprNode = ExprNode::from_def(parser, true)?;
            let is_final_value: bool = !parser.is_next(";");
            parser.skip_whitespace()?;
            if is_final_value && parser.cur_char()? != '}' {
                return Err(parser.error(CompilerErrorType::SyntaxError(SyntaxError::MissingSemicolon)));
            }
            return Ok(Statement::Expression { expr, is_final_value });
        }
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: Vec::new(),
            statements: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            type_defs: Vec::new(),
            implementations: Vec::new(),
            traits: Vec::new(),
            modules: Vec::new(),
            return_type: None,
            span: Span { file_id: FileId(0), line_start: 0, col_start: 0, line_end: 0, col_end: 0, line_index: 0 }
        }
    }

    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if parser.cur_char()? == '{' {
            return Ok(Some(Scope::from_def(parser)?));
        }
        Ok(None)
    }

    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut span: Span = parser.get_span_start()?;
        parser.ensure_next("{")?;
        parser.end_span(&mut span);
        let mut result: Scope = Scope::new();
        while !parser.is_next("}") {
            result.parse_next(parser)?;
        }
        result.span = span;
        Ok(result)
    }

    pub fn parse_next(&mut self, parser: &mut Parser) -> Result<(), CompilerError> {
        if parser.is_finished() {
            return Ok(());
        }
        if let Some(obj) = Module::is_from_def(parser)? {
            self.modules.push(obj);
        } else if let Some(strct) = Struct::is_from_def(parser)? {
            self.structs.push(strct);
        } else if let Some(enm) = Enum::is_from_def(parser)? {
            self.enums.push(enm);
        } else if let Some(type_def) = TypeDef::is_from_def(parser)? {
            self.type_defs.push(type_def);
        } else if let Some(imp) = Implementation::is_from_def(parser)? {
            self.implementations.push(imp);
        } else if let Some(trt) = Trait::is_from_def(parser)? {
            self.traits.push(trt);
        } else if let Some(fun) = Function::is_from_def(parser)? {
            self.functions.push(fun);
        } else {
            let statement: Statement = Statement::from_def(parser)?;
            self.statements.push(statement);
        }
        Ok(())
    }

    pub fn find_module(&self, name: &str) -> Option<&Module> {
        for module in self.modules.iter() {
            if module.name == name {
                return Some(module);
            }
        }
        None
    }
}

impl Module {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("mod") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        let scope: Scope = Scope::from_def(parser)?;
        Ok(Some(Module { name, scope }))
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::VarDeclaration(variable) => {
                format!("let {};", variable.to_string())
            }
            Statement::Expression { expr, is_final_value } => {
                expr.to_string() + if *is_final_value { "" } else { ";" }
            }
            Statement::ConditionalChain(cond_chain) => {
                cond_chain.to_string()
            }
            Statement::ControlFlow(control_flow) => {
                control_flow.to_string() + ";"
            }
            Statement::Const(const_value) => {
                const_value.to_string() + ";"
            }
        }
    }
}

impl ToString for Scope {
    fn to_string(&self) -> String {
        let mut indented_str: String = String::new();
        if !self.structs.is_empty() {
            indented_str += self.structs.iter().map(|strct: &Struct| format!("struct {};", strct.to_string())).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.enums.is_empty() {
            indented_str += self.enums.iter().map(|enm: &Enum| enm.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.implementations.is_empty() {
            indented_str += self.implementations.iter().map(|imp: &Implementation| imp.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.traits.is_empty() {
            indented_str += self.traits.iter().map(|trt: &Trait| trt.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.statements.is_empty() {
            indented_str += self.statements.iter().map(|stat: &Statement| stat.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.modules.is_empty() {
            indented_str += self.modules.iter().map(|obj: &Module| obj.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.functions.is_empty() {
            indented_str += self.functions.iter().map(|fun: &Function| fun.to_string()).collect::<Vec<String>>().join("\n").as_str();
        }
        return format!("{{\n{}\n}}", indent_each_line(indented_str.as_str()));
    }
}

impl ToString for Module {
    fn to_string(&self) -> String {
        return format!("module {} ", self.name) + self.scope.to_string().as_str();
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct Span {
    pub file_id: FileId,
    pub line_start: usize,
    pub col_start: usize,
    pub line_end: usize,
    pub col_end: usize,
    pub line_index: usize
}

impl Span {
    pub fn merge(&self, other: Span) -> Span {
        let is_self_start: bool = if self.line_start == other.line_start { self.col_start < other.col_start } else { self.line_start < other.line_start };
        let is_self_end: bool = if self.line_end == other.line_end { self.col_end > other.col_end } else { self.line_end > other.line_end };
        let (line_start, col_start, line_index) = if is_self_start { (self.line_start, self.col_start, self.line_index) } else { (other.line_start, other.col_start, other.line_index) };
        let (line_end, col_end) = if is_self_end { (self.line_end, self.col_end) } else { (other.line_end, other.col_end) };
        Span { file_id: self.file_id, line_start, col_start, line_end, col_end, line_index }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct FileId(i32);

pub struct FileContext {
    pub ids: Vec<String>,
    pub files: HashMap<FileId, Vec<char>>,
}

impl FileContext {
    pub fn new() -> Self {
        Self { ids: Vec::new(), files: HashMap::new() }
    }

    pub fn next_id(&mut self, path: String) -> FileId {
        for (i, cur_path) in self.ids.iter().enumerate() {
            if *cur_path == path {
                return FileId(i as i32);
            }
        }
        let id: FileId = FileId(self.ids.len() as i32);
        self.ids.push(path);
        id
    }

    pub fn get_path(&self, id: FileId) -> String {
        self.ids[id.0 as usize].clone()
    }

    pub fn contains_key(&self, path: String) -> bool {
        self.ids.iter().any(|p| *p == path)
    }
}

pub struct Parser<'fctx> {
    file_context: &'fctx mut FileContext,
    cur_file_id: FileId,
    index: usize,
    new_line_index: usize,
    cur_line: usize
}

impl<'fctx> Parser<'fctx> {
    pub fn new(file_context: &'fctx mut FileContext) -> Self {
        Parser {
            file_context,
            cur_file_id: FileId(-1),
            index: 0,
            new_line_index: 0,
            cur_line: 0
        }
    }

    pub fn error(&mut self, err_type: CompilerErrorType) -> CompilerError {
        let chars: &Vec<char> = &self.cur_file();
        let line_end_idx: usize = chars[self.new_line_index..].iter().position(|&c| c == '\n').map(|pos| self.new_line_index + pos).unwrap_or(chars.len());
        let line_str: String = chars[self.new_line_index..line_end_idx].iter().collect();
        let span_start = match self.get_span_start() { Ok(v) => v, Err(e) => return e };
        CompilerError { err_type, file: self.file_context.get_path(self.cur_file_id), span: Some(span_start), line_str }
    }

    pub fn get_span_start(&mut self) -> Result<Span, CompilerError> {
        self.skip_whitespace()?;
        Ok(Span { file_id: self.cur_file_id, line_start: self.cur_line, col_start: self.get_col() - 1, line_end: self.cur_line, col_end: self.get_col(), line_index: self.new_line_index })
    }

    pub fn end_span(&self, span: &mut Span) {
        span.line_end = self.cur_line;
        span.col_end = self.get_col() - 1;
    }

    pub fn get_col(&self) -> usize {
        self.index - self.new_line_index + 1
    }

    pub fn cur_file(&self) -> &Vec<char> {
        &self.file_context.files[&self.cur_file_id]
    }

    pub fn stamp_index(&self) -> (usize, usize, usize) {
        (self.index, self.new_line_index, self.cur_line)
    }

    pub fn set_index(&mut self, index_data: (usize, usize, usize)) {
        self.index = index_data.0;
        self.new_line_index = index_data.1;
        self.cur_line = index_data.2;
    }

    pub fn parse_file(&mut self, path: String, main_scope: &mut Scope) -> Result<(), CompilerError> {
        if self.file_context.contains_key(path.clone()) {
            return Ok(());
        }
        let contents: String = match fs::read_to_string(path.as_str()) {
            Ok(text) => text,
            Err(e) => {
                return Err(self.error(CompilerErrorType::LinkerError(LinkerError::ImportFailed(path, e.to_string()))));
            }
        };
        let file_id: FileId = self.file_context.next_id(path);
        let saved_file_id: FileId = self.cur_file_id;
        let saved_index: usize = self.index;
        let saved_cur_line: usize = self.cur_line;
        self.cur_file_id = file_id;
        self.index = 0;
        self.cur_line = 0;
        self.file_context.files.insert(self.cur_file_id, contents.chars().collect());
        self.parse(main_scope)?;
        self.cur_file_id = saved_file_id;
        self.index = saved_index;
        self.cur_line = saved_cur_line;
        Ok(())
    }

    pub fn parse(&mut self, main_scope: &mut Scope) -> Result<(), CompilerError> {
        self.skip_whitespace()?;
        while self.is_next("import") {
            let path: String = self.next_until(|ch| ch == ';')?;
            self.parse_file(path, main_scope)?;
        }
        while !self.is_finished() {
            main_scope.parse_next(self)?;
        }
        Ok(())
    }

    pub fn cur_char(&mut self) -> Result<char, CompilerError> {
        let file_text: &Vec<char> = self.file_context.files.get(&self.cur_file_id).unwrap();
        if let Some(ch) = file_text.get(self.index).copied() {
            return Ok(ch);
        } else {
            return Ok('\0');
        }
        let err = CompilerError { err_type: CompilerErrorType::SyntaxError(SyntaxError::ScriptEndedTooEarly), file: self.file_context.get_path(self.cur_file_id), span: None , line_str: "".to_string() };
        Err(err)
    }

    pub fn is_name_start(&mut self) -> Result<bool, CompilerError> {
        let ch = self.cur_char()?;
        Ok(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z')
    }

    pub fn is_num_start(&mut self) -> Result<bool, CompilerError> {
        let ch = self.cur_char()?;
        Ok(ch >= '0' && ch <= '9')
    }
    
    pub fn next_until<F>(&mut self, end_condition: F) -> Result<String, CompilerError> where F: Fn(char) -> bool {
        self.skip_whitespace()?;
        let start_index: usize = self.index;
        loop {
            let ch = self.cur_char()?;
            if end_condition(ch) {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.index += 1;
        Ok(result)
    }

    pub fn next_name(&mut self) -> Result<String, CompilerError> {
        self.skip_whitespace()?;
        let start_index: usize = self.index;
        if !self.is_name_start()? {
            return Err(self.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedName)));
        }
        self.index += 1;
        while let Ok(ch) = self.cur_char() {
            if !(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch >= '0' && ch <= '9') && ch != '_' {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        Ok(result)
    }

    pub fn skip_whitespace(&mut self) -> Result<(), CompilerError> {
        loop {
            let ch: char = self.cur_char()?;
            if ch == '/' {
                self.skip_if_comment()?;
            }
            if !ch.is_whitespace() { 
                break;
            }
            if ch == '\n' {
                self.cur_line += 1;
                self.new_line_index = self.index + 1;
            }
            self.index += 1;
        }
        Ok(())
    }

    pub fn is_finished(&mut self) -> bool {
        self.skip_whitespace();
        self.index >= self.cur_file().len()
    }

    pub fn skip_if_comment(&mut self) -> Result<(), CompilerError> {
        let mut ch: char = self.cur_char()?;
        if ch != '/' {
            return Ok(());
        }
        self.index += 1;
        ch = self.cur_char()?;
        if ch == '/' {
            self.index += 1;
            loop {
                ch = self.cur_char()?;
                if ch == '\n' {
                    break;
                }
                self.index += 1;
            }          
        } else {
            self.index -= 1;
        }
        return Ok(());
    }

    pub fn is_next(&mut self, s: &str) -> bool {
        if self.is_finished() {
            return false;
        }
        if let Err(_) = self.skip_whitespace() {
            return false;
        }
        let s_chars: Vec<char> = s.chars().collect();
        let end: usize = self.index + s_chars.len();
        if end > self.cur_file().len() {
            return false;
        }
        let result: bool = self.cur_file()[self.index..end] == s_chars;
        if result {
            self.index = end;
        }
        result
    }

    pub fn ensure_next(&mut self, s: &'static str) -> Result<(), CompilerError> {
        if !self.is_next(s) {
            return Err(self.error(CompilerErrorType::SyntaxError(SyntaxError::ExpectedToken(s))));
        }
        Ok(())
    }

    /*pub fn index_error(&self, err: CompilerError) -> CompilerError {
        let indexing_str = format!(
            "Error in file '{}' at line {}, column {}: ",
            self.file_context.get_path(self.cur_file_id),
            self.cur_line + 1,
            self.get_col(),
        );
        match err {
            CompilerError::LinkerError(err) => CompilerError::LinkerError(format!("{}{}", indexing_str, err)),
            CompilerError::SemanticError(err) => CompilerError::SemanticError(format!("{}{}", indexing_str, err)),
            CompilerError::SyntaxError(err) => CompilerError::SyntaxError(format!("{}{}", indexing_str, err)),
        }
    }*/
}

fn indent_each_line(input: &str) -> String {
    input
        .lines()
        .map(|line| format!("    {}", line))
        .collect::<Vec<String>>()
        .join("\n")
}