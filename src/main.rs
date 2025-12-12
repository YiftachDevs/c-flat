use std::{collections::HashMap, env, fs, path::Path};

use crate::errors::CompilerError;
mod errors;

pub struct NamePath {
    path: Vec<String>, // Game.Player.get_pos;
    templates: Option<TemplatesValues>
}

impl NamePath {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_name_start()? { return Ok(None); }
        return Ok(Some(NamePath::from_def(parser)?));
    }
    
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut path: Vec<String> = Vec::from([parser.next_name()?]);
        while parser.is_next(".") {
            path.push(parser.next_name()?);
        }
        let templates: Option<TemplatesValues> = TemplatesValues::is_from_def(parser)?;
        return Ok(NamePath { path, templates });
    }
}

impl ToString for NamePath {
    fn to_string(&self) -> String {
        let mut result: String = self.path.join(".");
        if let Some(templates_values) = &self.templates {
            result += templates_values.to_string().as_str();
        }
        result
    }
}

pub struct Templates {
    templates: Vec<Template>
}

impl Templates {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("<") { return Ok(None); }
        let mut templates: Vec<Template> = Vec::new();
        loop {
            templates.push(Template::from_def(parser)?);
            if parser.is_next(">") { break; }
            parser.ensure_next(",")?;
        }
        return Ok(Some(Templates { templates }));   
    }
}

impl ToString for Templates {
    fn to_string(&self) -> String {
        let str_vec: Vec<String> = self.templates.iter().map(|v| v.to_string()).collect();
        return format!("<{}>", str_vec.join(", "));
    }
}

pub enum Template {
    ConstValue(String, VarType),
    VarType(String)
}

impl Template {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if parser.is_next("const") {
            let arg: Variable = Variable::arg_from_def(parser)?;
            if let Some(var_type) = arg.var_type {
                return Ok(Template::ConstValue(arg.name, var_type));
            } else {
                return Err(CompilerError::SyntaxError("Expected a type for a const template".to_string()));
            }
        } else {
            return Ok(Template::VarType(parser.next_name()?));
        }
    }
}

impl ToString for Template {
    fn to_string(&self) -> String {
        match self {
            Template::ConstValue(name, var_type) => format!("const {}: {}", name, var_type.to_string()),
            Template::VarType(name) => name.clone()
        }
    }
}

pub struct TemplatesValues {
    templates: Box<ExprNode>
}


impl ToString for TemplatesValues {
    fn to_string(&self) -> String {
        return format!("<{}>", self.templates.to_string());
    }
}

impl TemplatesValues {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("<") { return Ok(None); }
        let result: TemplatesValues = TemplatesValues { templates: Box::new(ExprNode::from_def(parser)?) };
        parser.ensure_next(">")?;
        return Ok(Some(result));
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum PrimitiveType {
    Int,
    Size,
    Float,
    Byte,
    Char,
    Bool,
    Void
}

impl ToString for PrimitiveType {
    fn to_string(&self) -> String {
        match self {
            PrimitiveType::Int => "int".to_string(),
            PrimitiveType::Size => "size".to_string(),
            PrimitiveType::Float => "float".to_string(),
            PrimitiveType::Byte => "byte".to_string(),
            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::Void => "()".to_string(),
        }
    }
}

pub enum VarType {
    Unresolved { name_path: NamePath },
    Primitive(PrimitiveType),
    Pointer { ptr_type: Box<VarType>, is_ref: bool },
    // Pointer(Box<VarType>)
    // Array(Box<VarType>, u32),
    // Tuple(Vec<VarType>),
    // Callback(Vec<VarType>, Box<VarType>)
}

#[derive(PartialEq, Clone)]
pub enum ConstValue {
    UnresolvedInteger(u32),
    Int(u32),
    Size(u32),
    Bool(bool),
    Char(char),
    Float(f32),
    String(String),
    Void
}

pub struct Variable {
    name: String,
    var_type: Option<VarType>,
    init_expr: Option<ExprNode>,
    is_mut: bool,
    is_resolved: bool
}

pub struct Function {
    name: String,
    templates: Option<Templates>,
    args: Variables,
    return_type: Option<VarType>,
    scope: Option<Scope>
}

pub enum Statement {
    Expression{ expr: ExprNode, is_final_value: bool},
    VarDeclaration(Variable),
    ConditionalChain(ConditionalChain),
    ControlFlow(ControlFlow)
}

#[derive(PartialEq, Clone, Copy)]
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
    Com
}

#[derive(PartialEq, Clone, Copy)]
pub enum PrefixOpr {
    Not,
    Addr,
    LNot,
    Deref,
    UMin
}

#[derive(PartialEq, Clone, Copy)]
pub enum PostfixOpr {
    Inv,
    Idx,
    Mem
}

pub enum Conditional {
    If,
    For,
    While,
    When,
    Else
}

pub struct  ConditionalChain {
    kind: Conditional,
    cond_expr: Option<ExprNode>,
    then_scope: Scope,
    else_node: Option<Box<ConditionalChain>>
}

impl ToString for ConditionalChain {
    fn to_string(&self) -> String {
        let kind_str = match self.kind {
            Conditional::If => "if",
            Conditional::For => "for",
            Conditional::While => "while",
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

pub enum ControlFlow {
    Return(Option<ExprNode>),
    Skip,
    Stop(Option<ExprNode>)
}

impl ToString for ControlFlow {
    fn to_string(&self) -> String {
        match self {
            ControlFlow::Return(expr_node_opt) => {
                let mut result: String = "return".to_string();
                if let Some(expr_node) = expr_node_opt {
                    result += format!(" {}", expr_node.to_string()).as_str();
                }
                result
            }
            ControlFlow::Stop(expr_node_opt) => {
                let mut result: String = "stop".to_string();
                if let Some(expr_node) = expr_node_opt {
                    result += format!(" {}", expr_node.to_string()).as_str();
                }
                result
            }
            ControlFlow::Skip => "skip".to_string()
        }
    }
}

pub struct Scope {
    variables: Vec<Variable>,
    statements: Vec<Statement>,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    objects: Vec<Object>,
    return_type: Option<VarType>
}

pub enum ExprNode {
    InfixOpr(InfixOpr, Box<ExprNode>, Box<ExprNode>),
    PrefixOpr(PrefixOpr, Box<ExprNode>),
    PostfixOpr(PostfixOpr, Box<ExprNode>, Box<Option<ExprNode>>),
    NamePath(NamePath),
    ConstValue(ConstValue),
    VarDeclaration(Box<Variable>),
    Scope(Scope),
    ConditionalChain(Box<ConditionalChain>)
}

pub struct Object {
    name: String,
    scope: Scope
}

pub struct Struct {
    name: String,
    vars: Vec<Variable>
}

impl ConditionalChain {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let kind: Conditional = if parser.is_next("if") {
            Conditional::If
        } else if parser.is_next("for") {
            Conditional::For
        } else if parser.is_next("while") {
            Conditional::While
        } else if parser.is_next("when") {
            Conditional::When
        } else {
            return Ok(None);
        };

        let cond_expr: Option<ExprNode> = Some(ExprNode::from_def(parser)?);
        let then_scope: Scope = Scope::from_def(parser)?;
        let else_node: Option<Box<ConditionalChain>> = if parser.is_next("else") {
            if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
                Some(Box::new(cond_chain))
            } else {
                let else_scope: Scope = Scope::from_def(parser)?;
                Some(Box::new(ConditionalChain { kind: Conditional::Else, cond_expr: None, then_scope: else_scope, else_node: None }))
            }
        } else { None };
        return Ok(Some(ConditionalChain { kind, cond_expr, then_scope, else_node }));
    }
}

impl ControlFlow {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let result: ControlFlow = if parser.is_next("skip") {
            ControlFlow::Skip
        } else if parser.is_next("return") {
            ControlFlow::Return(ExprNode::is_from_def(parser)?)
        } else if parser.is_next("stop") {
            ControlFlow::Stop(ExprNode::is_from_def(parser)?)
        } else {
            return Ok(None);
        };
        Ok(Some(result))
    }
}

impl PrimitiveType {
    pub fn from_def(parser: &mut Parser) -> Option<Self> {
        if parser.is_next("int") {
            return Some(PrimitiveType::Int);
        }
        if parser.is_next("float") {
            return Some(PrimitiveType::Float);
        }
        if parser.is_next("bool") {
            return Some(PrimitiveType::Bool);
        }
        if parser.is_next("size") {
            return Some(PrimitiveType::Size);
        }
        if parser.is_next("byte") {
            return Some(PrimitiveType::Byte);
        }
        if parser.is_next("char") {
            return Some(PrimitiveType::Char);
        }
        if parser.is_next("void") {
            return Some(PrimitiveType::Void);
        }
        return None;
    }
}

impl InfixOpr {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let result: InfixOpr = match ch {
            'a' => if parser.cur_char()? == 's' {
                parser.index += 1; InfixOpr::As
            } else {
                parser.index -= 1;
                return Err(CompilerError::SyntaxError("Expected an infix operator, did you mean 'as'?".to_string()));
            },
            'i' => if parser.cur_char()? == 's' {
                parser.index += 1; InfixOpr::Is
            } else {
                parser.index -= 1;
                return Err(CompilerError::SyntaxError("Expected an infix operator, did you mean 'is'?".to_string()));
            },
            '*' => InfixOpr::Mul,
            '/' => InfixOpr::Div,
            '%' => InfixOpr::Mod,
            '+' => InfixOpr::Add,
            '-' => InfixOpr::Sub,
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
                return Err(CompilerError::SyntaxError("Expected an infix operator, instead found the '!' prefix operator".to_string()));
            },
            '&' => if parser.cur_char()? == '&' { parser.index += 1; InfixOpr::LAnd } else { InfixOpr::And },
            '^' => InfixOpr::Xor,
            '|' => if parser.cur_char()? == '|' { parser.index += 1; InfixOpr::LOr } else { InfixOpr::Or },
            '=' => if parser.cur_char()? == '=' { parser.index += 1; InfixOpr::Eq } else { InfixOpr::Asn },
            ',' => InfixOpr::Com,
            _ => { parser.index -= 1; return Ok(None); }
        };
        parser.skip_whitespace()?;
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
            InfixOpr::Asn => 11,
            InfixOpr::Com => 12
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
            InfixOpr::Asn  => "=".to_string(),
            InfixOpr::Com  => ",".to_string(),
        }
    }
}

impl PrefixOpr {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let result = match ch {
            '~' => PrefixOpr::Not,
            '!' => PrefixOpr::LNot,
            '&' => PrefixOpr::Addr,
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
            PrefixOpr::Not   => "~".to_string(),
            PrefixOpr::Addr  => "&".to_string(),
            PrefixOpr::LNot  => "!".to_string(),
            PrefixOpr::Deref => "*".to_string(),
            PrefixOpr::UMin  => "-".to_string()
        }
    }
}

impl PostfixOpr {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<(Self, Option<ExprNode>)>, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let expr_result: Option<ExprNode>;
        let result: PostfixOpr = match ch {
            '[' => {
                expr_result = Some(ExprNode::from_def(parser)?);
                parser.ensure_next("]")?;
                PostfixOpr::Idx
            }
            '(' => {
                expr_result = ExprNode::is_from_def(parser)?;
                parser.ensure_next(")")?;
                PostfixOpr::Inv
            }
            '.' => {
                expr_result = Some(ExprNode::NamePath(NamePath::from_def(parser)?));
                PostfixOpr::Mem
            }
            _ => { parser.index -= 1; return Ok(None); }
        };
        Ok(Some((result, expr_result)))
    }
}

impl VarType {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if let Some(primitive_type) = PrimitiveType::from_def(parser) {
            return Ok(VarType::Primitive(primitive_type));
        }
        if parser.is_next("*") {
            return Ok(VarType::Pointer { ptr_type: Box::new(VarType::from_def(parser)?), is_ref: false });
        }
        if parser.is_next("&") {
            return Ok(VarType::Pointer { ptr_type: Box::new(VarType::from_def(parser)?), is_ref: true });
        }
        if let Some(name_path) = NamePath::is_from_def(parser)? {
            return Ok(VarType::Unresolved { name_path: name_path });
        }
        Err(CompilerError::SyntaxError(format!("Type cannot start with char {}", parser.cur_char()?)))
    }  
}

impl ToString for VarType {
    fn to_string(&self) -> String {
        match self {
            VarType::Pointer { ptr_type, is_ref } =>
                if *is_ref {
                    format!("&{}", ptr_type.to_string())
                } else {
                    format!("*{}", ptr_type.to_string())
                },
            VarType::Unresolved { name_path } => name_path.to_string(),
            VarType::Primitive(primitive_type) => primitive_type.to_string()
        }
    }
}

impl ConstValue {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if parser.is_next("false") {
            return Ok(Some(ConstValue::Bool(false)));
        } else if parser.is_next("true") {
            return Ok(Some(ConstValue::Bool(true)));
        } else if parser.is_next("'") {
            let ch: char = ConstValue::char_from_def(parser)?;
            parser.ensure_next("'")?;
            return Ok(Some(ConstValue::Char(ch)));
        } else if parser.is_next("\"") {
            let str: String = ConstValue::string_from_def(parser)?;
            return Ok(Some(ConstValue::String(str)));
        } else if parser.is_num_start()? {
            let const_value: ConstValue = ConstValue::numeral_from_def(parser)?;
            return Ok(Some(const_value));
        }
        return Ok(None);
    }

    fn string_from_def(parser: &mut Parser) -> Result<String, CompilerError> {
        let mut result: String = String::new();
        loop {
            let ch: char = ConstValue::char_from_def(parser)?;
            parser.index += 1;
            if ch == '\"' {
                break;
            }
            result.push(ch);
        }
        return Ok(result);
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
        let ch: char = parser.cur_char()?;
        if !(ch >= '0' && ch <= '9') {
            return Err(CompilerError::SyntaxError(format!("Expected a numeral, found char '{}'", ch)));
        }
        #[derive(PartialEq)]
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
        let mut value: u32 = 0;
        loop {
            let ch: char = parser.cur_char()?;
            match num_type {
                NumType::Dec => {
                    if ch >= '0' && ch <= '9' {
                        value = value * 10 + (ch as u32 - '0' as u32);
                    } else {
                        break
                    }
                }
                NumType::Hex => {
                    if ch >= '0' && ch <= '9' {
                        value = value * 16 + (ch as u32 - '0' as u32);
                    } else if ch >= 'a' && ch <= 'f' {
                        value = value * 16 + (ch as u32 - 'a' as u32 + 10);
                    } else if ch >= 'A' && ch <= 'F' {
                        value = value * 16 + (ch as u32 - 'A' as u32 + 10);
                    } else {
                        break
                    }
                }
                NumType::Bin => {
                    if ch >= '0' && ch <= '1' {
                        value = value * 2 + (ch as u32 - '0' as u32);
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
                return Ok(ConstValue::UnresolvedInteger(value));
            }
            let mut fractional_value: f32 = 0.0;
            let mut i: u32 = 1;
            loop {
                let ch: char = parser.cur_char()?;
                if ch >= '0' && ch <= '9' {
                    fractional_value += (ch as u32 - '0' as u32) as f32 / (10 as u32).pow(i) as f32;
                    i += 1;
                    parser.index += 1;
                } else {
                    break;
                }
            }
            parser.skip_whitespace()?;
            return Ok(ConstValue::Float(value as f32 + fractional_value));
        }
        parser.skip_whitespace()?;
        return Ok(ConstValue::UnresolvedInteger(value));
    }
}

impl ToString for ConstValue {
    fn to_string(&self) -> String {
        match *self {
            ConstValue::Bool(b) => (if b { "true" } else { "false" }).to_string(),
            ConstValue::Char(ch) => ch.to_string(),
            ConstValue::UnresolvedInteger(int) => int.to_string(),
            ConstValue::Int(int) => int.to_string(),
            ConstValue::Float(num) => num.to_string(),
            ConstValue::Size(size) => size.to_string(),
            ConstValue::Void => "()".to_string(),
            _ => "?".to_string()
        }
    }
}

impl ExprNode {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        let ch: char = parser.cur_char()?;
        if ch == ';' || ch == ')' {
            return Ok(None);
        }
        return Ok(Some(ExprNode::from_def(parser)?));
    }

    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut root: Box<ExprNode> = Box::new(ExprNode::primary_from_def(parser)?);
        if let Some(infix_opr) = InfixOpr::is_from_def(parser)? {
            let next_node: ExprNode = ExprNode::primary_from_def(parser)?;
            root = Box::new(ExprNode::InfixOpr(infix_opr, root, Box::new(next_node)));
        }
        while let Some(infix_opr) = InfixOpr::is_from_def(parser)? {
            let prec = infix_opr.precedence();
            let is_left_to_right = infix_opr.is_left_to_right();
            let next_node = ExprNode::primary_from_def(parser)?;
            let mut cur_node: &mut Box<ExprNode> = &mut root;

            loop {
                let cur_prec: i32 = if let ExprNode::InfixOpr(ref cur_infix_opr, _, _) = **cur_node {
                    cur_infix_opr.precedence()
                } else {
                    -1
                };

                let go_right: bool = (is_left_to_right && cur_prec > prec) || (!is_left_to_right && cur_prec >= prec);

                if go_right {
                    if let ExprNode::InfixOpr(_, _, ref mut right) = **cur_node {
                        cur_node = right;
                        continue;
                    }
                }
                let temp: ExprNode = ExprNode::ConstValue(ConstValue::UnresolvedInteger(0));

                let old_box: Box<ExprNode> = std::mem::replace(
                    cur_node,
                    Box::new(temp)
                );

                *cur_node = Box::new(ExprNode::InfixOpr(
                    infix_opr,
                    old_box,
                    Box::new(next_node),
                ));
                break;
            }
        }
        return Ok(*root);
    }

    pub fn primary_from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut result: ExprNode;
        if let Some(prefix_opr) = PrefixOpr::is_from_def(parser)? {
            result = ExprNode::PrefixOpr(prefix_opr, Box::new(ExprNode::primary_from_def(parser)?));
        } else if let Some(new_var) = Variable::is_from_def(parser)? {
            result = ExprNode::VarDeclaration(Box::new(new_var));
        } else if let Some(const_value) = ConstValue::is_from_def(parser)? {
            result = ExprNode::ConstValue(const_value);
        } else if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
            result = ExprNode::ConditionalChain(Box::new(cond_chain));
        } else if let Some(name_path) = NamePath::is_from_def(parser)? {
            result = ExprNode::NamePath(name_path);
        } else if let Some(scope) = Scope::is_from_def(parser)? {
            result = ExprNode::Scope(scope);
        } else if parser.is_next("(") {
            let inner_expr: ExprNode = ExprNode::from_def(parser)?;
            parser.ensure_next(")")?;
            result = inner_expr;
        } else {
            return Err(CompilerError::SyntaxError("Missing / Unknown primary expression".to_string()));
        }

        while let Some((postfix_opr, expr_node)) = PostfixOpr::is_from_def(parser)? {
            result = ExprNode::PostfixOpr(postfix_opr, Box::new(result), Box::new(expr_node));
        }
        Ok(result)
    }
}

impl ToString for ExprNode {
    fn to_string(&self) -> String {
        match self {
            ExprNode::PrefixOpr(prefix_opr, node) => {
                format!("{}{}", prefix_opr.to_string(), node.to_string())
            },
            ExprNode::InfixOpr(infix_opr, left, right) => {
                format!("{} {} {}", left.to_string(), infix_opr.to_string(), right.to_string())
            },
            ExprNode::ConstValue(const_value) => {
                const_value.to_string()
            },
            ExprNode::NamePath(name_path) => {
                name_path.to_string()
            },
            ExprNode::PostfixOpr(postfix_opr, left, right) => {
                let right_str: String = if let Some(right_expr) = right.as_ref() { right_expr.to_string() } else { String::new() };
                match postfix_opr {
                    PostfixOpr::Idx => format!("{}[{}]", left.to_string(), right_str),
                    PostfixOpr::Inv => format!("{}({})", left.to_string(), right_str),
                    PostfixOpr::Mem => format!("{}.{}", left.to_string(), right_str)
                }
            },
            ExprNode::VarDeclaration(var_box) => var_box.to_string(),
            ExprNode::Scope(scope) => scope.to_string(),
            ExprNode::ConditionalChain(cond_chain) => cond_chain.to_string()
        }
    }
}

impl Variable {
    pub fn arg_from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let is_mut: bool = parser.is_next("mut");
        let name: String = parser.next_name()?;
        let mut var_type: Option<VarType> = None;
        let mut init_expr: Option<ExprNode> = None;
        if parser.is_next(":") {
            var_type = Some(VarType::from_def(parser)?);
        }
        if parser.is_next("=") {
            let expr: ExprNode = ExprNode::from_def(parser)?;
            init_expr = Some(expr);
        } else if let None = var_type {
            return Err(CompilerError::SyntaxError("Expected either a type decleration ':' or expression assigment '='".to_string()));
        }
        return Ok(Self { name: name, var_type: var_type, init_expr: init_expr, is_mut: is_mut, is_resolved: false });
    }
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("let") {
            return Ok(None);
        }
        Ok(Some(Self::arg_from_def(parser)?))
    }
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        let mut result: String = (if self.is_mut { "let mut " } else { "let " }).to_string();
        result += self.name.as_str();
        if let Some(var_type) = &self.var_type {
            result += format!(": {}", var_type.to_string()).as_str();
        }
        if let Some(init_expr) = &self.init_expr {
            result += format!(" = {};", init_expr.to_string()).as_str();
        }
        result
    }
}

struct Variables {
    variables: Vec<Variable>
}

impl Variables {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut variables: Vec<Variable> = Vec::new();
        parser.ensure_next("(")?;
        if !parser.is_next(")") {
            variables.push(Variable::arg_from_def(parser)?);
            while parser.is_next(",") {
                variables.push(Variable::arg_from_def(parser)?);                 
            }
            parser.ensure_next(")")?;
        }
        Ok(Self { variables })
    }
}

impl ToString for Variables {
    fn to_string(&self) -> String {
        format!("({})", self.variables.iter().map(|v: &Variable| v.to_string()).collect::<Vec<String>>().join(", ").as_str())
    }
}

impl Function {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("fun") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        let templates: Option<Templates> = Templates::is_from_def(parser)?;
        let args: Variables = Variables::from_def(parser)?;
        let return_type = if parser.is_next("->") {
            Some(VarType::from_def(parser)?)
        } else {
            None
        };
        let scope: Option<Scope> = if parser.is_next(";") { None } else { Some(Scope::from_def(parser)?) };
        Ok(Some(Self { name, templates, args, return_type, scope }))
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        return format!("fun {}{}{} {}{}",
            self.name,
            if let Some(templates) = self.templates.as_ref() { templates.to_string() } else { String::new() },
            self.args.to_string(),
            if let Some(return_type) = self.return_type.as_ref() { format!("-> {} ", return_type.to_string()) } else { String::new() },
            if let Some(scope) = self.scope.as_ref() { scope.to_string() } else { String::new() }
        )
    }
}

impl Statement {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if let Some(new_var) = Variable::is_from_def(parser)? {
            parser.ensure_next(";")?;
            return Ok(Statement::VarDeclaration(new_var));
        } else if let Some(cond_chain) = ConditionalChain::is_from_def(parser)? {
            return Ok(Statement::ConditionalChain(cond_chain));
        } else if let Some(control_flow) = ControlFlow::is_from_def(parser)? {
            parser.ensure_next(";")?;
            return Ok(Statement::ControlFlow(control_flow));
        } else {
            let expr: ExprNode = ExprNode::from_def(parser)?;
            let is_final_value: bool = !parser.is_next(";");
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
            objects: Vec::new(),
            return_type: None
        }
    }

    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if parser.cur_char()? == '{' {
            return Ok(Some(Scope::from_def(parser)?));
        }
        Ok(None)
    }

    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        parser.ensure_next("{")?;
        let mut result: Scope = Scope::new();
        while !parser.is_next("}") {
            result.parse_next(parser)?;
        }
        Ok(result)
    }

    pub fn parse_next(&mut self, parser: &mut Parser) -> Result<(), CompilerError> {
        if let Some(obj) = Object::is_from_def(parser)? {
            self.objects.push(obj);
        } else if let Some(fun) = Function::is_from_def(parser)? {
            self.functions.push(fun);
        } else {
            let statement: Statement = Statement::from_def(parser)?;
            self.statements.push(statement);
        }
        Ok(())
    }
}

impl Object {
    pub fn is_from_def(parser: &mut Parser) -> Result<Option<Self>, CompilerError> {
        if !parser.is_next("object") {
            return Ok(None);
        }
        let name: String = parser.next_name()?;
        let scope: Scope = Scope::from_def(parser)?;
        Ok(Some(Object { name, scope }))
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::VarDeclaration(variable) => {
                variable.to_string()
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
        }
    }
}

impl ToString for Scope {
    fn to_string(&self) -> String {
        let mut indented_str: String = String::new();
        if !self.objects.is_empty() {
            indented_str += self.objects.iter().map(|obj: &Object| obj.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        if !self.functions.is_empty() {
            indented_str += self.functions.iter().map(|fun: &Function| fun.to_string()).collect::<Vec<String>>().join("\n").as_str();
            indented_str += "\n";
        }
        indented_str += self.statements.iter().map(|stat: &Statement| stat.to_string()).collect::<Vec<String>>().join("\n").as_str();
        return format!("{{\n{}\n}}", indent_each_line(indented_str.as_str()));
    }
}

impl ToString for Object {
    fn to_string(&self) -> String {
        return format!("object {} ", self.name) + self.scope.to_string().as_str();
    }
}

pub struct Parser {
    files: HashMap<String, Vec<char>>,
    cur_file_path: String,
    index: usize,
    new_line_index: usize,
    cur_line: usize
}

impl Parser {
    pub fn new() -> Self {
        Parser {
            files: HashMap::new(),
            cur_file_path: String::new(),
            index: 0,
            new_line_index: 0,
            cur_line: 0
        }
    }

    pub fn cur_file(&self) -> &Vec<char> {
        &self.files[&self.cur_file_path]
    }

    pub fn parse_file(&mut self, path: &str, main_scope: &mut Scope) -> Result<(), CompilerError> {
        if self.files.contains_key(path) {
            return Ok(());
        }
        let contents: String = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(e) => {
                return Err(CompilerError::LinkerError(format!("Failed to import file {}, {}", path, e)));
            }
        };
        let saved_file_path: String = self.cur_file_path.clone();
        let saved_index: usize = self.index;
        let saved_cur_line: usize = self.cur_line;
        self.cur_file_path = path.to_string();
        self.index = 0;
        self.cur_line = 0;
        self.files.insert(self.cur_file_path.clone(), contents.chars().collect());
        self.parse(main_scope)?;
        self.cur_file_path = saved_file_path;
        self.index = saved_index;
        self.cur_line = saved_cur_line;
        Ok(())
    }

    pub fn parse(&mut self, main_scope: &mut Scope) -> Result<(), CompilerError> {
        self.skip_whitespace()?;
        while self.is_next("import") {
            let path: String = self.next_until(|ch| ch == ';')?;
            self.parse_file(path.as_str(), main_scope)?;
        }
        while !self.is_finished() {
            main_scope.parse_next(self)?;
        }
        Ok(())
    }

    pub fn cur_char(&self) -> Result<char, CompilerError> {
        let file_text: &Vec<char> = self.files.get(&self.cur_file_path).unwrap();
        if let Some(ch) = file_text.get(self.index).copied() {
            return Ok(ch);
        }
        return Err(CompilerError::SyntaxError("Script ended too early".to_string()));
    }

    pub fn is_name_start(&self) -> Result<bool, CompilerError> {
        let ch = self.cur_char()?;
        Ok((ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z') || ch == '_')
    }

    pub fn is_num_start(&self) -> Result<bool, CompilerError> {
        let ch = self.cur_char()?;
        Ok(ch >= '0' && ch <= '9')
    }

    pub fn next_until<F>(&mut self, end_condition: F) -> Result<String, CompilerError> where F: Fn(char) -> bool {
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
        self.skip_whitespace()?;
        Ok(result)
    }

    pub fn next_name(&mut self) -> Result<String, CompilerError> {
        let start_index: usize = self.index;
        let ch: char = self.cur_char()?;
        if !self.is_name_start()? {
            return Err(CompilerError::SyntaxError(format!("Expected a name, starting by 'a'-'z', 'A'-'Z' or '_'. found char '{}' instead", ch)));
        }
        self.index += 1;
        while let Ok(ch) = self.cur_char() {
            if !(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch >= '0' && ch <= '9') && ch != '_' {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.skip_whitespace()?;
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
        let s_chars: Vec<char> = s.chars().collect();
        let end: usize = self.index + s_chars.len();
        if end > self.cur_file().len() {
            return false;
        }
        let result: bool = self.cur_file()[self.index..end] == s_chars;
        if result {
            self.index = end;
        }
        self.skip_whitespace();
        result
    }

    pub fn ensure_next(&mut self, s: &str) -> Result<(), CompilerError> {
        if !self.is_next(s) {
            return Err(CompilerError::SyntaxError(format!("Expected '{}'", s)));
        }
        Ok(())
    }

    pub fn index_error(&self, err: CompilerError) -> CompilerError {
        let indexing_str = format!(
            "Error in file '{}' at line {}, column {}: ",
            self.cur_file_path,
            self.cur_line + 1,
            self.index - self.new_line_index + 1,
        );
        match err {
            CompilerError::LinkerError(err) => CompilerError::LinkerError(format!("{}{}", indexing_str, err)),
            CompilerError::SemanticError(err) => CompilerError::SemanticError(format!("{}{}", indexing_str, err)),
            CompilerError::SyntaxError(err) => CompilerError::SyntaxError(format!("{}{}", indexing_str, err)),
        }
    }
}

pub fn indent_each_line(input: &str) -> String {
    input
        .lines()
        .map(|line| format!("    {}", line))
        .collect::<Vec<String>>()
        .join("\n")
}

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

    println!("Done!");
    Ok(())
}
