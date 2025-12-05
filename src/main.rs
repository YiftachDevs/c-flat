use std::{collections::HashMap, env, fs, iter::Map, path::Path, process, thread::park_timeout_ms};

use crate::errors::CompilerError;
mod errors;

#[derive(PartialEq, Clone)]
pub struct NamePath {
    path: Vec<String>, // Game.Player.get_pos;
    templates: Option<TemplatesValues>
}

impl NamePath {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut path = Vec::new();
        while let Ok(name) = parser.next_name() {
            path.push(name);
            let ch: char = parser.cur_char()?;
            if ch != '.' { break; }
            parser.index += 1;
        }
        if path.is_empty() {
            return Err(CompilerError::SyntaxError("Expected a name path".to_string()));
        }
        let mut templates: Option<TemplatesValues> = None;
        if parser.is_next("<") {
            templates = Some(TemplatesValues::from_def(parser)?);
        }
        return Ok(NamePath { path, templates });
    }
}

#[derive(PartialEq, Clone)]
pub struct TemplatesValues {
    templates: Vec<VarType>
}

impl TemplatesValues {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut result: TemplatesValues = TemplatesValues { templates: Vec::new() };
        loop {
            let var_type: VarType = VarType::from_def(parser)?;
            result.templates.push(var_type);
            if parser.is_next(">") { break; }
            if !parser.is_next(",") {
                return Err(CompilerError::SyntaxError("Expected ',' or '>'".to_string()));
            }
        }
        return Ok(result);
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

#[derive(Clone, PartialEq)]
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
    is_resolved: bool
}

pub struct Function {
    name: String,
    args: Vec<Variable>,
    return_type: VarType,
}

pub enum Statement {
    VarDecleration(String),
    Expression,
    WhileLoop
}

#[derive(PartialEq, Clone, Copy)]
pub enum InfixOpr {
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

#[derive(PartialEq, Clone)]
pub enum ExprNode {
    InfixOpr(InfixOpr, Box<ExprNode>, Box<ExprNode>),
    PrefixOpr(PrefixOpr, Box<ExprNode>),
    PostfixOpr(PostfixOpr, Box<ExprNode>, Box<ExprNode>),
    NamePath(NamePath),
    ConstValue(ConstValue)
}

pub struct Expression {
    var_type: VarType
}

pub struct Object {
    name: String,
    globals: Vec<Variable>,
    statements: Vec<Statement>,
    functions: Vec<Function>,
    structs: Vec<Struct>,
    objects: Vec<Object>
}

pub struct Struct {
    name: String,
    vars: Vec<Variable>
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
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let result: InfixOpr = match ch {
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
            _ => { parser.index -= 1; return Err(CompilerError::SyntaxError("Unknown infix operator".to_string())); }
        };
        parser.skip_whitespace();
        Ok(result)
    }

    pub fn precedence(&self) -> i32 {
        match self {
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
        if *self == InfixOpr::Asn { false } else { true }
    }
}

impl ToString for InfixOpr {
    fn to_string(&self) -> String {
        match self {
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
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let ch: char = parser.cur_char()?;
        parser.index += 1;
        let result = match ch {
            '~' => PrefixOpr::Not,
            '!' => PrefixOpr::LNot,
            '&' => PrefixOpr::Addr,
            '*' => PrefixOpr::Deref,
            '-' => PrefixOpr::UMin,
            _ => { parser.index -= 1; return Err(CompilerError::SyntaxError("Expected prefix operator".to_string())); }
        };
        Ok(result)
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
        let name_path: NamePath = NamePath::from_def(parser)?;
        return Ok(VarType::Unresolved { name_path: name_path });
    }  
}

impl ToString for VarType {
    fn to_string(&self) -> String {
        return "TODO".to_string();
        // match self {
        //     VarType::Void => "Void".to_string(),
        //     VarType::Int => "Int".to_string(),
        //     VarType::Num => "Num".to_string(),
        //     VarType::Size => "UInt".to_string(),
        //     VarType::Byte => "Byte".to_string(),
        //     VarType::Bool => "Bool".to_string(),
        //     VarType::Char => "Char".to_string(),
        //     // VarType::Pointer(var_type) => format!("{}*"),
        //     _ => "Unknown".to_string()
        //     // VarType::Callback(args, return_type) => format!("({}) -> {}", args.iter().map(|x| x.to_string()), return_type.to_string())
        // }
    }
}

impl ConstValue {
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        if parser.is_next("false") {
            return Ok(ConstValue::Bool(false));
        } else if parser.is_next("true") {
            return Ok(ConstValue::Bool(true));
        } else if parser.is_next("'") {
            let ch: char = ConstValue::char_from_def(parser)?;
            parser.ensure_next("'")?;
            return Ok(ConstValue::Char(ch));
        } else if parser.is_next("\"") {
            let str: String = ConstValue::string_from_def(parser)?;
            return Ok(ConstValue::String(str));
        } else {
            let const_value = ConstValue::numeral_from_def(parser)?;
            return Ok(const_value);
        }
    }

    fn string_from_def(parser: &mut Parser) -> Result<String, CompilerError> {
        let mut result: String = String::new();
        loop {
            let ch: char = ConstValue::char_from_def(parser)?;
            parser.index += 1;
            if ch == '\"' {
                break;
            }
        }
        return Ok(result);
    }

    fn char_from_def(parser: &mut Parser) -> Result<char, CompilerError> {
        let mut result: char = '\0';
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
            if ch == 'x' {
                num_type = NumType::Hex; 
                parser.index += 1;
            } else if ch == 'b' {
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
                        value = value * 16 + (ch as u32 - 'a' as u32);
                    } else if ch >= 'A' && ch <= 'F' {
                        value = value * 16 + (ch as u32 - 'A' as u32);
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
        if parser.is_next(".") {
            let mut fractional_value: f32 = 0.0;
            let mut i: u32 = 1;
            loop {
                let ch: char = parser.cur_char()?;
                if ch >= '0' && ch <= '9' {
                    fractional_value += (ch as u32 - '0' as u32) as f32 / (10 as u32).pow(i) as f32;
                    i += 1;
                } else {
                    break
                }
            }
            parser.skip_whitespace();
            return Ok(ConstValue::Float(value as f32 + fractional_value));
        }
        parser.skip_whitespace();
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
    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut root: Box<ExprNode> = Box::new(ExprNode::primary_from_def(parser)?);
        if let Ok(infix_opr) = InfixOpr::from_def(parser) {
            let next_node: ExprNode = ExprNode::primary_from_def(parser)?;
            root = Box::new(ExprNode::InfixOpr(infix_opr, root, Box::new(next_node)));
        }
        while let Ok(infix_opr) = InfixOpr::from_def(parser) {
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
        if let Ok(prefix_opr) = PrefixOpr::from_def(parser) {
            return Ok(ExprNode::PrefixOpr(prefix_opr, Box::new(ExprNode::primary_from_def(parser)?)));
        } else if let Ok(const_value) = ConstValue::from_def(parser) {
            return Ok(ExprNode::ConstValue(const_value));
        } else if let Ok(name_path) = NamePath::from_def(parser) {
            return Ok(ExprNode::NamePath(name_path));
        } else if parser.is_next("(") {
            let inner_expr: ExprNode = ExprNode::from_def(parser)?;
            parser.ensure_next(")")?;
            return Ok(inner_expr);
        }
        return Err(CompilerError::SyntaxError("Unknown primary expression".to_string()));
    }
}

impl ToString for ExprNode {
    fn to_string(&self) -> String {
        match self {
            ExprNode::PrefixOpr(prefix_opr, node) => {
                format!("{}{}", prefix_opr.to_string(), node.to_string())
            },
            ExprNode::InfixOpr(infix_opr, left, right) => {
                format!("({}) {} ({})", left.to_string(), infix_opr.to_string(), right.to_string())
            },
            ExprNode::ConstValue(const_value) => {
                const_value.to_string()
            },
            _ => "?".to_string()
        }
    }
}

impl Variable {
    pub fn from_def(parser: &mut Parser) -> Result<Variable, CompilerError> {
        let name: String = parser.next_name()?;
        let mut var_type: Option<VarType> = None;
        let mut init_expr: Option<ExprNode> = None;
        if parser.is_next(":") {
            var_type = Some(VarType::from_def(parser)?);
        }
        if parser.is_next("=") {
            let mut expr: ExprNode = ExprNode::from_def(parser)?;
            println!("init expr: {}", expr.to_string());
            init_expr = Some(expr);
        } else if var_type == None {
            return Err(CompilerError::SyntaxError("Expected either a type decleration ':' or expression assigment '='".to_string()));
        }
        Ok(Variable { name: name, var_type: var_type, init_expr: init_expr, is_resolved: false })
    }
}

impl Object {
    pub fn new() -> Self {
        Object {
            name: String::new(),
            globals: Vec::new(),
            statements: Vec::new(),
            functions: Vec::new(),
            structs: Vec::new(),
            objects: Vec::new()
        }
    }

    pub fn from_def(parser: &mut Parser) -> Result<Self, CompilerError> {
        let mut result: Object = Self::new();
        let name: String = parser.next_name()?;
        result.name = name;
        parser.ensure_next("{")?;
        while !parser.is_next("}") {
            result.parse_next(parser)?;
        }
        Ok(result)
    }

    pub fn parse_next(&mut self, parser: &mut Parser) -> Result<(), CompilerError> {
        let is_var: bool = parser.is_next("var");
        let is_val: bool = parser.is_next("val");
        let is_new_var: bool = is_var ^ is_val;

        if parser.is_next("object") {
            self.objects.push(Object::from_def(parser)?);
        } else if is_new_var {
            let new_var: Variable = Variable::from_def(parser)?;
            parser.ensure_next(";")?;
            self.statements.push(Statement::VarDecleration(new_var.name.clone()));
            self.globals.push(new_var);
        } else {
            return Err(CompilerError::SyntaxError("Unknown statement".to_string()));
        }
        Ok(())
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

    pub fn parse_file(&mut self, path: &str, main_object: &mut Object) -> Result<(), CompilerError> {
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
        self.parse(main_object)?;
        self.cur_file_path = saved_file_path;
        self.index = saved_index;
        self.cur_line = saved_cur_line;
        Ok(())
    }

    pub fn parse(&mut self, main_object: &mut Object) -> Result<(), CompilerError> {
        self.skip_whitespace();
        while self.is_next("import") {
            let path: String = self.next_until(|ch| ch == ';');
            self.parse_file(path.as_str(), main_object)?;
        }
        while !self.is_finished() {
            main_object.parse_next(self)?;
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

    pub fn next_until<F>(&mut self, end_condition: F) -> String where F: Fn(char) -> bool {
        let start_index: usize = self.index;
        while let Ok(ch) = self.cur_char() {
            if end_condition(ch) {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.index += 1;
        self.skip_whitespace();
        result
    }

    pub fn next_name(&mut self) -> Result<String, CompilerError> {
        let start_index: usize = self.index;
        let ch: char = self.cur_char()?;
        if !(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z') && ch != '_' {
            return Err(CompilerError::SyntaxError(format!("Expected a name, starting by 'a'-'z', 'A'-'Z' or '_'. found char '{}' instead", ch)));
        }
        self.index += 1;
        while let Ok(ch) = self.cur_char() {
            if !(ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z') && ch != '_' {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.skip_whitespace();
        Ok(result)
    }

    pub fn skip_whitespace(&mut self) {
        while let Ok(ch) = self.cur_char() {
            if ch == '/' {
                self.skip_if_comment();
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
            "Error in file '{}' at line {}, index {}: ",
            self.cur_file_path,
            self.cur_line + 1,
            self.index - self.new_line_index,
        );
        match err {
            CompilerError::LinkerError(err) => CompilerError::LinkerError(format!("{}{}", indexing_str, err)),
            CompilerError::SemanticError(err) => CompilerError::SemanticError(format!("{}{}", indexing_str, err)),
            CompilerError::SyntaxError(err) => CompilerError::SyntaxError(format!("{}{}", indexing_str, err)),
        }
    }
}

fn main() -> Result<(), CompilerError> {
    let str_path: &str = "test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        return Err(CompilerError::LinkerError(format!("Failed to open working directory '{}', {}", str_path, e)));
    }

    let mut parser: Parser = Parser::new();
    let mut main_object: Object = Object::new();
    main_object.name = "main_object".to_string();

    if let Err(err) = parser.parse_file("main.cf", &mut main_object) {
        return Err(parser.index_error(err));
    }
    println!("Done!");
    Ok(())
}
