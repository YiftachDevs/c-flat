use std::{collections::HashMap, env, fs, path::Path, process};

#[derive(Clone, PartialEq)]
pub enum VarType {
    Void,
    Int,
    Size,
    Byte,
    Num,
    Bool,
    Char
    // Array(Box<VarType>, u32),
    // Tuple(Vec<VarType>),
    // Callback(Vec<VarType>, Box<VarType>)
}

pub enum ConstValue {
    Int(i32),
    Size(u32),
    Byte(u32),
    Num(f32),
    Bool(bool),
    Char(char)
}

pub struct Variable {
    name: String,
    var_type: VarType
}

pub struct Function {
    name: String,
    args: Vec<Variable>,
    return_type: VarType,
}

pub enum Statement {
    Unknown,
    VarDecleration,
    Expression,
    WhileLoop,
}

pub enum InfixOpr {
    Unknown,
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

#[derive(PartialEq)]
pub enum PrefixOpr {
    Unknown,
    Not,
    LNot,
    Addr,
    Deref
}

pub enum PostfixOpr {
    Unknown,
    Inv,
    Idx,
    Mem,
}

pub enum ExprNode {
    InfixOpr(InfixOpr, Box<ExprNode>, Box<ExprNode>),
    PrefixOpr(PrefixOpr, Box<ExprNode>),
    PostfixOpr(PostfixOpr, Box<ExprNode>, Box<ExprNode>),
    Const(String),
    Variable(String),
    Member(String)
}

pub struct Expression {
    var_type: VarType
}

pub struct Object {
    name: String,
    globals: Vec<Variable>,
    statements: Vec<Statement>,
    functions: Vec<Function>,
    objects: Vec<Object>
}

impl InfixOpr {
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
}

impl PrefixOpr {
    pub fn from_def(parser: &mut Parser) -> Option<Self> {
        let mut result: PrefixOpr = PrefixOpr::Unknown;
        let ch: char = parser.cur_char();
        result = match ch {
            '~' => PrefixOpr::Not,
            '!' => PrefixOpr::LNot,
            '&' => PrefixOpr::Addr,
            '*' => PrefixOpr::Deref,
            _ => { return None; }
        }
        Some(result)
    }
}

impl VarType {
    pub fn from_def(parser: &mut Parser, main_object: &Object) -> Self {
        if parser.is_next("Int") {
            return VarType::Int;
        }
        if parser.is_next("Num") {
            return VarType::Num;
        }
        if parser.is_next("Bool") {
            return VarType::Bool;
        }
        if parser.is_next("Size") {
            return VarType::Size;
        }
        if parser.is_next("Byte") {
            return VarType::Byte;
        }
        if parser.is_next("Char") {
            return VarType::Char;
        }
        parser.error("Unknown type");
        VarType::Void
    }

    pub fn to_script(&self) -> String {
        match self {
            VarType::Void => "Void".to_string(),
            VarType::Int => "Int".to_string(),
            VarType::Num => "Num".to_string(),
            VarType::Size => "UInt".to_string(),
            VarType::Byte => "Byte".to_string(),
            VarType::Bool => "Bool".to_string(),
            VarType::Char => "Char".to_string(),
            _ => "Unknown".to_string()
            // VarType::Callback(args, return_type) => format!("({}) -> {}", args.iter().map(|x| x.to_script()), return_type.to_script())
        }
    }
}

impl ConstValue {
    pub fn from_def(parser: &mut Parser, main_object: &Object) -> Option<Self> {
        if parser.is_next("false") {
            return Some(ConstValue::Bool(false));
        } else if parser.is_next("true") {
            return Some(ConstValue::Bool(true));
        }
        let mut negated = false;
        if parser.cur_char() == '-' {
            negated = true;
            parser.index += 1;
        }
        if let Some(num) = parser.next_numeral() {

        }
        None
    }

    fn numeral_from_def(&mut self, parser: &mut Parser) -> Option<Self> {
        let start_index: usize = parser.index;
        let ch: char = parser.cur_char();
        if !(ch >= '0' && ch <= '9') {
            return None;
        }
        enum NumType { Dec, Hex, Bin }
        let mut num_type: NumType = NumType::Dec;
        if ch == '0' {
            parser.index += 1;
            let ch: char = parser.cur_char();
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
        while let ch = parser.cur_char() {
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
        parser.skip_whitespace();
        value
    }
}

impl ExprNode {
    pub fn from_def(parser: &mut Parser, main_object: &Object) -> Self {
        parser.ensure_next("0");
        Expression { var_type: VarType::Int }
    }

    pub fn primary_from_def(parser: &mut Parser, main_object: &Object) -> Self {
        if let Some(prefix_opr) = PrefixOpr::from_def(parser) {
            return ExprNode::PrefixOpr(prefix_opr, Box::new(ExprNode::primary_from_def(parser, main_object)));
        } else if let Some(const_value) = ConstValue::from_def(parser, main_object) {

        }
        return ExprNode::Variable("".to_string());
    }
}

impl Variable {
    pub fn from_def(parser: &mut Parser, main_object: &Object) -> (Variable, Option<ExprNode>) {
        let name: String = parser.next_word();
        let mut result_var: Variable = Variable { name, var_type: VarType::Void };
        let mut optional_expression = None;
        let mut known_type: bool = false;
        if parser.is_next(":") {
            result_var.var_type = VarType::from_def(parser, main_object);
            known_type = true;
        }
        if parser.is_next("=") {
            let expr: ExprNode = ExprNode::from_def(parser, main_object);
            if known_type {
                if result_var.var_type != expr.var_type {
                    parser.error(format!("Type mismatch during variable creation, expected '{}', got '{}'", result_var.var_type.to_script(), expr.var_type.to_script()).as_str());
                }
            } else {
                result_var.var_type = expr.var_type.clone();
            }
            optional_expression = Some(expr);
        } else if !known_type {
            parser.error("Expected either a type decleration ':' or expression assigment '='");
        }
        (result_var, optional_expression)
    }
}

impl Object {
    pub fn new() -> Self {
        Object {
            name: String::new(),
            globals: Vec::new(),
            statements: Vec::new(),
            functions: Vec::new(),
            objects: Vec::new()
        }
    }

    pub fn from_def(parser: &mut Parser, main_object: &Object) -> Self {
        let mut result: Object = Self::new();
        let name: String = parser.next_word();
        result.name = name;
        parser.ensure_next("{");
        while !parser.is_next("}") {
            result.parse_next(parser);
        }
        result
    }

    pub fn parse_next(&mut self, parser: &mut Parser, main_object_box: Option<&Object>) {
        let main_object: &Object = main_object_box.unwrap_or(self);
        let is_var: bool = parser.is_next("var");
        let is_val: bool = parser.is_next("val");
        let is_new_var: bool = is_var ^ is_val;

        if parser.is_next("object") {
            self.objects.push(Object::from_def(parser, main_object));
        } else if is_new_var {
            let new_var: (Variable, Option<ExprNode>) = Variable::from_def(parser, main_object);
            parser.ensure_next(";");
        }
        else {
            parser.error("Unknown statement");
        }
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

    pub fn parse_file(&mut self, path: &str, main_object: &mut Object) {
        if self.files.contains_key(path) {
            return;
        }
        let contents: String = match fs::read_to_string(path) {
            Ok(text) => text,
            Err(e) => {
                self.error(format!("Failed to import file {}, {}", path, e).as_str());
                return;
            }
        };
        let saved_file_path: String = self.cur_file_path.clone();
        let saved_index: usize = self.index;
        let saved_cur_line: usize = self.cur_line;
        self.cur_file_path = path.to_string();
        self.index = 0;
        self.cur_line = 0;
        self.files.insert(self.cur_file_path.clone(), contents.chars().collect());
        self.parse(main_object);
        self.cur_file_path = saved_file_path;
        self.index = saved_index;
        self.cur_line = saved_cur_line;
    }

    pub fn parse(&mut self, main_object: &mut Object) {
        self.skip_whitespace();
        while self.is_next("import") {
            let path: String = self.next_until(';');
            self.parse_file(path.as_str(), main_object);
        }
        while !self.is_finished() {
            main_object.parse_next(self, None);
        }
    }

    pub fn cur_char(&self) -> char {
        let file_text: &Vec<char> = self.files.get(&self.cur_file_path)?;
        if let Some(ch) = file_text.get(self.index).copied() {
            return ch;
        }
        self.error("Script ended too early");
        return 'e';
    }

    pub fn next_until(&mut self, end: char) -> String {
        let start_index: usize = self.index;
        while let ch = self.cur_char() {
            if ch == end {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.index += 1;
        self.skip_whitespace();
        result
    }

    pub fn next_word(&mut self) -> String {
        let start_index: usize = self.index;
        let ch: char = self.cur_char();
        if !ch.is_alphabetic() && ch != '_' {
            self.error("Expected a word");
            return "".to_string();
        }
        self.index += 1;
        while let ch = self.cur_char() {
            if !ch.is_alphabetic() && ch != '_' && !(ch >= '0' && ch <= '9') {
                break
                // self.error(format!("Char {} is not a valid char for a name", ch).as_str());
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.skip_whitespace();
        result
    }

    pub fn skip_whitespace(&mut self) {
        while let ch = self.cur_char() {
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

    pub fn skip_if_comment(&mut self) {
        if let ch = self.cur_char() {
            if ch != '/' {
                return;
            }
            self.index += 1;
        }
        if let ch = self.cur_char() {
            if ch == '/' {
                while let ch = self.cur_char() {
                    if ch == '\n' {
                        break;
                    }
                    self.index += 1;
                }          
            } 
        }
        let s_chars: Vec<char> = "//".chars().collect();
        let end: usize = self.index + s_chars.len();
        if end > self.cur_file().len() {
            return;
        }
        let is_comment: bool = self.cur_file()[self.index..end] == s_chars;
        if !is_comment {
            return;
        }
        self.index = end;
        let start_index: usize = self.index;
        while let ch = self.cur_char() {
            if ch == '\n' {
                break;
            }
            self.index += 1;
        }
        let comment: String = self.cur_file()[start_index..self.index].iter().collect();
        self.index += 1;
        println!("comment: {}", comment);
    }

    pub fn is_next(&mut self, s: &str) -> bool {
        if self.is_finished() {
            self.error(format!("Script ended too early, expected '{}'", s).as_str());
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

    pub fn ensure_next(&mut self, s: &str) {
        if !self.is_next(s) {
            self.error(format!("Expected '{}'", s).as_str());
        }
    }

    pub fn error(&self, message: &str) {
        eprintln!(
            "Error in file '{}' at line {}, index {}: {}",
            self.cur_file_path,
            self.cur_line + 1,
            self.index - self.new_line_index,
            message
        );
        process::exit(1);
    }
}

fn main() {
    let str_path: &str = "test";
    let path: &Path = Path::new(str_path);
    if let Err(e) = env::set_current_dir(&path) {
        eprintln!("Error: Failed to open path '{}', {}", str_path, e);
    }

    let mut parser: Parser = Parser::new();
    let mut main_object: Object = Object::new();
    main_object.name = "main_object".to_string();

    parser.parse_file("main.cf", &mut main_object);
    println!("done!");
}
