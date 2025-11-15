use std::{collections::HashMap, env, fs, path::Path, process};

pub struct Variable {
    name: String
}

pub struct Function {
    name: String
}

pub struct Module {
    name: String,
    globals: Vec<Variable>,
    functions: Vec<Function>,
    modules: Vec<Module>,
    templates: Vec<String>
}

impl Module {
    pub fn new() -> Self {
        Module {
            name: String::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            modules: Vec::new(),
            templates: Vec::new()
        }
    }

    pub fn from_def(parser: &mut Parser) -> Self {
        let mut result: Module = Self::new();
        let name: String = parser.next_word();
        result.name = name;
        parser.ensure_next("{");
        while !parser.is_next("}") {
            result.parse_next(parser);
        }
        result
    }

    pub fn parse_next(&mut self, parser: &mut Parser) {
        println!("l {} i {}", parser.cur_line, parser.index - parser.new_line_index);
        if parser.is_next("module") {
            self.modules.push(Module::from_def(parser));
        } else {
            parser.error("Unknown expression");
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

    pub fn parse_file(&mut self, path: &str, main_module: &mut Module) {
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
        self.parse(main_module);
        self.cur_file_path = saved_file_path;
        self.index = saved_index;
        self.cur_line = saved_cur_line;
    }

    pub fn parse(&mut self, main_module: &mut Module) {
        while self.is_next("import") {
            let path: String = self.next_until(';');
            self.parse_file(path.as_str(), main_module);
        }
        while !self.is_finished() {
            main_module.parse_next(self);
        }
    }

    pub fn cur_char(&self) -> Option<char> {
        let file_text: &Vec<char> = self.files.get(&self.cur_file_path)?;
        file_text.get(self.index).copied()
    }

    pub fn next_until(&mut self, end: char) -> String {
        self.skip_whitespace();
        let start_index: usize = self.index;
        while let Some(ch) = self.cur_char() {
            if ch == end {
                break;
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        self.index += 1;
        result
    }

    pub fn next_word(&mut self) -> String {
        self.skip_whitespace();
        let start_index: usize = self.index;
        while let Some(ch) = self.cur_char() {
            if !ch.is_alphabetic() && ch != '_' {
                break
                // self.error(format!("Char {} is not a valid char for a name", ch).as_str());
            }
            self.index += 1;
        }
        let result: String = self.cur_file()[start_index..self.index].iter().collect();
        result
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(ch) = self.cur_char() {
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
        self.skip_whitespace();
        self.index == self.cur_file().len()
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
    let mut main_module: Module = Module::new();
    main_module.name = "main_module".to_string();

    parser.parse_file("main.cf", &mut main_module);
}
