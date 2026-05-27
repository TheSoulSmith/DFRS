use crate::lexer::TokenKind::{Identifier, Index};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    col: usize
}

pub enum TokenKind {
    Identifier(String),

    Assign,
    AddAssign,
    SubAssign,
    MultAssign,
    DivAssign,
    
    Index(Vec<ExprToken>),
    Entry(String),

    Call(Vec<Token>),

    Extension(String),
}

pub enum ExprTokenKind {
    Add,
    Subtract,
    Multiply,
    Divide,

    StringLit(String),
    NumLit(String),

    Identifier(String),

    Index(Vec<ExprToken>),
    Entry(String),

    Call(Vec<Token>),

    Extension(String),
}

pub struct ExprToken {
    kind: ExprTokenKind,
    line: usize,
    col: usize,
}

pub struct Token {
    kind: TokenKind,
    line: usize,
    col: usize,
}

pub enum ParamType {
    String,
    Number,
    List,
    Dictionary,
    Any,
    Variable(Box<ParamType>),
    Unknown(String),
}

pub enum ParamDefault {
    String(String),
    Number(String),
    None,
}

pub enum StructValType {
    String,
    Number,
    List(Box<StructValType>),
    Dictionary(Vec<String>, Vec<StructValType>),
}

pub struct FunctionParam {
    kind: ParamType,
    name: String,
    plural: bool,
    optional: bool,
    default: ParamDefault
}

pub struct Function {
    toks: Vec<Token>,
    args: Vec<FunctionParam>,
    name: String,
    line: usize,
    col: usize
}

pub struct Struct {
    name: String,
    val_names: Vec<String>,
    val_types: Vec<StructValType>,
    impls: Vec<Function>,
    line: usize,
    col: usize
}

pub struct Lexed {
    structs: Vec<Struct>,
    funcs: Vec<Vec<Token>>
}

pub struct LexError {
    line: usize,
    col: usize,
    message: String
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer { source: source.chars().collect(), pos: 0, line: 1, col: 1 }
    }
    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }
    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }
    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied();
        if let Some(c) = ch {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        ch
    }
    pub fn main(&mut self) -> Result<Lexed, LexError> {
        let mut structs: Vec<Struct> = Vec::new();
        let mut funcs: Vec<Vec<Token>> = Vec::new();

        loop {
            let mut ident = String::new();
            loop {
                match self.peek() {
                    None => {return Ok(Lexed { structs, funcs });},
                    Some(c) => {
                        if c.is_whitespace() {
                            break;
                        }
                    }
                };
                self.advance();
            };

            let m_line = self.line;
            let m_col = self.col;

            loop {
                match self.peek() {
                    None => {return Err(LexError { line: m_line, col: m_col, message: "Unconcluded Identifier or block declaration".to_string() });}
                    Some(c) => {
                        if c.is_whitespace() {
                            break;
                        }
                    }
                };
                ident.push(self.peek().unwrap());
                self.advance();
            }

            match ident {
                _ => {return Err(LexError { line: m_line, col: m_col, message: "Unrecognized identifier".to_string() });}
            };
        };
    }
    fn lex_function(&mut self) -> Result<Function, LexError> {
        while match self.peek() {
            None => {return Err(LexError { line: self.line, col: self.col, message: "Expected something after `fn`".to_string() });}
            Some(c) => c.is_alphabetic()
        } {
            self.advance();
        };

        let mut name = String::new();
        while match self.peek() {
            None => {return Err(LexError { line: self.line, col: self.col, message: "Expected a function definition".to_string() });}
            Some(c) => c.is_alphabetic()
        } {
            name.push(self.peek().unwrap());
            self.advance();
        };

        match self.peek() {
            None => {return Err(LexError { line: self.line, col: self.col, message: "Expected opening parenthesis".to_string() });}
            Some('(') => {self.advance()}
            Some(_) =>{return Err(LexError { line: self.line, col: self.col, message: "Expected opening parenthesis".to_string() });}
        };

        let mut params: Vec<FunctionParam> = Vec::new();
        let p_line = self.line;
        let p_col = self.col;
        loop {
            match self.peek() {
                None => {return Err(LexError { line: p_line, col: p_col, message: "Unconcluded parameters".to_string() });}
                Some(c) if c.is_whitespace() => {self.advance(); continue;}
                Some(')') => {self.advance(); break;}
                Some(c) if c.is_alphabetic() => {}
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in function parameters".to_string() });}
            };
            let mut name = String::new();
            let t_line = self.line;
            let t_col = self.col;
            loop {
                match self.peek() {
                    None => {return Err(LexError { line: t_line, col: t_col, message: "Expected parameter type declaration".to_string() });}
                    Some(c) if c.is_alphabetic() => {
                        name.push(c);
                        self.advance();
                    }
                    Some(':') => {self.advance(); break;}
                    Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in function parameters".to_string() });}
                };
            };
            loop {
                match self.peek() {
                    None => {return Err(LexError { line: self.line, col: self.col, message: "EOF is not a parameter type, and I am starting to get tired of coding all of these bizarre errors".to_string() });}
                    Some(c) if c.is_whitespace() => {self.advance();}
                    Some(c) if c.is_alphabetic() => {break;}
                    Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in function parameters".to_string() });}
                };
            };
            let mut type_ident = String::new();
            let mut plural = false;
            let mut optional = false;
            loop {
                match self.peek() {
                    None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                    Some(c) if c.is_alphabetic() => {
                        type_ident.push(c);
                        self.advance();
                    }
                    Some(',') => {self.advance(); break;}
                    Some(c) if c.is_whitespace() => {self.advance(); break;}
                    Some('?') => {optional = true; self.advance();}
                    Some('+') => {plural = true; self.advance();}
                    Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in function parameters".to_string() });}
                };
            };
            let mut default = ParamDefault::None;
            if optional && !plural && type_ident != "Variable".to_string() && type_ident != "Var".to_string() {
                loop {
                    match self.peek() {
                        None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                        Some(c) if c.is_whitespace() => {self.advance();}
                        Some('=') => {
                            self.advance();
                            default = match self.lex_param_default() {
                                Err(e) => {return Err(e);}
                                Ok(t) => t
                            };
                            break;
                        }
                        Some(c) if c.is_alphabetic() => {break;}
                        Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in parameter declaration".to_string() });}
                    };
                };
            }
            params.push(FunctionParam { kind: match type_ident.as_str() {
                "string" => ParamType::String,
                "str" => ParamType::String,
                "number" => ParamType::Number,
                "num" => ParamType::Number,
                "List" => ParamType::List,
                "Dictionary" => ParamType::Dictionary,
                "Dict" => ParamType::Dictionary,
                "Any" => ParamType::Any,
                "Variable"|"Var" => ParamType::Variable(Box::new(match self.lex_varto() {
                    Err(e) => {return Err(e);}
                    Ok(t) => t
                })),
                c => ParamType::Unknown(c.to_string())
            }, name, plural, optional, default });
        };
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Expected opening curly brace to begin function script".to_string() });}
                Some(c) if c.is_whitespace() => {self.advance();}
                Some('{') => {break;}
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Expected opening curly brace to begin function script".to_string() });}
            };
        };
        self.advance();
        let mut toks: Vec<Token> = Vec::new();
        loop {
            let t_line = self.line;
            let t_col = self.col;
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded function".to_string() });}
                Some(c) if c.is_whitespace() => {self.advance();}
                Some(c) if c.is_alphabetic() => {
                    toks.push(Token { kind: Identifier(self.lex_identifier()), line: t_line, col: t_col });
                }
                Some('[') => {
                    toks.push(Token { kind: Index(match self.lex_index() {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    }), line: t_line, col: t_col });
                }

                Some(_) => {
                    return Err(LexError { line: t_line, col: t_col, message: "Unexpected character".to_string() });
                }
            };
        };
    }
    fn lex_varto(&mut self) -> Result<ParamType, LexError> {
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                Some(c) if c.is_whitespace() => {self.advance();}
                Some('-') => {break;}
                Some(c) if c.is_alphabetic() => {return Ok(ParamType::Any);}
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in parameter declaration".to_string() });}
            };
        };
        if self.peek() != Some('>') {
            return Err(LexError { line: self.line, col: self.col, message: "Expected \">\" to continue output type declaration".to_string() });
        }
        self.advance();
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                Some(c) if c.is_whitespace() => {self.advance();}
                Some(c) if c.is_alphabetic() => {break;}
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in parameter declaration".to_string() });}
            };
        };
        let mut type_name = String::new();
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                Some(c) if c.is_alphabetic() => {
                    type_name.push(c);
                    self.advance();
                }
                Some(c) if c.is_whitespace() => {break;}
                Some(',') => {
                    self.advance();
                    break;
                }
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in parameter declaration".to_string() });}
            };
        };
        Ok(match type_name.as_str() {
            "string" => ParamType::String,
                "str" => ParamType::String,
                "number" => ParamType::Number,
                "num" => ParamType::Number,
                "List" => ParamType::List,
                "Dictionary" => ParamType::Dictionary,
                "Dict" => ParamType::Dictionary,
                "Any" => ParamType::Any,
                c => ParamType::Unknown(c.to_string())
        })
    }
    fn lex_param_default(&mut self) -> Result<ParamDefault, LexError> {
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                Some('"') => {
                    return Ok(ParamDefault::String(match self.lex_string_lit() {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    }));
                }
                Some(c) if c.is_numeric() => {
                    return Ok(ParamDefault::Number(self.lex_num_lit()));
                }
                Some(_) => {return Err(LexError { line: self.line, col: self.col, message: "Unexpected character in parameter declaration".to_string() });}
            };
        };
    }
    fn lex_string_lit(&mut self) -> Result<String, LexError> {
        self.advance();
        let mut o = String::new();
        loop {
            match self.peek() {
                None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded string".to_string() });}
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    self.advance();
                    match self.peek() {
                        None => {return Err(LexError { line: self.line, col: self.col, message: "Unconcluded parameters".to_string() });}
                        Some('n') => {
                            self.advance();
                            o.push('\n');
                        }
                        Some(c) => {o.push(c);}
                    }
                }
                Some(c) => {o.push(c);}
            };
        };
        Result::Ok(o)
    }
    fn lex_num_lit(&mut self) -> String {
        let mut o = String::new();
        let mut point = true;
        loop {
            match self.peek() {
                None => {break;}
                Some(c) if c.is_numeric() => {
                    o.push(c);
                    self.advance();
                }
                Some('.') if point => {
                    point = !point;
                    o.push('.');
                }
                Some(_) => {break;}
            };
        };
        self.advance();
        o
    }
    fn lex_identifier(&mut self) -> String {
        let mut o = String::new();
        loop {
            match self.peek() {
                None => {break;}
                Some(c) if c.is_alphabetic() => {
                    o.push(c);
                    self.advance();
                }
                Some(_) => {break;}
            };
        };
        self.advance();
        o
    }
    fn lex_index(&mut self) -> Result<Vec<ExprToken>, LexError> {
        self.advance();
        let toks = self.lex_expr();
        match self.peek() {
            Some(']') => toks,
            _ => Err(LexError { line: self.line, col: self.col, message: "Unconcluded index".to_string() })
        }
    }
    fn lex_expr(&mut self) -> Result<Vec<ExprToken>, LexError> {
        let mut toks: Vec<ExprToken> = Vec::new();
        loop {
            let t_line = self.line;
            let t_col = self.col;
            match self.peek() {
                None => {break;}
                Some('"') => {
                    toks.push(match self.lex_string_lit() {
                        Err(e) => {return Err(e);}
                        Ok(t) => ExprToken { kind: ExprTokenKind::StringLit(t), line: t_line, col: t_col }
                    });
                }
                Some(c) if c.is_numeric() => {
                    toks.push(ExprToken { kind: ExprTokenKind::NumLit(self.lex_num_lit()), line: t_line, col: t_col });
                }
                Some(c) if c.is_whitespace() => {
                    self.advance();
                }
                Some('+') => {
                    toks.push(ExprToken { kind: ExprTokenKind::Add, line: t_line, col: t_col });
                    self.advance();
                }
                Some('-') => {
                    toks.push(ExprToken { kind: ExprTokenKind::Subtract, line: t_line, col: t_col });
                    self.advance();
                }
                Some('/') => {
                    toks.push(ExprToken { kind: ExprTokenKind::Divide, line: t_line, col: t_col });
                    self.advance();
                }
                Some('*') => {
                    toks.push(ExprToken { kind: ExprTokenKind::Multiply, line: t_line, col: t_col });
                    self.advance();
                }
                Some(c) if c.is_alphabetic() => {
                    toks.push(ExprToken { kind: ExprTokenKind::Identifier(self.lex_identifier()), line: t_line, col: t_col });
                }
                Some('[') => {
                    toks.push(ExprToken { kind: ExprTokenKind::Index(match self.lex_index() {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    }), line: t_line, col: t_col });
                }
                Some('.') => {
                    self.advance();
                    toks.push(ExprToken { kind: ExprTokenKind::Entry(self.lex_identifier()), line: t_line, col: t_col });
                }

                Some(_) => {break;}
            };
        };
        Ok(toks)
    }
}