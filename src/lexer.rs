use std::{char, vec};

#[derive(Debug)]
pub enum TokenKind {
    RepOpen,
    IfOpen,
    RepClose,
    IfClose,
    Block(BlockType, String, String, Vec<Expr>)
}

#[derive(Debug)]
pub enum BlockType {
    Control,
    Select,
    Repeat,
    Else,
    IfPlayer,
    Player,
    Call,
    Set,
    IfVar,
    Game,
    IfGame,
    Start,
    IfEntity,
    Entity,
}

#[derive(Debug)]
pub enum ValType {
    Str,
    Num,
    STxt,
    Loc,
    Vec,
    List(Box<ValType>),
    Dict(Vec<String>, Vec<ValType>),
}

#[derive(Debug)]
pub struct Struct {
    data_names: Vec<String>,
    data_types: Vec<ValType>,
    impls: Vec<Vec<Token>>
}

#[derive(Debug)]
pub enum VarScope {
    Game,
    Save,
    Local,
    Line
}

#[derive(Debug)]
pub enum Expr {
    Str(String),
    Num(f64),
    NumExpr(String),
    Var(VarScope, String),
    STxt(String),
    Loc(f64, f64, f64, f64, f64),
    Vect(f64, f64, f64)
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    line: usize,
    col: usize
}

#[derive(Debug)]
enum BracketKind {
    Rep,
    If
}

impl BracketKind {
    pub fn open(&self) -> TokenKind {
        match self {
            BracketKind::If => TokenKind::IfOpen,
            BracketKind::Rep => TokenKind::RepOpen
        }
    }
    pub fn close(&self) -> TokenKind {
        match self {
            BracketKind::If => TokenKind::IfClose,
            BracketKind::Rep => TokenKind::RepClose
        }
    }
}

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    bracket_type_stack: Vec<BracketKind>,
    line: usize,
    col: usize
}

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
    pub line: usize,
    pub col: usize
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer { source: source.chars().collect(), pos: 0, bracket_type_stack: vec![], line: 0, col: 0 }
    }
    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }
    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }
    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied();
        match ch {
            Some('\n') => {
                self.col = 1;
                self.line += 1;
            }
            _ => {}
        };
        self.pos += 1;
        ch
    }
    fn make_token(&self, kind: TokenKind) -> Token {
        Token { kind: kind, line: self.line, col: self.col }
    }
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            match self.peek() {
                None => {
                    break;
                }
                Some('{') => {
                    tokens.push(self.make_token(self.bracket_type_stack.last().unwrap().open()));
                    self.advance();
                }
                Some('}') => {
                    let append = match self.bracket_type_stack.pop() {
                        Some(t) => t,
                        None => {
                            break;
                        }
                    };
                    tokens.push(self.make_token(append.close()));
                    self.advance();
                }
                Some(c) if c.is_alphabetic() => {
                    match self.lex_block() {
                        Result::Ok(t) => {tokens.push(t);}
                        Result::Err(e) => {return Err(e);}
                    };
                }
                Some(_) => {self.advance();}
            }
        }
        Ok(tokens)
    }
    fn lex_block(&mut self) -> Result<Token,LexError> {
        let line = self.line;
        let col = self.col;
        let mut ident;
        ident = String::new();
        while matches!(self.peek(), Some(c) if c.is_alphabetic()) {
            let ch = match self.advance() {
                Some(t) => t,
                None => ' '
            };
            ident.push(ch);
        };
        match self.peek() {
            None => {return Err(LexError { msg: "unconcluded line".to_string(), line:line, col:col });}
            Some(_) => {}
        };
        let block: BlockType = match ident.as_str() {
            "cont" => BlockType::Control,
            "sel" => BlockType::Select,
            "loop" => {
                self.bracket_type_stack.push(BracketKind::Rep);
                BlockType::Repeat
            },
            "else" => BlockType::Else,
            "ifp" => {
                self.bracket_type_stack.push(BracketKind::If);
                BlockType::IfPlayer
            }
            "p" => BlockType::Player,
            "call" => BlockType::Call,
            "set" => BlockType::Set,
            "ifv" => {
                self.bracket_type_stack.push(BracketKind::If);
                BlockType::IfVar
            }
            "game" => BlockType::Game,
            "ifg" => {
                self.bracket_type_stack.push(BracketKind::If);
                BlockType::IfGame
            }
            "start" => BlockType::Start,
            "ife" => {
                self.bracket_type_stack.push(BracketKind::If);
                BlockType::IfEntity
            }
            "e" => BlockType::Entity,
            _ => {
                return Err(LexError { msg: std::fmt::format(format_args!("invalid block type: {}", ident)), line, col });
            }
        };
        self.advance();
        ident = String::new();
        if matches!(self.peek(), Some('"')) {
            ident = match self.lex_string() {
                Err(e) => {return Err(e);}
                Ok(t) => t
            }
        } else {
            while matches!(self.peek(), Some(c) if c.is_alphabetic() || c=='.' || c=='/' || c=='-' || c=='+' || c=='=' || c=='%' || c=='!') {
                ident.push(self.advance().unwrap());
            };
        };
        let mut sub = String::new();
        if matches!(self.peek(), Some(' ')) {
            self.advance();
            loop {
                match self.peek() {
                    None => {return Err(LexError { msg: "Unconcluded line".to_string(), line, col });}
                    Some(c) if c.is_alphabetic() || c=='.' || c=='/' || c=='-' || c=='+' || c=='=' || c=='%' || c=='!' => {
                        sub.push(c);
                        self.advance();
                    }
                    Some(_) => {break;}
                };
            };
        };
        let mut args: Vec<Expr> = Vec::new();
        if matches!(self.peek(), Some('(')) {
            self.advance();
            match self.lex_args() {
                Err(e) => {return Err(e);}
                Ok(t) => {args = t;}
            };
        };
        Ok(Token { kind: TokenKind::Block(block, ident, sub, args), line: line, col: col })
    }
    fn lex_args(&mut self) -> Result<Vec<Expr>, LexError> {
        let mut exprs = Vec::new();
        let m_line = self.line;
        let m_col = self.col;
        loop {
            match self.peek() {
                None => {
                    return Err(LexError { msg: "arguments are never closed".to_string(), line: m_line, col: m_col });
                }
                Some('"') => {
                    exprs.push(Expr::Str(match self.lex_string() {
                        Ok(t) => t,
                        Err(e) => {return Err(e);}
                    }));
                }
                Some('!') => {
                    self.advance();
                    exprs.push(Expr::STxt(match self.lex_string() {
                        Ok(t) => t,
                        Err(e) => {return Err(e);}
                    }));
                }
                Some('#') => {
                    self.advance();
                    exprs.push(Expr::NumExpr(match self.lex_string() {
                        Ok(t) => t,
                        Err(e) => {return Err(e);}
                    }));
                }
                Some(')') => {break;}
                Some(c) if c.is_numeric() => {
                    self.advance();
                    exprs.push(Expr::Num(self.lex_number()));
                }
                Some('-') if match self.peek_ahead(1) {
                    None => {return Err(LexError { msg: "arguments are never closed".to_string(), line: m_line, col: m_col });}
                    Some(t) => t
                }.is_numeric() => {
                    self.advance();
                    exprs.push(Expr::Num(-self.lex_number()));
                }
                Some('g') => {
                    exprs.push(match self.lex_var(VarScope::Game) {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                }
                Some('s') => {
                    exprs.push(match self.lex_var(VarScope::Save) {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                }
                Some('l') => {
                    exprs.push(match self.lex_var(VarScope::Local) {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                }
                Some('i') => {
                    exprs.push(match self.lex_var(VarScope::Line) {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                }
                Some('[') => {
                    exprs.push(match self.lex_loc() {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                    self.advance();
                }
                Some('<') => {
                    exprs.push(match self.lex_vec() {
                        Err(e) => {return Err(e);}
                        Ok(t) => t
                    });
                }
                Some(_) => {self.advance();}
            };
        };
        Ok(exprs)
    }
    fn lex_string(&mut self) -> Result<String, LexError> {
        let mut str = String::new();
        self.advance();
        let m_line = self.line;
        let m_col = self.col;
        loop {
            match self.peek() {
                None => {
                    return Err(LexError { msg: "Unconcluded string".to_string(), line: m_line, col: m_col });
                }
                Some('"') => {
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        None => {
                            return Err(LexError { msg: "string is never closed".to_string(), line: m_line, col: m_col });
                        }
                        Some('n') => {str.push('\n');}
                        Some(c) => {
                            str.push(c);
                        }
                    };
                }
                Some(s) => {
                    str.push(s);
                    self.advance();
                }
            };
        };
        self.advance();
        Ok(str)
    }
    fn lex_number(&mut self) -> f64 {
        let mut num: isize = 0;
        let mut point: i32 = 0;
        loop {
            match self.peek() {
                None => {break;}
                Some(c) if c.is_numeric() => {
                    num *= 10;
                    point += 1;
                    num += c.to_digit(10).expect("If this fires, something is very wrong") as isize;
                }
                Some('.') => {
                    point = 0;
                }
                Some(_) => {
                    self.advance();
                    break;
                }
            };
            self.advance();
        };
        let ten: f64 = 10.;
        num as f64 / ten.powi(point)
    }
    fn lex_var(&mut self, scope: VarScope) -> Result<Expr, LexError> {
        let m_line = self.line;
        let m_col = self.col;
        self.advance();
        match self.peek() {
            None => {return Err(LexError { msg: "arguments are never closed".to_string(), line: m_line, col: m_col });}
            Some('"') => {
                return Ok(Expr::Var(scope, match self.lex_string() {
                    Ok(t) => t,
                    Err(e) => {return Err(e);}
                }));
            }
            Some(_) => {
                let mut name = String::new();
                while matches!(self.peek(), Some(c) if c.is_alphabetic() || c =='_') {
                    name.push(match self.advance() {
                        None => {return Err(LexError { msg: "arguments are never closed".to_string(), line: m_line, col: m_col });}
                        Some(c) => c
                    });
                };
                return Ok(Expr::Var(scope, name));
            }
        };
    }
    fn lex_loc(&mut self) -> Result<Expr, LexError> {
        let m_line = self.line;
        let m_col = self.col;
        self.advance();
        let mut axis = [0.; 5];
        let mut index = 0;
        loop {
            match self.peek() {
                None => {return Err(LexError { msg: "unconcluded location".to_string(), line: m_line, col: m_col });}
                Some(']') => {break ;}
                Some(c) if c.is_numeric() => {
                    axis[index] = self.lex_number();
                    self.advance();
                    index += 1;
                    if index<4 {
                        return Err(LexError { msg: "locations only have 5 values".to_string(), line: m_line, col: m_col });
                    }
                }
                Some(_) => {self.advance();}
            };
        };
        Ok(Expr::Loc(axis[0], axis[1], axis[2], axis[3], axis[4]))
    }
    fn lex_vec(&mut self) -> Result<Expr, LexError> {
        let m_line = self.line;
        let m_col = self.col;
        self.advance();
        let mut axis = [0.; 3];
        let mut index = 0;
        loop {
            match self.peek() {
                None => {return Err(LexError { msg: "unconcluded location".to_string(), line: m_line, col: m_col });}
                Some('>') => {break;}
                Some(c) if c.is_numeric() => {
                    axis[index] = self.lex_number();
                    self.advance();
                    index += 1;
                    if index<2 {
                        return Err(LexError { msg: "vectors only have 3 values".to_string(), line: m_line, col: m_col });
                    }
                }
                Some(_) => {self.advance();}
            };
        };
        Ok(Expr::Vect(axis[0], axis[1], axis[2]))
    }
}