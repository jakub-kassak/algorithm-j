#[derive(Debug, PartialEq)]
pub enum Token {
    TkBackslash,
    TkDot,
    TkEof,
    TkEq,
    TkIdent(String),
    TkIn,
    TkLParen,
    TkLet,
    RParen,
    TkUnit,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn next(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            let ch = self.input[self.pos..].chars().next().unwrap();
            self.pos += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Token {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\n' => {
                    self.next();
                }
                '(' => {
                    self.next();
                    if let Some(')') = self.peek() {
                        self.next();
                        return Token::TkUnit;
                    }
                    return Token::TkLParen;
                }
                ')' => {
                    self.next();
                    return Token::RParen;
                }
                '=' => {
                    self.next();
                    return Token::TkEq;
                }
                '.' => {
                    self.next();
                    return Token::TkDot;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    return self.read_ident();
                }
                '0'..='9' => {
                    return self.read_unit();
                }
                '\\' => {
                    self.next();
                    return Token::TkBackslash;
                }
                _ => {
                    panic!("Unexpected character: {}", ch);
                }
            }
        }
        Token::TkEof
    }

    fn read_ident(&mut self) -> Token {
        let start = self.pos;
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                self.next();
            } else {
                break;
            }
        }
        let ident = &self.input[start..self.pos];
        match ident {
            "let" => Token::TkLet,
            "in" => Token::TkIn,
            _ => Token::TkIdent(ident.to_string()),
        }
    }

    fn read_unit(&mut self) -> Token {
        self.next();
        Token::TkUnit
    }
}
