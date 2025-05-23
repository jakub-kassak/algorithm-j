use crate::ast::Expr;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    pub fn parse(&mut self) -> Expr {
        self.expr(false, false)
    }

    fn expr(&mut self, in_app: bool, in_add: bool) -> Expr {
        let mut left = match &self.current_token {
            Token::TkLet => self.let_expr(),
            Token::TkIdent(_) => self.ident(),
            Token::TkUnit => self.unit(),
            Token::TkBackslash => self.lambda(),
            Token::TkLParen => {
                self.consume(Token::TkLParen);
                let expr = self.expr(false, false);
                self.consume(Token::RParen);
                expr
            }
            Token::TkNumber(n) => {
                let n = *n;
                self.consume(Token::TkNumber(n));
                Expr::Int(n)
            }
            _ => panic!("Unexpected token: {:?}", self.current_token),
        };

        // Look ahead for function application
        while !in_app
            && self.current_token != Token::RParen
            && self.current_token != Token::TkEof
            && self.current_token != Token::TkIn
            && self.current_token != Token::TkPlus
            && self.current_token != Token::TkSemicolon
        {
            let arg = self.expr(true, false);
            left = Expr::App {
                func: Box::new(left),
                arg: Box::new(arg),
            }
        }

        while !in_app && self.current_token == Token::TkPlus {
            self.consume(Token::TkPlus);
            let right = self.expr(false, true);
            left = Expr::Add {
                lhs: Box::new(left),
                rhs: Box::new(right),
            }
        } 

        if !in_app && !in_add && self.current_token == Token::TkSemicolon {
            self.consume(Token::TkSemicolon);
            let right = self.expr(false, false);
            left = Expr::Seq {
                lhs: Box::new(left),
                rhs: Box::new(right),
            }
        }

        left
    }

    fn let_expr(&mut self) -> Expr {
        self.consume(Token::TkLet);
        let ident = match &self.current_token {
            Token::TkIdent(name) => name.clone(),
            _ => panic!("Expected identifier, found: {:?}", self.current_token),
        };
        self.consume(Token::TkIdent(ident.clone())); // Consume identifier

        self.consume(Token::TkEq);
        let value = self.expr(false, false);

        self.consume(Token::TkIn);
        let body = self.expr(false, false);

        Expr::Let {
            ident,
            value: Box::new(value),
            body: Box::new(body),
        }
    }

    fn ident(&mut self) -> Expr {
        if let Token::TkIdent(name) = &self.current_token {
            let ident = name.clone();
            self.consume(Token::TkIdent(name.clone()));
            Expr::Var(ident)
        } else {
            panic!("Expected identifier, found: {:?}", self.current_token)
        }
    }

    fn unit(&mut self) -> Expr {
        self.consume(Token::TkUnit);
        Expr::Unit
    }

    fn lambda(&mut self) -> Expr {
        self.consume(Token::TkBackslash);
        let ident = match &self.current_token {
            Token::TkIdent(name) => name.clone(),
            _ => panic!("Expected identifier, found: {:?}", self.current_token),
        };
        self.consume(Token::TkIdent(ident.clone()));

        self.consume(Token::TkDot);
        let body = self.expr(false, false);

        Expr::Lam {
            ident,
            body: Box::new(body),
        }
    }

    fn consume(&mut self, token: Token) {
        if self.current_token == token {
            self.current_token = self.lexer.next_token();
        } else {
            panic!("Expected token {:?}, found {:?}", token, self.current_token);
        }
    }
}
