use miette::SourceSpan;

use super::lexer::{Lexer, Token};
use super::{Ast, ByteOffset, Expr, ParsingError};
use crate::scripting::AstNode;

const EXPECTED_STR: &str = "Expected a string";
const EXPECTED_IDENT: &str = "Expected an identifier";
const EXPECTED_LIT_OR_PARENS: &str = "Expected (, an identifier or a literal";
const INVALID_INT: &str = "That is an invalid integer";
const INVALID_FLOAT: &str = "That is an invalid float";

/*
#[derive(Debug, Error, Diagnostic)]
pub enum ParsingError {
    #[error("{0:?}: Expected `{1}`")]
    ExpectedKeyword(&'static str),
    #[error("Expected `{0}`, but found nothing")]
    ExpectedKeywordEOF(&'static str),
    #[error("{0:?}: Expected an identifier")]
    ExpectedIdent(ByteOffset),
    #[error("Expected an identifier")]
    ExpectedIdentEOF,
    #[error("{0:?}: Expected `{1}`")]
    ExpectedSymbol(ByteOffset, char),
    #[error("Expected `{0}`, but found nothing")]
    ExpectedSymbolEOF(char),
    #[error("{0:?}: Unexpected Token")]
    UnexpectedToken(ByteOffset),
    #[error("Unexpected Token")]
    UnexpectedTokenEof,
    #[error("{0:?}: Expected a String")]
    ExpectedString(ByteOffset),
    #[error("Expected a String")]
    ExpectedStringEof,
    #[error("{0:?}: Expected is not a valid integer")]
    InvalidInt(ByteOffset),
    #[error("{0:?}: Expected is not a valid float")]
    InvalidFloat(ByteOffset),
}*/

#[derive(Debug)]
pub struct Parser<'ast> {
    ast: &'ast mut Ast,
    lexer: Lexer,
    peek_buf: Vec<Token>,
    at_end: bool,
}

type TokenResult<V = Token> = Result<Option<V>, ParsingError>;

impl<'ast> Parser<'ast> {
    pub fn new(ast: &'ast mut Ast) -> Self {
        let lexer = Lexer::new(ast.source.clone());
        Self {
            ast,
            lexer,
            peek_buf: Vec::new(),
            at_end: false,
        }
    }

    pub fn parse_all(&mut self) -> Result<(), ParsingError> {
        while !self.is_at_end()? {
            let node = self.parse()?;
            self.ast.nodes.push(node);
        }
        Ok(())
    }

    fn err<T>(&self, help: impl Into<String>, pos: impl Into<SourceSpan>) -> Result<T, ParsingError> {
        Err(ParsingError {
            src: self.ast.source.clone(),
            at: pos.into(),
            help: help.into(),
        })
    }

    fn err_end<T>(&self, help: impl Into<String>) -> Result<T, ParsingError> {
        self.err(help, ByteOffset::from(self.ast.source.len()))
    }

    fn peek_n(&mut self, n: usize) -> TokenResult {
        while self.peek_buf.len() < n {
            match self.lexer.next() {
                Some(Ok(v)) => self.peek_buf.push(v),
                Some(Err(e)) => return Err(e),
                None => {
                    self.at_end = true;
                    return Ok(None);
                }
            }
        }
        Ok(Some(self.peek_buf[n - 1]))
    }

    fn peek(&mut self) -> TokenResult {
        self.peek_n(1)
    }

    fn expect_kw(&mut self, kw: &'static str) -> Result<ByteOffset, ParsingError> {
        let Some(v) = self.peek()? else { return self.err_end(format!("Expected `{kw}`")) };
        self.next().expect("it is guaranteed there's another token");
        match v {
            Token::Keyword(offset) if &self.ast.source[offset.range()] == kw => Ok(offset),
            t => self.err(format!("Expected `{kw}`"), t.loc()),
        }
    }

    fn expect_symbol(&mut self, sym: char) -> Result<ByteOffset, ParsingError> {
        let Some(v) = self.peek()? else { return self.err_end(format!("Expected `{sym}`")) };
        self.next().expect("it is guaranteed there's another token");
        match v {
            Token::Symbol(symbol, offset) if symbol == sym => Ok(offset),
            t => self.err(format!("Expected `{sym}`"), t.loc()),
        }
    }

    fn if_kw(&mut self, kw: &'static str) -> TokenResult<(ByteOffset, &'static str)> {
        match self.peek()? {
            Some(Token::Keyword(offset)) if &self.ast.source[offset.range()] == kw => {
                self.next().expect("it is guaranteed there's another token");
                Ok(Some((offset, kw)))
            }
            _ => Ok(None),
        }
    }

    fn if_symbol(&mut self, symbol: char) -> TokenResult<(ByteOffset, char)> {
        match self.peek()? {
            Some(Token::Symbol(sym, offset)) if sym == symbol => {
                self.next().expect("it is guaranteed there's another token");
                Ok(Some((offset, symbol)))
            }
            _ => Ok(None),
        }
    }

    fn is_at_end(&mut self) -> Result<bool, ParsingError> {
        Ok(self.peek()?.is_none())
    }

    fn next(&mut self) -> TokenResult {
        let v = self.peek();
        if let Ok(Some(_)) = v {
            self.peek_buf.remove(0);
        }
        v
    }

    fn skip_expr_end(&mut self) -> Result<(), ParsingError> {
        if let Some(Token::ExprEnd(..)) = self.peek()? {
            self.next().expect("it is guaranteed there's another token");
        }
        Ok(())
    }

    fn skip_expr_start(&mut self) -> Result<(), ParsingError> {
        if let Some(Token::ExprStart(..)) = self.peek()? {
            self.next().expect("it is guaranteed there's another token");
        }
        Ok(())
    }

    fn parse(&mut self) -> Result<AstNode, ParsingError> {
        if let Some(Token::Html(loc)) = self.peek()? {
            self.next().expect("it is guaranteed there's another token");
            return Ok(AstNode::Html(loc));
        }
        self.skip_expr_start()?;
        let node = 'outer: {
            if let Some(Token::Identifier(loc)) = self.peek()? {
                match self.peek_n(2)? {
                    Some(Token::Keyword(pos)) if self.ast.source[pos.range()] == *":=" => {
                        break 'outer self.parse_variable(loc);
                    }
                    _ => (),
                }
            }
            let Some(Token::Keyword(mut loc)) = self.peek()? else {
                break 'outer self.parse_expr().map(Box::new).map(AstNode::Expr);
            };

            match &self.ast.source[loc.range()] {
                "if" => {
                    self.next().expect("it is guaranteed there's another token");
                    let expr = self.parse_expr()?;
                    loc += expr.loc();
                    self.skip_expr_end()?;
                    let mut body = Vec::new();
                    let mut else_body = Vec::new();
                    let kw = loop {
                        self.skip_expr_start()?;
                        if let Some((kw_loc, name)) = combine_token_result(self.if_kw("else"), || self.if_kw("end"))? {
                            loc += kw_loc;
                            self.skip_expr_end()?;
                            break name;
                        }
                        let node = self.parse()?;
                        loc += node.loc();
                        body.push(node);
                    };
                    if kw == "else" {
                        loop {
                            self.skip_expr_start()?;
                            if let Some((kw_loc, _)) = self.if_kw("end")? {
                                loc += kw_loc;
                                break;
                            }
                            let node = self.parse()?;
                            loc += node.loc();
                            else_body.push(node);
                        }
                    }
                    Ok(AstNode::If(
                        loc,
                        Box::new(expr),
                        body.into_boxed_slice(),
                        else_body.into_boxed_slice(),
                    ))
                }
                "for" => {
                    self.next().expect("it is guaranteed there's another token");
                    let ident_loc = match self.peek()? {
                        Some(Token::Identifier(ident_loc)) => ident_loc,
                        Some(t) => break 'outer self.err(EXPECTED_STR, t.loc()),
                        None => break 'outer self.err_end(EXPECTED_STR),
                    };
                    loc += ident_loc;
                    loc += self.expect_kw("in")?;
                    let expr = self.parse_expr()?;
                    loc += expr.loc();
                    self.skip_expr_end()?;

                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start()?;
                        if let Some((kw_loc, _)) = self.if_kw("end")? {
                            loc += kw_loc;
                            break;
                        }
                        let node = self.parse()?;
                        loc += node.loc();
                        body.push(node);
                    }

                    Ok(AstNode::For(loc, ident_loc, Box::new(expr), body.into_boxed_slice()))
                }
                "slot" => {
                    self.next().expect("it is guaranteed there's another token");
                    Ok(AstNode::Slot(loc))
                }
                "render" => {
                    self.next().expect("it is guaranteed there's another token");
                    let template_expr = self.parse_expr()?;
                    loc += template_expr.loc();
                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start()?;
                        if let Some((kw_loc, _)) = self.if_kw("end")? {
                            loc += kw_loc;
                            break;
                        }
                        let node = self.parse()?;
                        loc += node.loc();
                        body.push(node);
                    }
                    Ok(AstNode::Render(loc, Box::new(template_expr), body.into_boxed_slice()))
                }
                "extend" => {
                    self.next().expect("it is guaranteed there's another token");
                    let str_loc = match self.peek()? {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer self.err(EXPECTED_STR, t.loc()),
                        None => break 'outer self.err_end(EXPECTED_STR),
                    };
                    Ok(AstNode::Extend(loc, str_loc))
                }
                "block" => {
                    self.next().expect("it is guaranteed there's another token");
                    let str_loc = match self.peek()? {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer self.err(EXPECTED_STR, t.loc()),
                        None => break 'outer self.err_end(EXPECTED_STR),
                    };
                    Ok(AstNode::Block(loc, str_loc))
                }
                "enter" => {
                    self.next().expect("it is guaranteed there's another token");
                    let str_loc = match self.peek()? {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer self.err(EXPECTED_STR, t.loc()),
                        None => break 'outer self.err_end(EXPECTED_STR),
                    };
                    self.skip_expr_end()?;
                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start()?;
                        if let Some((kw_loc, _)) = self.if_kw("end")? {
                            loc += kw_loc;
                            break;
                        }
                        let node = self.parse()?;
                        loc += node.loc();
                        body.push(node);
                    }
                    Ok(AstNode::Enter(loc, str_loc, body.into_boxed_slice()))
                }
                _ => self.parse_expr().map(Box::new).map(AstNode::Expr),
            }
        };

        self.skip_expr_end()?;
        node
    }

    // <ident> := <expr>
    fn parse_variable(&mut self, ident_loc: ByteOffset) -> Result<AstNode, ParsingError> {
        let mut loc = ident_loc;
        loc += self.expect_kw(":=")?;
        let expr = self.parse_expr()?;
        loc += expr.loc();
        Ok(AstNode::VarDef(loc, ident_loc, Box::new(expr)))
    }

    /*
     * Precedences:
     * 1 - not
     * 2 - and, or
     * 3 - ==, !=
     * 4 - +
     * 5 - ??
     * 6 - (), .
     * 7 - (<expr>), <literal>
     */

    fn parse_expr(&mut self) -> Result<Expr, ParsingError> {
        self.parse_not()
    }

    fn parse_not(&mut self) -> Result<Expr, ParsingError> {
        let Some((mut range, _)) = self.if_kw("not")? else { return self.parse_and_or() };
        let mut negates = true;
        while let Some((kw_range, _)) = self.if_kw("not")? {
            negates = !negates;
            range += kw_range;
        }
        let expr = self.parse_and_or()?;
        if negates { Ok(Expr::Not(range, Box::new(expr))) } else { Ok(expr) }
    }

    fn parse_and_or(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_equality()?;

        while let Some((loc, kw)) = combine_token_result(self.if_kw("and"), || self.if_kw("or"))? {
            let rhs = self.parse_equality()?;
            let loc = loc + lhs.loc() + rhs.loc();
            lhs = match kw {
                "and" => Expr::And(loc, Box::new(lhs), Box::new(rhs)),
                "or" => Expr::Or(loc, Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            }
        }

        Ok(lhs)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_concatenation()?;

        while let Some((loc, kw)) = combine_token_result(self.if_kw("=="), || self.if_kw("!="))? {
            let rhs = self.parse_concatenation()?;
            let loc = loc + lhs.loc() + rhs.loc();
            lhs = match kw {
                "==" => Expr::Equal(loc, Box::new(lhs), Box::new(rhs)),
                "!=" => Expr::NotEqual(loc, Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            }
        }

        Ok(lhs)
    }

    fn parse_concatenation(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_either()?;

        while let Some((loc, sym)) = combine_token_result(self.if_symbol('+'), || self.if_symbol('-'))? {
            let rhs = self.parse_either()?;
            let loc = loc + lhs.loc() + rhs.loc();
            match sym {
                '+' => lhs = Expr::Add(loc, Box::new(lhs), Box::new(rhs)),
                '-' => lhs = Expr::Sub(loc, Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            }
        }

        Ok(lhs)
    }

    fn parse_either(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_call_index()?;

        while let Some((loc, _)) = self.if_kw("??")? {
            let rhs = self.parse_call_index()?;
            let loc = loc + lhs.loc() + rhs.loc();
            lhs = Expr::Either(loc, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_call_index(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_literal()?;

        while let Some((loc, _)) = self.if_symbol('.')? {
            let ident_loc = match self.peek()? {
                Some(Token::Identifier(v)) => v,
                Some(t) => return self.err(EXPECTED_IDENT, t.loc()),
                None => return self.err_end(EXPECTED_IDENT),
            };
            self.next().expect("it is guaranteed there's another token");
            let mut new_loc = loc + ident_loc + lhs.loc();
            match self.if_symbol('(')? {
                None => lhs = Expr::Index(new_loc, Box::new(lhs), ident_loc),
                Some((loc, _)) => {
                    new_loc += loc;
                    let mut args = vec![];
                    loop {
                        if let Some((loc, _)) = self.if_symbol(')')? {
                            new_loc += loc;
                            break;
                        }
                        if !args.is_empty() {
                            new_loc += self.expect_symbol(',')?;
                            if let Some((loc, _)) = self.if_symbol(')')? {
                                new_loc += loc;
                                break;
                            }
                        }
                        args.push(self.parse_expr()?);
                    }
                    lhs = Expr::FuncCall(new_loc, Box::new(lhs), ident_loc, args.into_boxed_slice());
                }
            }
        }

        Ok(lhs)
    }

    fn parse_literal(&mut self) -> Result<Expr, ParsingError> {
        if self.if_symbol('(')?.is_some() {
            let expr = self.parse_expr()?;
            self.expect_symbol(')')?;
            return Ok(expr);
        }
        match self.peek()? {
            Some(Token::Identifier(loc)) => {
                self.next().expect("it is guaranteed there's another token");
                Ok(Expr::Var(loc))
            }
            Some(Token::StringLiteral(loc)) => {
                self.next().expect("it is guaranteed there's another token");
                Ok(Expr::String(loc, ByteOffset::new(loc.start + 1, loc.end - 1)))
            }
            Some(Token::Int(loc)) => {
                self.next().expect("it is guaranteed there's another token");

                let Ok(v) = self.ast.source[loc.range()].parse::<i64>() else { return self.err(INVALID_INT, loc) };
                Ok(Expr::Int(loc, v))
            }
            Some(Token::Float(loc)) => {
                self.next().expect("it is guaranteed there's another token");

                let Ok(v) = self.ast.source[loc.range()].parse::<f64>() else { return self.err(INVALID_FLOAT, loc) };
                Ok(Expr::Float(loc, v))
            }
            Some(t) => self.err(EXPECTED_LIT_OR_PARENS, t.loc()),
            None => self.err_end(EXPECTED_LIT_OR_PARENS),
        }
    }
}

fn combine_token_result<T>(a: TokenResult<T>, b: impl FnOnce() -> TokenResult<T>) -> TokenResult<T> {
    let Ok(None) = a else { return a };
    b()
}

// Tests: See ./mod.rs
