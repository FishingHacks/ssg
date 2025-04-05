use thiserror::Error;

use super::lexer::Token;
use super::{Ast, ByteOffset, Expr};
use crate::scripting::AstNode;

#[derive(Debug, Error)]
pub enum ParsingError {
    #[error("{0:?}: Expected `{1}`")]
    ExpectedKeyword(ByteOffset, &'static str),
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
}

#[derive(Debug)]
pub struct Parser<'ast, 'tokens> {
    ast: &'ast mut Ast,
    tokens: &'tokens [Token],
    pos: usize,
}

impl<'ast, 'tokens> Parser<'ast, 'tokens> {
    pub fn new(ast: &'ast mut Ast, tokens: &'tokens [Token]) -> Self {
        Self { ast, tokens, pos: 0 }
    }

    pub fn parse_all(&mut self) -> Result<(), ParsingError> {
        while !self.is_at_end() {
            println!("{}", self.pos);
            let node = self.parse()?;
            self.ast.nodes.push(node);
        }
        Ok(())
    }

    fn expect_kw(&mut self, kw: &'static str) -> Result<ByteOffset, ParsingError> {
        let Some(v) = self.peek() else { return Err(ParsingError::ExpectedKeywordEOF(kw)) };
        self.next();
        match v {
            Token::Keyword(offset) if &self.ast.source[offset.range()] == kw => Ok(offset),
            t => Err(ParsingError::ExpectedKeyword(t.loc(), kw)),
        }
    }

    fn expect_symbol(&mut self, sym: char) -> Result<ByteOffset, ParsingError> {
        let Some(v) = self.peek() else { return Err(ParsingError::ExpectedSymbolEOF(sym)) };
        self.next();
        match v {
            Token::Symbol(symbol, offset) if symbol == sym => Ok(offset),
            t => Err(ParsingError::ExpectedSymbol(t.loc(), sym)),
        }
    }

    fn if_kw(&mut self, kw: &'static str) -> Option<(ByteOffset, &'static str)> {
        match self.peek() {
            Some(Token::Keyword(offset)) if &self.ast.source[offset.range()] == kw => {
                self.next();
                Some((offset, kw))
            }
            _ => None,
        }
    }

    fn if_symbol(&mut self, symbol: char) -> Option<(ByteOffset, char)> {
        match self.peek() {
            Some(Token::Symbol(sym, offset)) if sym == symbol => {
                self.next();
                Some((offset, symbol))
            }
            _ => None,
        }
    }

    fn is_at_end(&self) -> bool {
        self.tokens.len() == self.pos
    }

    fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    /// assumes there's a next tokens
    ///
    /// # Panics
    ///
    /// panics when there's no next token
    fn next(&mut self) -> Token {
        let tok = self.tokens[self.pos];
        self.pos += 1;
        tok
    }

    fn skip_expr_end(&mut self) {
        if let Some(Token::ExprEnd(..)) = self.peek() {
            self.next();
        }
    }

    fn skip_expr_start(&mut self) {
        if let Some(Token::ExprStart(..)) = self.peek() {
            self.next();
        }
    }

    fn parse(&mut self) -> Result<AstNode, ParsingError> {
        if let Some(Token::Html(loc)) = self.peek() {
            self.next();
            return Ok(AstNode::Html(loc));
        }
        self.skip_expr_start();
        let node = 'outer: {
            if let Some(Token::Identifier(loc)) = self.peek() {
                match self.tokens.get(self.pos + 1) {
                    Some(Token::Keyword(pos)) if self.ast.source[pos.range()] == *":=" => {
                        break 'outer self.parse_variable(loc);
                    }
                    _ => (),
                }
            }
            let Some(Token::Keyword(mut loc)) = self.peek() else {
                break 'outer self.parse_expr().map(Box::new).map(AstNode::Expr);
            };
            match &self.ast.source[loc.range()] {
                "if" => {
                    self.next();
                    let expr = self.parse_expr()?;
                    loc += expr.loc();
                    self.skip_expr_end();
                    let mut body = Vec::new();
                    let mut else_body = Vec::new();
                    let kw = loop {
                        self.skip_expr_start();
                        if let Some((kw_loc, name)) = self.if_kw("else").or_else(|| self.if_kw("end")) {
                            loc += kw_loc;
                            self.skip_expr_end();
                            break name;
                        }
                        let node = self.parse()?;
                        loc += node.loc();
                        body.push(node);
                    };
                    if kw == "else" {
                        loop {
                            self.skip_expr_start();
                            if let Some((kw_loc, _)) = self.if_kw("end") {
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
                    self.next();
                    let ident_loc = match self.peek() {
                        Some(Token::Identifier(ident_loc)) => ident_loc,
                        Some(t) => break 'outer Err(ParsingError::ExpectedString(t.loc())),
                        None => break 'outer Err(ParsingError::ExpectedStringEof),
                    };
                    loc += ident_loc;
                    loc += self.expect_kw("in")?;
                    let expr = self.parse_expr()?;
                    loc += expr.loc();
                    self.skip_expr_end();

                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start();
                        if let Some((kw_loc, _)) = self.if_kw("end") {
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
                    self.next();
                    Ok(AstNode::Slot(loc))
                }
                "render" => {
                    self.next();
                    let template_expr = self.parse_expr()?;
                    loc += template_expr.loc();
                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start();
                        if let Some((kw_loc, _)) = self.if_kw("end") {
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
                    self.next();
                    let str_loc = match self.peek() {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer Err(ParsingError::ExpectedString(t.loc())),
                        None => break 'outer Err(ParsingError::ExpectedStringEof),
                    };
                    Ok(AstNode::Extend(loc, str_loc))
                }
                "block" => {
                    self.next();
                    let str_loc = match self.peek() {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer Err(ParsingError::ExpectedString(t.loc())),
                        None => break 'outer Err(ParsingError::ExpectedStringEof),
                    };
                    Ok(AstNode::Block(loc, str_loc))
                }
                "enter" => {
                    self.next();
                    let str_loc = match self.peek() {
                        Some(Token::StringLiteral(str_loc)) => {
                            loc += str_loc;
                            ByteOffset::new(str_loc.start + 1, str_loc.end - 1)
                        }
                        Some(t) => break 'outer Err(ParsingError::ExpectedString(t.loc())),
                        None => break 'outer Err(ParsingError::ExpectedStringEof),
                    };
                    self.skip_expr_end();
                    let mut body = Vec::new();
                    loop {
                        self.skip_expr_start();
                        if let Some((kw_loc, _)) = self.if_kw("end") {
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

        self.skip_expr_end();
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
        let Some((mut range, _)) = self.if_kw("not") else { return self.parse_and_or() };
        let mut negates = true;
        while let Some((kw_range, _)) = self.if_kw("not") {
            negates = !negates;
            range += kw_range;
        }
        let expr = self.parse_and_or()?;
        if negates { Ok(Expr::Not(range, Box::new(expr))) } else { Ok(expr) }
    }

    fn parse_and_or(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_equality()?;

        while let Some((loc, kw)) = self.if_kw("and").or_else(|| self.if_kw("or")) {
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

        while let Some((loc, kw)) = self.if_kw("==").or_else(|| self.if_kw("!=")) {
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

        while let Some((loc, _)) = self.if_symbol('+') {
            let rhs = self.parse_either()?;
            let loc = loc + lhs.loc() + rhs.loc();
            lhs = Expr::Concat(loc, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_either(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_call_index()?;

        while let Some((loc, _)) = self.if_kw("??") {
            let rhs = self.parse_call_index()?;
            let loc = loc + lhs.loc() + rhs.loc();
            lhs = Expr::Either(loc, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_call_index(&mut self) -> Result<Expr, ParsingError> {
        let mut lhs = self.parse_literal()?;

        while let Some((loc, _)) = self.if_symbol('.') {
            let ident_loc = match self.peek() {
                Some(Token::Identifier(v)) => v,
                Some(t) => return Err(ParsingError::ExpectedIdent(t.loc())),
                None => return Err(ParsingError::ExpectedIdentEOF),
            };
            self.next();
            let mut new_loc = loc + ident_loc + lhs.loc();
            match self.if_symbol('(') {
                None => lhs = Expr::Index(new_loc, Box::new(lhs), ident_loc),
                Some((loc, _)) => {
                    new_loc += loc;
                    let mut args = vec![];
                    loop {
                        if let Some((loc, _)) = self.if_symbol(')') {
                            new_loc += loc;
                            break;
                        }
                        if !args.is_empty() {
                            new_loc += self.expect_symbol(',')?;
                            if let Some((loc, _)) = self.if_symbol(')') {
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
        if self.if_symbol('(').is_some() {
            let expr = self.parse_expr()?;
            self.expect_symbol(')')?;
            return Ok(expr);
        }
        match self.peek() {
            Some(Token::Identifier(loc)) => {
                self.next();
                Ok(Expr::Var(loc))
            }
            Some(Token::StringLiteral(loc)) => {
                self.next();
                Ok(Expr::String(loc, ByteOffset::new(loc.start + 1, loc.end - 1)))
            }
            Some(t) => {
                println!("{t:?}");
                Err(ParsingError::UnexpectedToken(t.loc()))
            }
            None => Err(ParsingError::UnexpectedTokenEof),
        }
    }
}
