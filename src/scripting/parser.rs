use super::Ast;
use super::lexer::Token;

#[derive(Debug, Default)]
pub struct Parser {
    pos: usize,
}

impl Parser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse(&mut self, ast: &mut Ast, tokens: Vec<Token>) {
        // TODO: this here
    }
}
