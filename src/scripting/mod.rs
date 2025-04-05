use lexer::Lexer;

mod lexer;
mod parser;

#[derive(Debug, Clone, Copy)]
pub struct ByteOffset {
    start: usize,
    end: usize,
}

impl ByteOffset {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl From<(usize, usize)> for ByteOffset {
    fn from((start, end): (usize, usize)) -> Self {
        debug_assert!(end > start);
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Ast {
    source: String,
    nodes: Vec<AstNode>,
}

#[derive(Debug)]
pub enum AstNode {
    Html(ByteOffset),
}

pub fn parse_template(template: String) -> miette::Result<Ast> {
    let mut ast = Ast {
        source: template,
        nodes: vec![],
    };

    let tokens = Lexer::new(&ast.source).lex()?;
    let mut parser = parser::Parser::new();
    parser.parse(&mut ast, tokens);

    Ok(ast)
}
