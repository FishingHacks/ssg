use std::ops::{Add, AddAssign, Range};
use std::sync::Arc;

use lexer::{Lexer, Token};
use miette::{Diagnostic, SourceSpan};
use parser::Parser;
use thiserror::Error;

mod lexer;
mod parser;

#[derive(Default, Debug, Clone, Copy)]
pub struct ByteOffset {
    start: usize,
    end: usize,
}

impl ByteOffset {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn range(self) -> Range<usize> {
        self.start..self.end
    }
}

impl Add for ByteOffset {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
        }
    }
}
impl AddAssign for ByteOffset {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl From<(usize, usize)> for ByteOffset {
    fn from((start, end): (usize, usize)) -> Self {
        debug_assert!(end > start);
        Self { start, end }
    }
}
impl From<Range<usize>> for ByteOffset {
    fn from(Range { start, end }: Range<usize>) -> Self {
        debug_assert!(end > start);
        Self { start, end }
    }
}
impl From<usize> for ByteOffset {
    fn from(value: usize) -> Self {
        Self {
            start: value,
            end: value + 1,
        }
    }
}
impl From<ByteOffset> for SourceSpan {
    fn from(val: ByteOffset) -> Self {
        val.range().into()
    }
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("While parsing template")]
#[diagnostic()]
pub struct ParsingError {
    #[source_code]
    pub(super) src: Arc<String>,
    #[label = "in this section"]
    pub(super) at: SourceSpan,
    #[help]
    pub(super) help: String,
}

#[derive(Debug)]
pub struct Ast {
    source: Arc<String>,
    nodes: Vec<AstNode>,
}

#[derive(Debug)]
pub enum AstNode {
    Html(ByteOffset),
    Identifier(ByteOffset),
    IntLiteral(ByteOffset),
    FloatLiteral(ByteOffset),
    StringLiteral(ByteOffset),
    Variable {
        ident: ByteOffset,
        value: Box<AstNode>,
    },
    BinaryOp {
        left: Box<AstNode>,
        right: Box<AstNode>,
        offset: ByteOffset,
        operator: Operator,
    },
    ForLoop {
        identifier: ByteOffset,
        index: Option<ByteOffset>,
        list: Box<AstNode>,
        offset: ByteOffset,
        body: Vec<AstNode>,
    },
    Render {
        component: Box<AstNode>,
        offset: ByteOffset,
        body: Vec<AstNode>,
    },
    Extend {
        template: ByteOffset,
        offset: ByteOffset,
    },
    Block {
        name: ByteOffset,
        offset: ByteOffset,
        body: Vec<AstNode>,
    },
    MemberAccess {
        object: Box<AstNode>,
        property: ByteOffset,
        offset: ByteOffset,
    },
    FunctionCall {
        function: Box<AstNode>,
        args: Vec<AstNode>,
        offset: ByteOffset,
    },
    IfStatement {
        condition: Box<AstNode>,
        truthy: Vec<AstNode>,
        falsy: Vec<AstNode>,
        offset: ByteOffset,
    },
    Not {
        expr: Box<AstNode>,
        offset: ByteOffset,
    },
    Minus {
        expr: Box<AstNode>,
        offset: ByteOffset,
    },
    Slot {
        name: Option<ByteOffset>,
        offset: ByteOffset,
    },
}

#[derive(Debug)]
pub enum Operator {
    Equal,
    NotEqual,
    GreaterEqual,
    LesserEqual,
    Greater,
    Lesser,

    Either,
    Not,
    And,
    Or,

    LeftParen,
    RightParen,
    Dot,

    Plus,
    Minus,
    Mul,
    Div,
    Modulo,
}

impl Operator {
    pub fn new_unchecked(token: Token) -> Self {
        match token {
            Token::Equal(_) => Operator::Equal,
            Token::NotEqual(_) => Operator::NotEqual,
            Token::GreaterEqual(_) => Operator::GreaterEqual,
            Token::LesserEqual(_) => Operator::LesserEqual,
            Token::Greater(_) => Operator::Greater,
            Token::Lesser(_) => Operator::Lesser,
            Token::LeftParen(_) => Operator::LeftParen,
            Token::RightParen(_) => Operator::RightParen,
            Token::Minus(_) => Operator::Minus,
            Token::Plus(_) => Operator::Plus,
            Token::Mul(_) => Operator::Mul,
            Token::Div(_) => Operator::Div,
            Token::Modulo(_) => Operator::Modulo,
            Token::Dot(_) => Operator::Dot,

            Token::And(_) => Operator::And,
            Token::Or(_) => Operator::Or,
            Token::Either(_) => Operator::Either,
            Token::Not(_) => Operator::Not,

            t => unreachable!("{t:?}"),
        }
    }
}

impl AstNode {
    fn loc(&self) -> ByteOffset {
        match self {
            AstNode::Html(offset)
            | AstNode::Identifier(offset)
            | AstNode::StringLiteral(offset)
            | AstNode::IntLiteral(offset)
            | AstNode::FloatLiteral(offset) => *offset,
            AstNode::Block { offset, .. } => *offset,
            AstNode::Not { offset, .. } => *offset,
            AstNode::Extend { offset, .. } => *offset,
            AstNode::Slot { offset, .. } => *offset,
            AstNode::Minus { offset, .. } => *offset,
            AstNode::Render { offset, .. } => *offset,
            AstNode::ForLoop { offset, .. } => *offset,
            AstNode::BinaryOp { offset, .. } => *offset,
            AstNode::IfStatement { offset, .. } => *offset,
            AstNode::MemberAccess { offset, .. } => *offset,
            AstNode::FunctionCall { offset, .. } => *offset,
            AstNode::Variable { ident, value } => *ident + value.loc(),
        }
    }
}

pub fn parse_template(template: String) -> Result<Ast, ParsingError> {
    let source = Arc::new(template);
    let lexer = Lexer::new(source.clone());
    let mut parser = Parser::new(source.clone(), lexer);
    let ast = parser.parse_all()?;

    Ok(ast)
}
