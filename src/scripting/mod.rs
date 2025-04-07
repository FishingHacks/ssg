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
    /// Expression holds a single identifier. This will yield the identifier's
    /// value into the template
    ///
    /// # Example:
    ///
    /// {{ some_value }}
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
        component: ByteOffset,
        offset: ByteOffset,
    },
    Extend {
        template: ByteOffset,
        offset: ByteOffset,
    },
    Block {
        name: ByteOffset,
        offset: ByteOffset,
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

            Token::Bang(_) => Operator::Not,
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

#[derive(Debug)]
enum Expression {}

// #[derive(Debug)]
// // TODO: Remove
// // temporary, the fields will be used when doing evaluation
// #[allow(dead_code)]
// pub enum AstNode {
//     Identifier {
//         name: ByteOffset,
//     },
//     // Does not include either quotes. Extend/shorten by 1 on either side to get the quotes too
//     Html(ByteOffset),
//     /// ` ┌───────────────┐ <- .0 `
//     /// ` block "meow meow"       `
//     /// `        └───────┘  <- .1 `
//     Block(ByteOffset, ByteOffset),
//     /// ` ┌───────────────┐ <- .0 `
//     /// ` enter "meow meow"       `
//     /// `        └───────┘  <- .1 `
//     Enter(ByteOffset, ByteOffset, Box<[AstNode]>),
//     /// ` ┌────────────────┐ <- .0 `
//     /// ` extend "base.html"       `
//     /// `         └───────┘  <- .1 `
//     Extend(ByteOffset, ByteOffset),
//     Slot(ByteOffset),
//     If(ByteOffset, Box<Expr>, Box<[AstNode]>, Box<[AstNode]>),
//     // 1.: Location of the statement, 2.: Location of the variable identfier
//     For(ByteOffset, ByteOffset, Box<Expr>, Box<[AstNode]>),
//     Render(ByteOffset, Box<Expr>, Box<[AstNode]>),
//     // a := b + c
//     /// ` ┌──────────────┐ <- .0 `
//     /// ` my_var := <expr>       `
//     /// ` └────┘           <- .1 `
//     VarDef {
//         name: ByteOffset,
//         value: Box<Expr>,
//     },
//     Expr(Box<Expr>),
// }

// impl AstNode {
//     fn loc(&self) -> ByteOffset {
//         match self {
//             AstNode::Html(byte_offset)
//             | AstNode::Block(byte_offset, ..)
//             | AstNode::Enter(byte_offset, ..)
//             | AstNode::Extend(byte_offset, ..)
//             | AstNode::Slot(byte_offset)
//             | AstNode::If(byte_offset, ..)
//             | AstNode::For(byte_offset, ..)
//             | AstNode::Render(byte_offset, ..)
//             | AstNode::VarDef(byte_offset, ..) => *byte_offset,
//             AstNode::Expr(expr) => expr.loc(),
//         }
//     }
// }

#[derive(Debug)]
// TODO: Remove
// temporary, the fields will be used when doing evaluation
#[allow(dead_code)]
pub enum Expr {
    /// ` ┌───────────────┐ <- .0 `
    /// ` "my fancy string"       `
    /// `  └─────────────┘  <- .1 `
    String(ByteOffset, ByteOffset),
    Float(ByteOffset, f64),
    Int(ByteOffset, i64),
    Var(ByteOffset),
    /// <expr _1>._2
    /// ` ┌──────────────┐ <- .0 `
    /// ` <expr>.something       `
    /// `        └───────┘ <- .2 `
    Index(ByteOffset, Box<Expr>, ByteOffset),
    // <expr _1>._2(<exprs _3>)
    /// ` ┌───────────────────┐ <- .0 `
    /// ` <expr>.something(...)       `
    /// `        └───────┘      <- .2 `
    FuncCall(ByteOffset, Box<Expr>, ByteOffset, Box<[Expr]>),
    // <expr _1> ?? <expr _2>
    Either(ByteOffset, Box<Expr>, Box<Expr>),
    // not <expr _1>
    Not(ByteOffset, Box<Expr>),
    // <expr _1> and <expr _2>
    And(ByteOffset, Box<Expr>, Box<Expr>),
    // <expr _1> or <expr _2>
    Or(ByteOffset, Box<Expr>, Box<Expr>),
    // <expr _1> == <expr _2>
    Equal(ByteOffset, Box<Expr>, Box<Expr>),
    // <expr _1> != <expr _2>
    NotEqual(ByteOffset, Box<Expr>, Box<Expr>),
    // <expr _1> + <expr _2>
    Add(ByteOffset, Box<Expr>, Box<Expr>),
    // <expr _1> - <expr _2>
    Sub(ByteOffset, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn loc(&self) -> ByteOffset {
        match self {
            Expr::String(byte_offset, ..)
            | Expr::Float(byte_offset, ..)
            | Expr::Int(byte_offset, ..)
            | Expr::Var(byte_offset)
            | Expr::Index(byte_offset, ..)
            | Expr::FuncCall(byte_offset, ..)
            | Expr::Either(byte_offset, ..)
            | Expr::Not(byte_offset, ..)
            | Expr::And(byte_offset, ..)
            | Expr::Or(byte_offset, ..)
            | Expr::Equal(byte_offset, ..)
            | Expr::NotEqual(byte_offset, ..)
            | Expr::Add(byte_offset, ..)
            | Expr::Sub(byte_offset, ..) => *byte_offset,
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

// #[allow(dead_code)]
// pub fn print_token(src: &str, tok: &Token) {
//     match tok {
//         Token::Html(byte_offset) => println!("Html({:?})", &src[byte_offset.range()]),
//         Token::ExprStart(_) => println!("ExprStart"),
//         Token::ExprEnd(_) => println!("ExprEnd"),
//         Token::Identifier(byte_offset) => println!("Ident({})", &src[byte_offset.range()]),
//         Token::StringLiteral(byte_offset) => {
//             println!("StringLit({:?})", &src[byte_offset.start + 1..byte_offset.end - 1])
//         }
//         Token::Float(byte_offset) => println!("Float({})", &src[byte_offset.range()]),
//         Token::Int(byte_offset) => println!("Int({})", &src[byte_offset.range()]),
//         Token::Assign(_) => todo!(),
//         Token::Equal(_) => todo!(),
//         Token::NotEqual(_) => todo!(),
//         Token::GreaterEqual(_) => todo!(),
//         Token::LessEqual(_) => todo!(),
//         Token::Greater(_) => todo!(),
//         Token::Less(_) => todo!(),
//         Token::Dot(_) => todo!(),
//         Token::LeftParen(_) => todo!(),
//         Token::RightParen(_) => todo!(),
//         Token::Minus(_) => todo!(),
//         Token::Plus(_) => todo!(),
//         Token::For(_) => todo!(),
//         Token::In(_) => todo!(),
//         Token::End(_) => todo!(),
//         Token::If(_) => todo!(),
//         Token::Else(_) => todo!(),
//         Token::Block(_) => todo!(),
//         Token::Render(_) => todo!(),
//         Token::Slot(_) => todo!(),
//         Token::Extend(_) => todo!(),
//     }
// }
//
// #[allow(dead_code)]
// pub fn print_node(src: &str, node: &AstNode, indent: usize) {
//     for _ in 0..indent {
//         print!("    ");
//     }
//     match node {
//         AstNode::Html(byte_offset) => println!("Html({:?})", &src[byte_offset.range()]),
//         AstNode::Block(_, name) => println!("Block({:?})", &src[name.range()]),
//         AstNode::Enter(_, name, children) => {
//             println!("Enter({:?}, [", &src[name.range()]);
//             children.iter().for_each(|v| print_node(src, v, indent + 1));
//             for _ in 0..indent {
//                 print!("    ");
//             }
//             println!("])");
//         }
//         AstNode::Extend(_, file) => println!("Extend({:?})", &src[file.range()]),
//         AstNode::Slot(_) => println!("Slot"),
//         AstNode::If(_, expr, if_block, else_block) => {
//             print!("If(");
//             print_expr(src, expr);
//             println!(", [");
//             if_block.iter().for_each(|v| print_node(src, v, indent + 1));
//             for _ in 0..indent {
//                 print!("    ");
//             }
//             println!("], [");
//             else_block.iter().for_each(|v| print_node(src, v, indent + 1));
//             for _ in 0..indent {
//                 print!("    ");
//             }
//             println!("])");
//         }
//         AstNode::For(_, var, expr, body) => {
//             print!("For({}, ", &src[var.range()]);
//             print_expr(src, expr);
//             println!(", [");
//             body.iter().for_each(|v| print_node(src, v, indent + 1));
//             for _ in 0..indent {
//                 print!("    ");
//             }
//             println!("])");
//         }
//         AstNode::Render(_, expr, children) => {
//             print!("Render(");
//             print_expr(src, expr);
//             println!(", [");
//             children.iter().for_each(|v| print_node(src, v, indent + 1));
//             for _ in 0..indent {
//                 print!("    ");
//             }
//             println!("])");
//         }
//         AstNode::VarDef(_, name, expr) => {
//             print!("Define({}, ", &src[name.range()]);
//             print_expr(src, expr);
//             println!(")");
//         }
//         AstNode::Expr(expr) => {
//             print!("Expr(");
//             print_expr(src, expr);
//             println!(")\n")
//         }
//     }
// }
//
// #[allow(dead_code)]
// pub fn print_expr(src: &str, expr: &Expr) {
//     match expr {
//         Expr::String(_, range) => print!("{:?}", &src[range.range()]),
//         Expr::Float(_, v) => print!("{v}"),
//         Expr::Int(_, v) => print!("{v}"),
//         Expr::Var(byte_offset) => print!("{}", &src[byte_offset.range()]),
//         Expr::Index(_, lhs, rhs) => {
//             print!("(index ");
//             print_expr(src, lhs);
//             print!(" {})", &src[rhs.range()]);
//         }
//         Expr::FuncCall(_, expr, name, exprs) => {
//             print!("(call ");
//             print_expr(src, expr);
//             print!(", {}", &src[name.range()]);
//             for expr in exprs {
//                 print!(", ");
//                 print_expr(src, expr);
//             }
//             print!(")");
//         }
//         Expr::Not(_, expr) => {
//             print!("(not ");
//             print_expr(src, expr);
//             print!(")");
//         }
//         Expr::Either(_, lhs, rhs) => {
//             print!("(either ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::And(_, lhs, rhs) => {
//             print!("(and ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::Or(_, lhs, rhs) => {
//             print!("(or ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::Equal(_, lhs, rhs) => {
//             print!("(equal ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::NotEqual(_, lhs, rhs) => {
//             print!("(notequal ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::Add(_, lhs, rhs) => {
//             print!("(add ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//         Expr::Sub(_, lhs, rhs) => {
//             print!("(sub ");
//             print_expr(src, lhs);
//             print!(", ");
//             print_expr(src, rhs);
//             print!(")");
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn simple_page() {
//         let src = r#"
// <!DOCTYPE html>
// <html lang="en">
//     <head id="head">
//         <meta charset="UTF-8">
//
//         <meta name="description" content="{{ page.description }}">
//
//         <meta http-equiv="x-ua-compatible" content="ie=edge">
//         <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
//
//         {{ if page.author }}
//         <title>{{ page.title }} - {{ page.author }}</title>
//         {{ else }}
//         <title>{{ page.title }}</title>
//         {{ end }}
//
//         {{ slot "head" }}
//     </head>
//
//     <body>
//         <main>
//             {{ slot "page" }}
//         </main>
//     </body>
// </html>
// "#;
//         let ast = parse_template(src.into()).unwrap();
//         insta::assert_debug_snapshot!(ast);
//     }
// }
