use std::ops::{Add, AddAssign, Range};

use lexer::{Lexer, Token};

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

    pub fn range(self) -> Range<usize> {
        self.start..self.end
    }
}

impl Add for ByteOffset {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start.min(rhs.start),
            end: self.end.min(rhs.end),
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

#[derive(Debug)]
pub struct Ast {
    source: String,
    nodes: Vec<AstNode>,
}

#[derive(Debug)]
pub enum AstNode {
    // Does not include either quotes. Extend/shorten by 1 on either side to get the quotes too
    Html(ByteOffset),
    /// ` ┌───────────────┐ <- .0 `
    /// ` block "meow meow"       `
    /// `        └───────┘  <- .1 `
    Block(ByteOffset, ByteOffset),
    /// ` ┌───────────────┐ <- .0 `
    /// ` enter "meow meow"       `
    /// `        └───────┘  <- .1 `
    Enter(ByteOffset, ByteOffset, Box<[AstNode]>),
    /// ` ┌────────────────┐ <- .0 `
    /// ` extend "base.html"       `
    /// `         └───────┘  <- .1 `
    Extend(ByteOffset, ByteOffset),
    Slot(ByteOffset),
    If(ByteOffset, Box<Expr>, Box<[AstNode]>, Box<[AstNode]>),
    // 1.: Location of the statement, 2.: Location of the variable identfier
    For(ByteOffset, ByteOffset, Box<Expr>, Box<[AstNode]>),
    Render(ByteOffset, Box<Expr>, Box<[AstNode]>),
    // a := b + c
    /// ` ┌──────────────┐ <- .0 `
    /// ` my_var := <expr>       `
    /// ` └────┘           <- .1 `
    VarDef(ByteOffset, ByteOffset, Box<Expr>),
    Expr(Box<Expr>),
}

impl AstNode {
    fn loc(&self) -> ByteOffset {
        match self {
            AstNode::Html(byte_offset)
            | AstNode::Block(byte_offset, ..)
            | AstNode::Enter(byte_offset, ..)
            | AstNode::Extend(byte_offset, ..)
            | AstNode::Slot(byte_offset)
            | AstNode::If(byte_offset, ..)
            | AstNode::For(byte_offset, ..)
            | AstNode::Render(byte_offset, ..)
            | AstNode::VarDef(byte_offset, ..) => *byte_offset,
            AstNode::Expr(expr) => expr.loc(),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    /// ` ┌───────────────┐ <- .0 `
    /// ` "my fancy string"       `
    /// `  └─────────────┘  <- .1 `
    String(ByteOffset, ByteOffset),
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
    Concat(ByteOffset, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn loc(&self) -> ByteOffset {
        match self {
            Expr::String(byte_offset, ..)
            | Expr::Var(byte_offset)
            | Expr::Index(byte_offset, ..)
            | Expr::FuncCall(byte_offset, ..)
            | Expr::Either(byte_offset, ..)
            | Expr::Not(byte_offset, ..)
            | Expr::And(byte_offset, ..)
            | Expr::Or(byte_offset, ..)
            | Expr::Equal(byte_offset, ..)
            | Expr::NotEqual(byte_offset, ..)
            | Expr::Concat(byte_offset, ..) => *byte_offset,
        }
    }
}

pub fn parse_template(template: String) -> miette::Result<Ast> {
    let mut ast = Ast {
        source: template,
        nodes: vec![],
    };

    let tokens = Lexer::new(&ast.source).lex()?;
    let mut parser = parser::Parser::new(&mut ast, &tokens);
    parser.parse_all().map_err(|v| miette::miette!("{v}"))?;

    Ok(ast)
}

pub fn print_token(src: &str, tok: &Token) {
    match tok {
        Token::Html(byte_offset) => println!("Html({:?})", &src[byte_offset.range()]),
        Token::ExprStart(_) => println!("ExprStart"),
        Token::ExprEnd(_) => println!("ExprEnd"),
        Token::Identifier(byte_offset) => println!("Ident({})", &src[byte_offset.range()]),
        Token::StringLiteral(byte_offset) => {
            println!("StringLit({:?})", &src[byte_offset.start + 1..byte_offset.end - 1])
        }
        Token::Keyword(byte_offset) => println!("Keyword({})", &src[byte_offset.range()]),
        Token::Symbol(c, _) => println!("Symbol({c:?})"),
    }
}

pub fn print_node(src: &str, node: &AstNode, indent: usize) {
    for _ in 0..indent {
        print!("    ");
    }
    match node {
        AstNode::Html(byte_offset) => println!("Html({:?})", &src[byte_offset.range()]),
        AstNode::Block(_, name) => println!("Block({:?})", &src[name.range()]),
        AstNode::Enter(_, name, children) => {
            println!("Enter({:?}, [", &src[name.range()]);
            children.iter().for_each(|v| print_node(src, v, indent + 1));
            for _ in 0..indent {
                print!("    ");
            }
            println!("])");
        }
        AstNode::Extend(_, file) => println!("Extend({:?})", &src[file.range()]),
        AstNode::Slot(_) => println!("Slot"),
        AstNode::If(_, expr, if_block, else_block) => {
            print!("If(");
            print_expr(src, expr);
            println!(", [");
            if_block.iter().for_each(|v| print_node(src, v, indent + 1));
            for _ in 0..indent {
                print!("    ");
            }
            println!("], [");
            else_block.iter().for_each(|v| print_node(src, v, indent + 1));
            for _ in 0..indent {
                print!("    ");
            }
            println!("])");
        }
        AstNode::For(_, var, expr, body) => {
            print!("For({}, ", &src[var.range()]);
            print_expr(src, expr);
            println!(", [");
            body.iter().for_each(|v| print_node(src, v, indent + 1));
            for _ in 0..indent {
                print!("    ");
            }
            println!("])");
        }
        AstNode::Render(_, expr, children) => {
            print!("Render(");
            print_expr(src, expr);
            println!(", [");
            children.iter().for_each(|v| print_node(src, v, indent + 1));
            for _ in 0..indent {
                print!("    ");
            }
            println!("])");
        }
        AstNode::VarDef(_, name, expr) => {
            print!("Define({}, ", &src[name.range()]);
            print_expr(src, expr);
            println!(")");
        }
        AstNode::Expr(expr) => {
            print!("Expr(");
            print_expr(src, expr);
            println!(")\n")
        }
    }
}

pub fn print_expr(src: &str, expr: &Expr) {
    match expr {
        Expr::String(_, range) => print!("{:?}", &src[range.range()]),
        Expr::Var(byte_offset) => print!("{}", &src[byte_offset.range()]),
        Expr::Index(_, lhs, rhs) => {
            print!("(index ");
            print_expr(src, lhs);
            print!(" {})", &src[rhs.range()]);
        }
        Expr::FuncCall(_, expr, name, exprs) => {
            print!("(call ");
            print_expr(src, expr);
            print!(", {}", &src[name.range()]);
            for expr in exprs {
                print!(", ");
                print_expr(src, expr);
            }
            print!(")");
        }
        Expr::Not(_, expr) => {
            print!("(not ");
            print_expr(src, expr);
            print!(")");
        }
        Expr::Either(_, lhs, rhs) => {
            print!("(either ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
        Expr::And(_, lhs, rhs) => {
            print!("(and ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
        Expr::Or(_, lhs, rhs) => {
            print!("(or ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
        Expr::Equal(_, lhs, rhs) => {
            print!("(equal ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
        Expr::NotEqual(_, lhs, rhs) => {
            print!("(notequal ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
        Expr::Concat(_, lhs, rhs) => {
            print!("(concat ");
            print_expr(src, lhs);
            print!(", ");
            print_expr(src, rhs);
            print!(")");
        }
    }
}
