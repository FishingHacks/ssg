use std::sync::Arc;

use super::lexer::{Lexer, Token};
use super::{Ast, Operator, ParsingError};
use crate::scripting::AstNode;

mod precedences {
    pub const BASE: u8 = 0;
    pub const SUM: u8 = 3;
    pub const MUL: u8 = 4;
    pub const ASSOC: u8 = 5;
    pub const APPLY: u8 = 6;
    pub const MEMBER: u8 = 7;
}

fn get_precedence(token: Token) -> u8 {
    match token {
        Token::Plus(_) | Token::Minus(_) => precedences::SUM,
        Token::Mul(_) => precedences::MUL,
        Token::LeftParen(_) => precedences::APPLY,
        Token::Dot(_) => precedences::MEMBER,
        _ => precedences::BASE,
    }
}

#[macro_export]
macro_rules! peek {
    ($lexer:expr) => {
        $lexer.peek().transpose()?
    };

    ($lexer:expr, $amount:expr) => {
        $lexer.peek_n($amount).transpose()?
    };
}

#[macro_export]
macro_rules! peek_bail {
    ($lexer:expr) => {{
        let Some(token) = peek!($lexer) else {
            return Err(ParsingError {
                src: $lexer.source.clone(),
                at: (0..1).into(),
                help: "Syntax error: unterminated expression".into(),
            });
        };

        token
    }};

    ($lexer:expr, $amount:expr) => {{
        let Some(token) = peek!($lexer, $amount) else {
            return Err(ParsingError {
                src: $lexer.source.clone(),
                at: (0..1).into(),
                help: "Syntax error: unterminated expression".into(),
            });
        };

        token
    }};
}

#[macro_export]
macro_rules! consume {
    ($lexer:expr) => {
        $lexer.next().transpose()?
    };

    ($lexer:expr, $amount:expr) => {{
        for _ in 0..$amount {
            $lexer.next().transpose()?;
        }
    }};
}

#[macro_export]
macro_rules! expect {
    ($lexer:expr, $expected:pat) => {{
        let Some(token) = $lexer.next().transpose()? else {
            return Err(ParsingError {
                at: (0..1).into(),
                help: format!("Unexpected token: expected {:?} but found EOF", stringify!($expected)),
                src: $lexer.source.clone(),
            });
        };

        if matches!(token, $expected) {
            Ok(token)
        } else {
            Err(ParsingError {
                at: token.loc().range().into(),
                help: format!(
                    "Unexpecte token: expected {:?} but found {token:?}",
                    stringify!($expected)
                ),
                src: $lexer.source.clone(),
            })
        }
    }};
}

#[derive(Debug)]
pub struct Parser {
    source: Arc<String>,
    lexer: Lexer,
}

impl Parser {
    pub fn new(source: Arc<String>, lexer: Lexer) -> Self {
        Self { source, lexer }
    }

    pub fn parse_all(&mut self) -> Result<Ast, ParsingError> {
        let mut nodes = vec![];

        while !self.lexer.is_empty() {
            let node = self.parse()?;
            nodes.push(node);
        }

        Ok(Ast {
            source: self.source.clone(),
            nodes,
        })
    }

    fn parse(&mut self) -> Result<AstNode, ParsingError> {
        // token will exist as we only loop while lexer is not empty
        let token = peek!(self.lexer).unwrap();

        match token {
            Token::Html(offset) => {
                consume!(self.lexer);
                Ok(AstNode::Html(offset))
            }
            Token::ScriptStart(_) => self.parse_script_block(),
            t => unreachable!("{t:?}"),
        }
    }

    fn parse_script_block(&mut self) -> Result<AstNode, ParsingError> {
        expect!(self.lexer, Token::ScriptStart(_))?;

        let token = peek_bail!(self.lexer);

        if matches!(token, Token::For(_)) {
            let keyword = expect!(self.lexer, Token::For(_))?;
            return self.parse_for_loop(keyword);
        }

        if matches!(token, Token::Render(_)) {
            let keyword = expect!(self.lexer, Token::Render(_))?;
            let component = expect!(self.lexer, Token::StringLiteral(_))?.loc();
            let offset = keyword.loc() + component;
            expect!(self.lexer, Token::ScriptEnd(_))?;
            return Ok(AstNode::Render { component, offset });
        }

        if matches!(token, Token::Extend(_)) {
            let keyword = expect!(self.lexer, Token::Extend(_))?;
            let template = expect!(self.lexer, Token::StringLiteral(_))?.loc();
            let offset = keyword.loc() + template;
            expect!(self.lexer, Token::ScriptEnd(_))?;
            return Ok(AstNode::Extend { template, offset });
        }

        if matches!(token, Token::Block(_)) {
            let keyword = expect!(self.lexer, Token::Block(_))?;
            let name = expect!(self.lexer, Token::StringLiteral(_))?.loc();
            let offset = keyword.loc() + name;
            expect!(self.lexer, Token::ScriptEnd(_))?;
            return Ok(AstNode::Block { name, offset });
        }

        let expr = self.parse_expression(precedences::BASE)?;
        expect!(self.lexer, Token::ScriptEnd(_))?;

        Ok(expr)
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<AstNode, ParsingError> {
        let left = self.lexer.next().transpose()?;
        let mut left = match left {
            Some(Token::Identifier(offset)) => AstNode::Identifier(offset),
            Some(Token::Int(offset)) => AstNode::IntLiteral(offset),
            Some(Token::Float(offset)) => AstNode::FloatLiteral(offset),
            Some(Token::StringLiteral(offset)) => AstNode::StringLiteral(offset),
            Some(token) if token.is_operator() => self.parse_operator(token)?,
            None => unreachable!(),
            t => unreachable!("{t:?}"),
        };

        if let Token::Assign(_) = peek_bail!(self.lexer) {
            expect!(self.lexer, Token::Assign(_))?;

            let value = self.parse_expression(precedences::BASE)?;

            return Ok(AstNode::Variable {
                ident: left.loc(),
                value: Box::new(value),
            });
        };

        loop {
            let Some(next) = peek!(self.lexer) else {
                return Ok(left);
            };

            if !next.is_operator() {
                return Ok(left);
            }

            let precedence = get_precedence(next);

            if precedence <= min_precedence {
                break;
            }

            match next {
                Token::Dot(_) => {
                    expect!(self.lexer, Token::Dot(_))?;
                    let property = expect!(self.lexer, Token::Identifier(_))?;
                    println!("{property:?}");
                    let offset = left.loc() + property.loc();

                    left = AstNode::MemberAccess {
                        object: Box::new(left),
                        property: property.loc(),
                        offset,
                    };
                }
                _ => {
                    let operator = consume!(self.lexer).expect("operator must exist here");
                    let operator = Operator::new_unchecked(operator);
                    let right = self.parse_expression(precedence)?;

                    left = AstNode::BinaryOp {
                        offset: left.loc() + right.loc(),
                        left: Box::new(left),
                        right: Box::new(right),
                        operator,
                    }
                }
            }
        }

        Ok(left)
    }

    fn parse_operator(&mut self, token: Token) -> Result<AstNode, ParsingError> {
        let operator = Operator::new_unchecked(token);

        match operator {
            Operator::LeftParen => {
                let left = self.parse_expression(precedences::BASE)?;
                expect!(self.lexer, Token::RightParen(_))?;
                Ok(left)
            }
            _ => unreachable!(),
        }
    }

    fn parse_for_loop(&mut self, keyword: Token) -> Result<AstNode, ParsingError> {
        let identifier = expect!(self.lexer, Token::Identifier(_))?.loc();

        let index = match peek!(self.lexer) {
            Some(Token::Comma(_)) => {
                consume!(self.lexer);
                Some(expect!(self.lexer, Token::Identifier(_))?.loc())
            }
            _ => None,
        };

        expect!(self.lexer, Token::In(_))?;
        let list = self.parse_expression(precedences::BASE)?;
        let offset = keyword.loc() + list.loc();

        expect!(self.lexer, Token::ScriptEnd(_))?;

        let body = self.parse_until_end_block()?;

        Ok(AstNode::ForLoop {
            identifier,
            index,
            list: Box::new(list),
            offset,
            body,
        })
    }

    fn parse_until_end_block(&mut self) -> Result<Vec<AstNode>, ParsingError> {
        let mut body = vec![];

        loop {
            let first = peek_bail!(self.lexer);
            let second = peek_bail!(self.lexer, 1);

            if matches!(first, Token::ScriptStart(_)) && matches!(second, Token::End(_)) {
                consume!(self.lexer, 2);
                expect!(self.lexer, Token::ScriptEnd(_))?;
                break;
            }

            let node = self.parse()?;

            body.push(node);
        }

        Ok(body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scripting::ByteOffset;

    #[allow(dead_code)]
    #[derive(Debug)]
    struct ByteOffsetSnapshot<'ast> {
        end: usize,
        start: usize,
        content: &'ast str,
    }

    impl<'ast> ByteOffsetSnapshot<'ast> {
        fn with_content(offset: ByteOffset, source: &'ast str) -> Self {
            Self {
                end: offset.end,
                start: offset.start,
                content: &source[offset.range()],
            }
        }
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    enum AstNodeSnapshot<'ast> {
        Html {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Identifier {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        IntLiteral {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        FloatLiteral {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        StringLiteral {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Variable {
            ident: ByteOffsetSnapshot<'ast>,
            value: Box<AstNodeSnapshot<'ast>>,
            content: &'ast str,
        },
        BinaryOp {
            left: Box<AstNodeSnapshot<'ast>>,
            right: Box<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
            operator: Operator,
        },
        ForLoop {
            identifier: ByteOffsetSnapshot<'ast>,
            index: Option<ByteOffsetSnapshot<'ast>>,
            list: Box<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
            body: Vec<AstNodeSnapshot<'ast>>,
        },
        Render {
            component: ByteOffsetSnapshot<'ast>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Extend {
            template: ByteOffsetSnapshot<'ast>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Block {
            name: ByteOffsetSnapshot<'ast>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        MemberAccess {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
            property: ByteOffsetSnapshot<'ast>,
            object: Box<AstNodeSnapshot<'ast>>,
        },
    }

    fn node_to_snapshot(node: AstNode, source: &str) -> AstNodeSnapshot<'_> {
        match node {
            AstNode::Html(offset) => AstNodeSnapshot::Html {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::Identifier(offset) => AstNodeSnapshot::Identifier {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::IntLiteral(offset) => AstNodeSnapshot::IntLiteral {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::FloatLiteral(offset) => AstNodeSnapshot::FloatLiteral {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::StringLiteral(offset) => AstNodeSnapshot::StringLiteral {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::Variable { ident, value } => AstNodeSnapshot::Variable {
                content: &source[(ident + value.loc()).range()],
                value: Box::new(node_to_snapshot(*value, source)),
                ident: ByteOffsetSnapshot::with_content(ident, source),
            },
            AstNode::BinaryOp {
                left,
                right,
                operator,
                offset,
            } => AstNodeSnapshot::BinaryOp {
                operator,
                content: &source[offset.range()],
                left: Box::new(node_to_snapshot(*left, source)),
                right: Box::new(node_to_snapshot(*right, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::ForLoop {
                identifier,
                index,
                list,
                offset,
                body,
            } => AstNodeSnapshot::ForLoop {
                content: &source[offset.range()],
                list: Box::new(node_to_snapshot(*list, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                identifier: ByteOffsetSnapshot::with_content(identifier, source),
                index: index.map(|index| ByteOffsetSnapshot::with_content(index, source)),
                body: body.into_iter().map(|node| node_to_snapshot(node, source)).collect(),
            },
            AstNode::Render { component, offset } => AstNodeSnapshot::Render {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
                component: ByteOffsetSnapshot::with_content(component, source),
            },
            AstNode::Extend { template, offset } => AstNodeSnapshot::Extend {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
                template: ByteOffsetSnapshot::with_content(template, source),
            },
            AstNode::Block { name, offset } => AstNodeSnapshot::Block {
                content: &source[offset.range()],
                name: ByteOffsetSnapshot::with_content(name, source),
                offset: ByteOffsetSnapshot::with_content(offset, source),
            },
            AstNode::MemberAccess {
                object,
                offset,
                property,
            } => AstNodeSnapshot::MemberAccess {
                content: &source[offset.range()],
                object: Box::new(node_to_snapshot(*object, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                property: ByteOffsetSnapshot::with_content(property, source),
            },
        }
    }

    #[test]
    fn test_parse_identifier() {
        let source = Arc::new(String::from("{{ var_name }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_var_identifier() {
        let source = Arc::new(String::from("{{ var_name := something_else }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_var_number() {
        let source = Arc::new(String::from("{{ var_name := 1337 }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_var_float() {
        let source = Arc::new(String::from("{{ var_name := 13.37 }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_var_string() {
        let source = Arc::new(String::from("{{ var_name := \"hello\" }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_var_expr() {
        let source = Arc::new(String::from("{{ var_name := (100 + 300) * 200 }}"));

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        let ast = ast
            .nodes
            .into_iter()
            .map(|node| node_to_snapshot(node, &source))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_for_loop() {
        let source = [
            "{{ for tag, idx in page.tags }}",
            "<div>",
            "  <span>Hello World!</span>",
            "</div>",
            "{{ render \"button.html\" }}",
            "<div>",
            "  <span>Hello Another</span>",
            "</div>",
            "{{ end }}",
        ]
        .join("\n");

        let source = Arc::new(source);
        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        let ast = ast
            .nodes
            .into_iter()
            .map(|node| node_to_snapshot(node, &source))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_for_without_idx() {
        let source = [
            "{{ for tag in page.tags }}",
            "<div>",
            "  <span>Hello World!</span>",
            "</div>",
            "{{ render \"button.html\" }}",
            "<div>",
            "  <span>Hello Another</span>",
            "</div>",
            "{{ end }}",
        ]
        .join("\n");

        let source = Arc::new(source);
        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        let ast = ast
            .nodes
            .into_iter()
            .map(|node| node_to_snapshot(node, &source))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_parse_member_access() {
        let source = String::from("{{ page.author.name }}");
        let source = Arc::new(source);

        let lexer = Lexer::new(source.clone());
        let mut parser = Parser::new(source.clone(), lexer);
        let ast = parser.parse_all().unwrap();

        let ast = ast
            .nodes
            .into_iter()
            .map(|node| node_to_snapshot(node, &source))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(ast);
    }
}
