use std::sync::Arc;

use super::lexer::{Lexer, Token};
use super::{Ast, ByteOffset, Operator, ParsingError};
use crate::scripting::AstNode;

mod precedences {
    pub const BASE: u8 = 0;
    pub const COALISCING: u8 = 1;
    pub const OR: u8 = 2;
    pub const AND: u8 = 3;
    pub const RELATIONAL: u8 = 4;
    pub const ADDITIVE: u8 = 5;
    pub const MULTIPLICATIVE: u8 = 6;
    pub const UNARY: u8 = 7;
    pub const APPLICATION: u8 = 8;
}

fn get_precedence(operator: Operator) -> u8 {
    match operator {
        // null coalescing is lowest precedence
        Operator::Either => precedences::COALISCING,

        Operator::Or => precedences::OR,
        Operator::And => precedences::AND,

        // relational operators
        Operator::Equal
        | Operator::NotEqual
        | Operator::GreaterEqual
        | Operator::LesserEqual
        | Operator::Greater
        | Operator::Lesser => precedences::RELATIONAL,

        // additive operators
        Operator::Plus | Operator::Minus => precedences::ADDITIVE,

        // multiplicative operators
        Operator::Mul | Operator::Div | Operator::Modulo => precedences::MULTIPLICATIVE,

        // unary operators
        Operator::Not => precedences::UNARY,

        // application operators
        Operator::LeftParen | Operator::RightParen | Operator::Dot => precedences::APPLICATION,
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
                help: format!(
                    "Unexpected token: expected {:?} but found EOF",
                    stringify!($expected)
                ),
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
struct BlockDelimiter {
    delimiter: Token,
    end: ByteOffset,
    body: Vec<AstNode>,
}

impl Default for BlockDelimiter {
    fn default() -> Self {
        Self {
            delimiter: Token::Html(Default::default()),
            end: ByteOffset::default(),
            body: vec![],
        }
    }
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
            nodes.push(self.parse()?);
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
        let start = expect!(self.lexer, Token::ScriptStart(_))?;

        let token = peek_bail!(self.lexer);

        if matches!(token, Token::For(_)) {
            consume!(self.lexer);
            return self.parse_for_loop(start);
        }

        if matches!(token, Token::Slot(_)) {
            consume!(self.lexer);
            return self.parse_slot(start);
        }

        if matches!(token, Token::If(_)) {
            consume!(self.lexer);
            return self.parse_if_statement(start);
        }

        if matches!(token, Token::Block(_)) {
            consume!(self.lexer);
            return self.parse_block(start);
        }

        if matches!(token, Token::Render(_)) {
            consume!(self.lexer);
            return self.parse_render(start);
        }

        if matches!(token, Token::Extend(_)) {
            let keyword = expect!(self.lexer, Token::Extend(_))?;
            let template = expect!(self.lexer, Token::StringLiteral(_))?.loc();
            let offset = keyword.loc() + template;
            expect!(self.lexer, Token::ScriptEnd(_))?;
            return Ok(AstNode::Extend { template, offset });
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
            Some(token) => {
                return Err(ParsingError {
                    src: self.source.clone(),
                    at: token.loc().range().into(),
                    help: "Unexpected token".into(),
                });
            }
            None => unreachable!(),
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

            let precedence = get_precedence(Operator::new_unchecked(next));

            if precedence <= min_precedence {
                break;
            }

            match next {
                Token::Dot(_) => {
                    expect!(self.lexer, Token::Dot(_))?;
                    let property = expect!(self.lexer, Token::Identifier(_))?;
                    let offset = left.loc() + property.loc();

                    left = AstNode::MemberAccess {
                        object: Box::new(left),
                        property: property.loc(),
                        offset,
                    };
                }
                Token::LeftParen(_) => {
                    expect!(self.lexer, Token::LeftParen(_))?;
                    let mut args = vec![];

                    while !matches!(peek_bail!(self.lexer), Token::RightParen(_)) {
                        args.push(self.parse_expression(precedences::BASE)?);

                        if !matches!(peek_bail!(self.lexer), Token::Comma(_)) {
                            break;
                        }

                        consume!(self.lexer);
                    }

                    let closing = expect!(self.lexer, Token::RightParen(_))?;
                    let offset = left.loc() + closing.loc();

                    left = AstNode::FunctionCall {
                        function: Box::new(left),
                        args,
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
            Operator::Not => {
                let left = self.parse_expression(precedences::UNARY)?;
                Ok(AstNode::Not {
                    offset: token.loc() + left.loc(),
                    expr: Box::new(left),
                })
            }
            Operator::Minus => {
                let left = self.parse_expression(precedences::UNARY)?;
                Ok(AstNode::Minus {
                    offset: token.loc() + left.loc(),
                    expr: Box::new(left),
                })
            }
            t => unreachable!("{t:?}"),
        }
    }

    fn parse_for_loop(&mut self, start: Token) -> Result<AstNode, ParsingError> {
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
        expect!(self.lexer, Token::ScriptEnd(_))?;

        let block = self.parse_until_block_delimiter(|t| matches!(t, Token::End(_)))?;

        let offset = start.loc() + block.end;
        Ok(AstNode::ForLoop {
            identifier,
            index,
            list: Box::new(list),
            offset,
            body: block.body,
        })
    }

    fn parse_slot(&mut self, start: Token) -> Result<AstNode, ParsingError> {
        let name = match peek_bail!(self.lexer) {
            Token::StringLiteral(offset) => Some(offset),
            _ => None,
        };

        if name.is_some() {
            consume!(self.lexer);
        }

        let end = expect!(self.lexer, Token::ScriptEnd(_))?;

        let offset = start.loc() + end.loc();
        Ok(AstNode::Slot { name, offset })
    }

    fn parse_if_statement(&mut self, start: Token) -> Result<AstNode, ParsingError> {
        let condition = self.parse_expression(precedences::BASE)?;
        expect!(self.lexer, Token::ScriptEnd(_))?;

        let truthy = self
            .parse_until_block_delimiter(|token| matches!(token, Token::End(_) | Token::Else(_)))?;

        let mut falsy = vec![];
        let mut end = truthy.end;

        if matches!(truthy.delimiter, Token::Else(_)) {
            let block = self.parse_until_block_delimiter(|t| matches!(t, Token::End(_)))?;
            end = block.end;
            falsy = block.body;
        }

        let offset = start.loc() + end;
        Ok(AstNode::IfStatement {
            condition: Box::new(condition),
            truthy: truthy.body,
            falsy,
            offset,
        })
    }

    fn parse_block(&mut self, start: Token) -> Result<AstNode, ParsingError> {
        let name = expect!(self.lexer, Token::StringLiteral(_))?.loc();
        let mut block = BlockDelimiter::default();

        // when the component name is not immediately followed by a `end`, render
        // block has a body
        let has_block = matches!(peek_bail!(self.lexer), Token::ScriptEnd(_));
        if has_block {
            consume!(self.lexer);
            block = self.parse_until_block_delimiter(|t| matches!(t, Token::End(_)))?;
        } else {
            expect!(self.lexer, Token::End(_))?;
            expect!(self.lexer, Token::ScriptEnd(_))?;
        }

        let offset = start.loc() + block.end;

        Ok(AstNode::Block {
            name,
            offset,
            body: block.body,
        })
    }

    fn parse_render(&mut self, start: Token) -> Result<AstNode, ParsingError> {
        let component = expect!(self.lexer, Token::StringLiteral(_))?.loc();
        let mut block = BlockDelimiter::default();

        // when the component name is not immediately followed by a `end`, render
        // block has a body
        let has_block = matches!(peek_bail!(self.lexer), Token::ScriptEnd(_));
        if has_block {
            consume!(self.lexer);
            block = self.parse_until_block_delimiter(|t| matches!(t, Token::End(_)))?;
        } else {
            expect!(self.lexer, Token::End(_))?;
        }

        let offset = if has_block {
            start.loc() + block.end
        } else {
            let end = expect!(self.lexer, Token::ScriptEnd(_))?;
            start.loc() + end.loc()
        };
        Ok(AstNode::Render {
            component,
            offset,
            body: block.body,
        })
    }

    fn parse_until_block_delimiter(
        &mut self,
        is_delimiter: impl Fn(Token) -> bool,
    ) -> Result<BlockDelimiter, ParsingError> {
        let mut block = BlockDelimiter::default();

        loop {
            let start = peek_bail!(self.lexer);
            let keyword = peek_bail!(self.lexer, 1);

            if matches!(start, Token::ScriptStart(_)) && is_delimiter(keyword) {
                consume!(self.lexer, 2);
                let end_script = expect!(self.lexer, Token::ScriptEnd(_))?;

                block.delimiter = keyword;
                block.end = end_script.loc();

                break;
            }

            let node = self.parse()?;

            block.body.push(node);
        }

        Ok(block)
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
            body: Vec<AstNodeSnapshot<'ast>>,
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
            body: Vec<AstNodeSnapshot<'ast>>,
        },
        MemberAccess {
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
            property: ByteOffsetSnapshot<'ast>,
            object: Box<AstNodeSnapshot<'ast>>,
        },
        FunctionCall {
            function: Box<AstNodeSnapshot<'ast>>,
            args: Vec<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        IfStatement {
            condition: Box<AstNodeSnapshot<'ast>>,
            truthy: Vec<AstNodeSnapshot<'ast>>,
            falsy: Vec<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Not {
            expr: Box<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Slot {
            name: Option<ByteOffsetSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
        },
        Minus {
            expr: Box<AstNodeSnapshot<'ast>>,
            offset: ByteOffsetSnapshot<'ast>,
            content: &'ast str,
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
                body: body
                    .into_iter()
                    .map(|node| node_to_snapshot(node, source))
                    .collect(),
            },
            AstNode::Render {
                component,
                body,
                offset,
            } => AstNodeSnapshot::Render {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
                component: ByteOffsetSnapshot::with_content(component, source),
                body: body
                    .into_iter()
                    .map(|node| node_to_snapshot(node, source))
                    .collect(),
            },
            AstNode::Extend { template, offset } => AstNodeSnapshot::Extend {
                content: &source[offset.range()],
                offset: ByteOffsetSnapshot::with_content(offset, source),
                template: ByteOffsetSnapshot::with_content(template, source),
            },
            AstNode::Block { name, offset, body } => AstNodeSnapshot::Block {
                content: &source[offset.range()],
                name: ByteOffsetSnapshot::with_content(name, source),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                body: body
                    .into_iter()
                    .map(|node| node_to_snapshot(node, source))
                    .collect(),
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
            AstNode::FunctionCall {
                args,
                function,
                offset,
            } => AstNodeSnapshot::FunctionCall {
                function: Box::new(node_to_snapshot(*function, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                args: args
                    .into_iter()
                    .map(|arg| node_to_snapshot(arg, source))
                    .collect(),
                content: &source[offset.range()],
            },
            AstNode::IfStatement {
                condition,
                truthy,
                falsy,
                offset,
            } => AstNodeSnapshot::IfStatement {
                condition: Box::new(node_to_snapshot(*condition, source)),
                truthy: truthy
                    .into_iter()
                    .map(|node| node_to_snapshot(node, source))
                    .collect(),
                falsy: falsy
                    .into_iter()
                    .map(|node| node_to_snapshot(node, source))
                    .collect(),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                content: &source[offset.range()],
            },
            AstNode::Not { offset, expr } => AstNodeSnapshot::Not {
                expr: Box::new(node_to_snapshot(*expr, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                content: &source[offset.range()],
            },
            AstNode::Slot { offset, name } => AstNodeSnapshot::Slot {
                name: name.map(|n| ByteOffsetSnapshot::with_content(n, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                content: &source[offset.range()],
            },
            AstNode::Minus { offset, expr } => AstNodeSnapshot::Minus {
                expr: Box::new(node_to_snapshot(*expr, source)),
                offset: ByteOffsetSnapshot::with_content(offset, source),
                content: &source[offset.range()],
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
            "{{ render \"button.html\" end }}",
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
            "{{ render \"button.html\" end }}",
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

    #[test]
    fn test_parse_function_calls() {
        let source = String::from("{{ site.assets.get('assets/logo.png') }}");
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
    fn test_parse_if_null_check() {
        let source = [
            "{{ if page.title }}",
            "<title>{{ page.title }}</title>",
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
    fn test_parse_if_else() {
        let source = [
            "{{ if page.author }}",
            "<title>{{ page.title }} - {{ page.author }}<title>",
            "{{ else }}",
            "<title>{{ page.title }}</title>",
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
    fn test_parse_if_nested() {
        let source = [
            "{{ if page.author }}",
            "  {{ if page.author.name }}",
            "  <title>{{ page.title }} - {{ page.author.name }}<title>",
            "  {{ else }}",
            "  <title>{{ page.title }} - {{ page.author }}<title>",
            "  {{ end }}",
            "{{ else }}",
            "<title>{{ page.title }}</title>",
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
    fn test_parse_binary_expressions() {
        let source = [
            "{{ something ?? other_thing }}",
            "{{ something and other_thing }}",
            "{{ something or other_thing }}",
            "{{ something == other_thing }}",
            "{{ something != other_thing }}",
            "{{ something < other_thing }}",
            "{{ something <= other_thing }}",
            "{{ something > other_thing }}",
            "{{ something >= other_thing }}",
            "{{ something - other_thing }}",
            "{{ something + other_thing }}",
            "{{ something * other_thing }}",
            "{{ something / other_thing }}",
            "{{ something % other_thing }}",
        ]
        .join("");
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
    fn test_parse_unary_expressions() {
        let source = [
            "{{ not something and other_thing }}",
            "{{ not (something and other_thing) }}",
        ]
        .join("");
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
    fn test_parse_blocks() {
        let source = [
            "{{ block \"name\" }}",
            "<span>html here</span>",
            "{{ end }}",
            // empty block
            "{{ block \"name\" end }}",
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
    fn test_parse_render_blocks() {
        let source = [
            "{{ render \"other\" end }}",
            "{{ render \"name\" }}",
            "<span>html here</span>",
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
    fn test_parse_slot() {
        let source = ["{{ slot \"name\" }}", "{{ slot }}"].join("\n");
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
    fn test_unary_minus() {
        let source = [
            "{{ -b }}",
            "{{ -1 }}",
            "{{ not -1 and true }}",
            "{{ not -b and true }}",
            "{{ -(-1 - -2) * -(3 * -2) }}",
            "{{ something := -(-1 - -2) * -(3 * -2) }}",
        ]
        .join("");
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
