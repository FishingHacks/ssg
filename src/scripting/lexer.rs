use thiserror::Error;

use super::ByteOffset;

#[derive(Debug, Error)]
pub enum LexingError {
    #[error("{0:?}: String literal is unterminated")]
    UnterminatedStringLiteral(ByteOffset),
    #[error("{0:?}: Expected {1} but found {2}")]
    UnexpectedCharacter(ByteOffset, char, char),
    #[error("Expected {0} but found EOF")]
    UnexpectedEOF(char),
}

pub struct Lexer<'lex> {
    cursor: usize,
    source: &'lex str,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Html(ByteOffset),
    ExprStart(ByteOffset),
    ExprEnd(ByteOffset),
    Identifier(ByteOffset),
    /// "afjkdgkjfg"
    /// ~~~~~~~~~~~~ <- .0
    StringLiteral(ByteOffset),
    Keyword(ByteOffset),
    Symbol(char, ByteOffset),
}

impl Token {
    pub fn loc(&self) -> ByteOffset {
        match self {
            Token::Html(byte_offset)
            | Token::ExprStart(byte_offset)
            | Token::ExprEnd(byte_offset)
            | Token::Identifier(byte_offset)
            | Token::StringLiteral(byte_offset)
            | Token::Keyword(byte_offset)
            | Token::Symbol(_, byte_offset) => *byte_offset,
        }
    }
}

impl<'lex> Lexer<'lex> {
    pub fn new(source: &'lex str) -> Self {
        Self {
            source,
            cursor: 0,
            tokens: vec![],
        }
    }

    fn peek(&self) -> Option<char> {
        self.source[self.cursor..].chars().next()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.cursor += ch.len_utf8();
        Some(ch)
    }

    fn starts_with(&self, s: &str) -> bool {
        self.source[self.cursor..].starts_with(s)
    }

    fn bump(&mut self, n: usize) {
        self.cursor += n;
    }

    // Why not make this miette::Result<Token>?
    pub fn lex(mut self) -> miette::Result<Vec<Token>> {
        while self.cursor < self.source.len() {
            if self.starts_with("{{") {
                self.bump(2);
                self.lex_script()?;
            } else {
                self.lex_html()?;
            }
        }

        Ok(self.tokens)
    }

    fn lex_html(&mut self) -> miette::Result<()> {
        let start = self.cursor;

        while self.cursor < self.source.len() {
            if self.starts_with("{{") {
                break;
            }
            self.next();
        }

        if self.cursor > start {
            self.tokens.push(Token::Html(ByteOffset::new(start, self.cursor)));
        }

        Ok(())
    }

    fn lex_script(&mut self) -> miette::Result<()> {
        let start = self.cursor - 2; // already consumed "{{"
        self.tokens.push(Token::ExprStart(ByteOffset::new(start, self.cursor)));

        self.consume_whitespace();

        loop {
            if self.starts_with("}}") {
                let end_start = self.cursor;
                self.cursor += 2;
                self.tokens
                    .push(Token::ExprEnd(ByteOffset::new(end_start, self.cursor)));
                break;
            }

            match self.peek() {
                Some('"') => {
                    let offset = self.lex_string_literal()?;
                    self.tokens.push(Token::StringLiteral(offset));
                }
                Some(c) if c.is_alphabetic() || c == '_' => {
                    let span = self.lex_identifier();
                    let ident = &self.source[span.start..span.end];

                    if self.is_keyword(ident) {
                        self.tokens.push(Token::Keyword(span));
                    } else {
                        self.tokens.push(Token::Identifier(span));
                    }
                }
                Some(c @ ('.' | '+' | '(' | ')' | ',')) => {
                    let start = self.cursor;
                    self.cursor += 1;
                    self.tokens.push(Token::Symbol(c, ByteOffset::new(start, self.cursor)));
                }
                // NOTE: I am parsing these as keywords.. idk if this is sane or expected.. or if
                // we should just merge keywords and symbols
                Some(':') => {
                    let start = self.cursor;
                    self.bump(1);
                    self.expect('=')?;
                    self.tokens.push(Token::Keyword(ByteOffset::new(start, self.cursor)));
                }
                Some('=') => {
                    let start = self.cursor;
                    self.bump(1);
                    self.expect('=')?;
                    self.tokens.push(Token::Keyword(ByteOffset::new(start, self.cursor)));
                }
                Some('!') => {
                    let start = self.cursor;
                    self.bump(1);
                    self.expect('=')?;
                    self.tokens.push(Token::Keyword(ByteOffset::new(start, self.cursor)));
                }
                Some('?') => {
                    let start = self.cursor;
                    self.bump(1);
                    self.expect('?')?;
                    self.tokens.push(Token::Keyword(ByteOffset::new(start, self.cursor)));
                }
                Some(' ') => self.bump(1),
                Some(c) => return Err(miette::miette!("Unexpected character `{c}`")),
                None => panic!("Unclosed script block (missing '}}')"),
            }
        }

        Ok(())
    }

    fn lex_identifier(&mut self) -> ByteOffset {
        let start = self.cursor;

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.next();
            } else {
                break;
            }
        }

        ByteOffset::new(start, self.cursor)
    }

    fn lex_string_literal(&mut self) -> miette::Result<ByteOffset> {
        let start = self.cursor;
        self.expect('"')?;

        while let Some(c) = self.next() {
            if c == '"' {
                return Ok(ByteOffset::new(start, self.cursor));
            }
        }

        Err(miette::miette!("Unterminated string literal"))
    }

    fn expect(&mut self, expected: char) -> miette::Result<()> {
        match self.next() {
            Some(c) if c == expected => Ok(()),
            Some(c) => Err(miette::miette!("Expected '{}', found '{}'", expected, c)),
            None => Err(miette::miette!("Expected '{}', found EOF", expected)),
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    fn is_keyword(&self, ident: &str) -> bool {
        matches!(
            ident,
            "for" | "in" | "end" | "if" | "else" | "block" | "enter" | "render" | "slot" | "extend"
        )
    }
}
