use super::ByteOffset;

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
    StringLiteral(ByteOffset),
    Keyword(ByteOffset),
    Symbol(char, ByteOffset),
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
                Some('.') => {
                    let start = self.cursor;
                    self.cursor += 1;
                    self.tokens
                        .push(Token::Symbol('.', ByteOffset::new(start, self.cursor)));
                }
                Some(' ') => self.bump(1),
                Some(c) => {
                    // here we hit an unknown symbol. Right now im just lexing it as a "symbol"
                    // but we should probably error. idk
                    let start = self.cursor;
                    self.cursor += c.len_utf8();
                    self.tokens.push(Token::Symbol(c, ByteOffset::new(start, self.cursor)));
                }
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
        self.expect('"')?;
        let start = self.cursor;

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
            "for" | "end" | "block" | "if" | "page" | "resources" | "site" | "render" | "extend"
        )
    }
}
