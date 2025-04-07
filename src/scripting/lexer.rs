use std::collections::VecDeque;
use std::ops::ControlFlow;
use std::sync::Arc;

use super::{ByteOffset, ParsingError};

#[derive(Debug)]
enum LexingMode {
    Html,
    Script,
}

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Html(ByteOffset),
    ScriptStart(ByteOffset),
    ScriptEnd(ByteOffset),
    Identifier(ByteOffset),
    StringLiteral(ByteOffset),
    Float(ByteOffset),
    Int(ByteOffset),

    Assign(ByteOffset),
    Equal(ByteOffset),
    NotEqual(ByteOffset),
    GreaterEqual(ByteOffset),
    LesserEqual(ByteOffset),
    Greater(ByteOffset),
    Lesser(ByteOffset),
    Dot(ByteOffset),
    LeftParen(ByteOffset),
    RightParen(ByteOffset),
    Minus(ByteOffset),
    Plus(ByteOffset),
    Mul(ByteOffset),

    Comma(ByteOffset),

    For(ByteOffset),
    In(ByteOffset),
    End(ByteOffset),
    If(ByteOffset),
    Else(ByteOffset),
    Block(ByteOffset),
    Render(ByteOffset),
    Slot(ByteOffset),
    Extend(ByteOffset),
}

impl Token {
    pub fn loc(&self) -> ByteOffset {
        match self {
            Token::Html(byte_offset)
            | Token::ScriptStart(byte_offset)
            | Token::ScriptEnd(byte_offset)
            | Token::Identifier(byte_offset)
            | Token::StringLiteral(byte_offset)
            | Token::Float(byte_offset)
            | Token::Assign(byte_offset)
            | Token::Equal(byte_offset)
            | Token::NotEqual(byte_offset)
            | Token::GreaterEqual(byte_offset)
            | Token::LesserEqual(byte_offset)
            | Token::Greater(byte_offset)
            | Token::Lesser(byte_offset)
            | Token::Dot(byte_offset)
            | Token::LeftParen(byte_offset)
            | Token::RightParen(byte_offset)
            | Token::Minus(byte_offset)
            | Token::Plus(byte_offset)
            | Token::For(byte_offset)
            | Token::In(byte_offset)
            | Token::End(byte_offset)
            | Token::If(byte_offset)
            | Token::Else(byte_offset)
            | Token::Block(byte_offset)
            | Token::Comma(byte_offset)
            | Token::Render(byte_offset)
            | Token::Slot(byte_offset)
            | Token::Mul(byte_offset)
            | Token::Extend(byte_offset)
            | Token::Int(byte_offset) => *byte_offset,
        }
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::Plus(_)
                | Token::Minus(_)
                | Token::LeftParen(_)
                | Token::RightParen(_)
                | Token::Lesser(_)
                | Token::LesserEqual(_)
                | Token::Greater(_)
                | Token::GreaterEqual(_)
                | Token::NotEqual(_)
                | Token::Equal(_)
                | Token::Dot(_)
                | Token::Mul(_)
        )
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Token::For(_)
                | Token::In(_)
                | Token::If(_)
                | Token::Render(_)
                | Token::Extend(_)
                | Token::Block(_)
                | Token::Slot(_)
                | Token::Else(_)
        )
    }
}

#[derive(Debug)]
pub struct Lexer {
    cursor: usize,
    pub source: Arc<String>,
    mode: LexingMode,
    html_start: Option<usize>,
    script_start: Option<usize>,
    peeked: VecDeque<Result<Token, ParsingError>>,
}

impl Lexer {
    pub fn new(source: Arc<String>) -> Self {
        Self {
            source,
            cursor: 0,
            html_start: None,
            script_start: None,
            peeked: VecDeque::new(),
            mode: LexingMode::Html,
        }
    }

    pub fn peek_n(&mut self, n: usize) -> Option<Result<Token, ParsingError>> {
        let mut buffer = Vec::with_capacity(n);

        while self.peeked.len() <= n {
            let Some(next) = self.next() else {
                break;
            };

            buffer.push(next);
        }

        self.peeked.extend(buffer);

        self.peeked.get(n).cloned()
    }

    pub fn peek(&mut self) -> Option<Result<Token, ParsingError>> {
        self.peek_n(0)
    }

    pub fn is_empty(&mut self) -> bool {
        self.peek().is_none()
    }

    fn is_keyword(&self, ident: &str, offset: ByteOffset) -> Option<Token> {
        match ident {
            "for" => Some(Token::For(offset)),
            "in" => Some(Token::In(offset)),
            "end" => Some(Token::End(offset)),
            "if" => Some(Token::If(offset)),
            "else" => Some(Token::Else(offset)),
            "block" => Some(Token::Block(offset)),
            "render" => Some(Token::Render(offset)),
            "slot" => Some(Token::Slot(offset)),
            "extend" => Some(Token::Extend(offset)),
            _ => None,
        }
    }

    fn advance_script(&mut self, amount: usize) {
        self.cursor += amount;
        self.script_start = Some(self.cursor);
    }

    fn lex_html(&mut self, curr: char, next: Option<char>) -> ControlFlow<Result<Token, ParsingError>, ()> {
        match (curr, next) {
            ('{', Some('{')) => {
                self.mode = LexingMode::Script;
                self.script_start = Some(self.cursor);

                if let Some(start) = self.html_start.take() {
                    return ControlFlow::Break(Ok(Token::Html((start, self.cursor).into())));
                };

                ControlFlow::Continue(())
            }
            // reached the last character, so we return the whole section since
            // next char would be EOF
            (_, None) => {
                self.cursor += 1;
                let start = self.html_start.take().unwrap_or(self.cursor - 1);
                ControlFlow::Break(Ok(Token::Html((start, self.cursor).into())))
            }
            _ => {
                if self.html_start.is_none() {
                    self.html_start = Some(self.cursor);
                }

                self.cursor += 1;

                ControlFlow::Continue(())
            }
        }
    }

    fn lex_script(&mut self, curr: char, next: Option<char>) -> ControlFlow<Result<Token, ParsingError>> {
        match (curr, next) {
            (c, _) if c.is_whitespace() => {
                self.advance_script(c.len_utf8());
                ControlFlow::Continue(())
            }
            ('{', Some('{')) => {
                let start = self
                    .script_start
                    .expect("script start must be set while lexing script content");

                self.advance_script(2);

                ControlFlow::Break(Ok(Token::ScriptStart((start, self.cursor).into())))
            }
            ('}', Some('}')) => {
                self.mode = LexingMode::Html;
                self.cursor += 2;

                let start = self
                    .script_start
                    .take()
                    .expect("script start must be set while lexing script content");

                ControlFlow::Break(Ok(Token::ScriptEnd((start, self.cursor).into())))
            }
            (':', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Assign((self.cursor - 2, self.cursor).into())))
            }
            ('=', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Equal((self.cursor - 2, self.cursor).into())))
            }
            ('!', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::NotEqual((self.cursor - 2, self.cursor).into())))
            }
            ('>', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::GreaterEqual((self.cursor - 2, self.cursor).into())))
            }
            ('<', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::LesserEqual((self.cursor - 2, self.cursor).into())))
            }
            ('>', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Greater((self.cursor - 1, self.cursor).into())))
            }
            ('<', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Lesser((self.cursor - 1, self.cursor).into())))
            }
            ('.', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Dot((self.cursor - 1, self.cursor).into())))
            }
            ('(', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::LeftParen((self.cursor - 1, self.cursor).into())))
            }
            (')', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::RightParen((self.cursor - 1, self.cursor).into())))
            }
            ('-', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Minus((self.cursor - 1, self.cursor).into())))
            }
            ('*', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Mul((self.cursor - 1, self.cursor).into())))
            }
            (',', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Comma((self.cursor - 1, self.cursor).into())))
            }
            ('+', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Plus((self.cursor - 1, self.cursor).into())))
            }
            ('"' | '\'', _) => {
                self.cursor += 1;
                let start = self.cursor;
                let source = &self.source[self.cursor..];

                let Some(length) = source.find(curr) else {
                    return ControlFlow::Break(Err(ParsingError {
                        src: self.source.clone(),
                        at: (start..self.source.len()).into(),
                        help: "unterminated string".into(),
                    }));
                };

                // skip past the closing " for the next iterations but don't
                // include it on the offset range
                self.advance_script(length + 1);
                let end = start + length;

                ControlFlow::Break(Ok(Token::StringLiteral((start, end).into())))
            }
            ('0'..='9', _) => {
                let start = self.cursor;
                let source = &self.source[self.cursor..];

                let length = source
                    .find(|c| !matches!(c, '_' | '.' | '0'..='9'))
                    .unwrap_or(self.source.len());

                self.advance_script(length);
                let end = start + length;
                let literal = &self.source[start..end];

                let mut dotted = literal.splitn(3, '.');
                match (dotted.next(), dotted.next(), dotted.next()) {
                    // found at least 2 dots on a number. thats a invalid float
                    (Some(_), Some(_), Some(_)) => {
                        return ControlFlow::Break(Err(ParsingError {
                            src: self.source.clone(),
                            at: (start..end).into(),
                            help: "invalid numeral".into(),
                        }));
                    }
                    _ => {
                        // literal is either a integer or a valid floating point
                        // so skip doing anything here
                    }
                };

                let is_float = literal.contains('.');
                if is_float {
                    return ControlFlow::Break(Ok(Token::Float((start, end).into())));
                }

                ControlFlow::Break(Ok(Token::Int((start, end).into())))
            }
            (c, _) => {
                if c.is_identifier_starter() {
                    let start = self.cursor;
                    let source = &self.source[self.cursor..];

                    let length = source
                        .find(|c: char| !c.is_valid_identifier())
                        .unwrap_or(self.source.len());

                    self.advance_script(length);

                    let end = start + length;
                    let content = &self.source[start..end];
                    let offset = (start, end).into();

                    if let Some(keyword) = self.is_keyword(content, offset) {
                        return ControlFlow::Break(Ok(keyword));
                    }

                    return ControlFlow::Break(Ok(Token::Identifier(offset)));
                }

                println!("{c:?}");
                panic!();
            }
        }
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, ParsingError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(peeked) = self.peeked.pop_front() {
                return Some(peeked);
            }

            let source = &self.source[self.cursor..];
            let mut chars = source.chars().peekable();

            let ch = chars.next()?;
            let next = chars.peek().copied();

            match self.mode {
                LexingMode::Html => match self.lex_html(ch, next) {
                    ControlFlow::Continue(_) => continue,
                    ControlFlow::Break(token) => return Some(token),
                },
                LexingMode::Script => match self.lex_script(ch, next) {
                    ControlFlow::Continue(_) => continue,
                    ControlFlow::Break(token) => return Some(token),
                },
            }
        }
    }
}

trait Identifier {
    fn is_identifier_starter(&self) -> bool;
    fn is_valid_identifier(&self) -> bool;
}

impl Identifier for char {
    fn is_identifier_starter(&self) -> bool {
        self.is_alphabetic() || matches!(self, '_' | '$')
    }

    fn is_valid_identifier(&self) -> bool {
        self.is_ascii_alphanumeric() || matches!(self, '_')
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // allowing dead code as this is simply a test helper
    #[derive(Debug)]
    #[allow(dead_code)]
    struct TokenSnapshot<'snap> {
        token: Token,
        content: &'snap str,
    }

    impl<'snap> TokenSnapshot<'snap> {
        fn with_content(token: Token, content: &'snap str) -> Self {
            Self { content, token }
        }
    }

    #[test]
    fn test_lexing_simple_document() {
        let source = [
            "<!DOCTYPE html>",
            "<html lang=\"en\">",
            "  <head id=\"head\">",
            "    <meta charset=\"UTF-8\">",
            "    <meta name=\"description\" content=\"{{ page.description }}\">",
            "",
            "    {{ if page.author }}",
            "    <title>{{ page.title }} - {{ page.author }}</title>",
            "    {{ else }}",
            "    <title>{{ page.title }}</title>",
            "    {{ end }}",
            "",
            "    {{ slot \"head\" }}",
            "  </head>",
            "  <body>",
            "    <main>",
            "      {{ slot \"page\" }}",
            "    </main>",
            "  </body>",
            "</html>",
        ]
        .join("\n");

        let source = Arc::new(source);
        let lexer = Lexer::new(source.clone());

        let tokens = lexer
            .map(|t| t.unwrap())
            .map(|token| TokenSnapshot::with_content(token, &source[token.loc().range()]))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_lexing_language_features() {
        // this is purposefully nonsensical as it is supposed to test lexing of
        // language features.
        let source = [
            // if statement
            "{{ if page.author }}",
            "<title>{{ page.title }} - {{ page.author }}</title>",
            "{{ else }}",
            "<title>{{ page.title }}</title>",
            "{{ end }}",
            "",
            // for loop
            "{{ for tag in page.tags }}",
            "<span>{{ tag.name }}</span>",
            "{{ end }}",
            "",
            // slots
            "{{ slot \"head\" }}",
            "{{ slot \"page\" }}",
            "",
            // blocks
            "{{ block \"head\" }}",
            "{{ block \"page\" }}",
            // render
            "{{ render \"something.html\" }}",
            "{{ render \"another_thing.html\" }}",
            // extend
            "{{ extend \"template.html\" }}",
            // calling functions
            "{{ site.assets.get('asset_name.extension') }}",
            // variable
            "{{ name := page.title }}",
            // comparisons
            "{{ page.title == \"some title\" }}",
            "{{ page.title != \"some title\" }}",
            "{{ page.words > 1337 }}",
            "{{ page.words >= 1337 }}",
            "{{ page.words < 1337 }}",
            "{{ page.words <= 1337 }}",
            "{{ page.words != 1337 }}",
            // numbers
            "{{ 1337 }}",
            "{{ -1337 }}",
            "{{ 13.37 }}",
            "{{ -13.37 }}",
            "{{ -1337 }}",
            "{{ 13.37 }}",
            "{{ -13.37 }}",
            "{{ 1337 - -1337 }}",
            "{{ -1337 + -1337 }}",
            "{{ -1337+-1337 }}",
            "{{ -1337.1337+-13.33 }}",
        ]
        .join("\n");

        let source = Arc::new(source);
        let lexer = Lexer::new(source.clone());

        let tokens = lexer
            .map(|t| t.unwrap())
            .map(|token| TokenSnapshot::with_content(token, &source[token.loc().range()]))
            .collect::<Vec<_>>();

        insta::assert_debug_snapshot!(tokens);
    }
}
