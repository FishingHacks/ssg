use std::ops::ControlFlow;
use std::sync::Arc;

use super::{ByteOffset, ParsingError};

#[derive(Debug)]
enum LexingMode {
    Html,
    Script,
}

#[derive(Debug)]
pub struct Lexer {
    cursor: usize,
    source: Arc<String>,
    mode: LexingMode,
    html_start: Option<usize>,
    script_start: Option<usize>,
    peeked: Option<Result<Token, ParsingError>>,
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
    Float(ByteOffset),
    Int(ByteOffset),
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
            | Token::Symbol(_, byte_offset)
            | Token::Float(byte_offset)
            | Token::Int(byte_offset) => *byte_offset,
        }
    }
}

impl Lexer {
    pub fn new(source: Arc<String>) -> Self {
        Self {
            source,
            cursor: 0,
            peeked: None,
            html_start: None,
            script_start: None,
            mode: LexingMode::Html,
        }
    }

    fn is_keyword(&self, ident: &str) -> bool {
        matches!(
            ident,
            "for" | "in" | "end" | "if" | "else" | "block" | "render" | "slot" | "extend"
        )
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

                ControlFlow::Break(Ok(Token::ExprStart((start, self.cursor).into())))
            }
            ('}', Some('}')) => {
                self.mode = LexingMode::Html;
                self.cursor += 2;

                let start = self
                    .script_start
                    .take()
                    .expect("script start must be set while lexing script content");

                ControlFlow::Break(Ok(Token::ExprEnd((start, self.cursor).into())))
            }
            (':', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Keyword((self.cursor - 2, self.cursor).into())))
            }
            ('=', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Keyword((self.cursor - 2, self.cursor).into())))
            }
            ('!', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Keyword((self.cursor - 2, self.cursor).into())))
            }
            ('>', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Keyword((self.cursor - 2, self.cursor).into())))
            }
            ('<', Some('=')) => {
                self.advance_script(2);
                ControlFlow::Break(Ok(Token::Keyword((self.cursor - 2, self.cursor).into())))
            }
            ('>', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            ('<', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            ('.', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            ('(', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            (')', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            ('-', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
            }
            ('+', _) => {
                self.advance_script(1);
                ControlFlow::Break(Ok(Token::Symbol(curr, (self.cursor - 1, self.cursor).into())))
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

                    if self.is_keyword(content) {
                        return ControlFlow::Break(Ok(Token::Keyword((start, end).into())));
                    }

                    return ControlFlow::Break(Ok(Token::Identifier((start, end).into())));
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
            if let Some(peeked) = self.peeked.take() {
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
        self.is_ascii_alphanumeric()
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
