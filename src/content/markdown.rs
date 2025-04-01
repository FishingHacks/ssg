use super::ContentParser;

#[derive(Debug)]
pub struct Markdown;

impl ContentParser for Markdown {
    fn parse(&self, _: &[u8]) -> (super::ContentMeta, super::Html) {
        todo!()
    }
}
