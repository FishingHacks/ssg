mod markdown;

use std::collections::HashMap;

use markdown::Markdown;

#[derive(Debug)]
enum FileType {
    Markdown,
    OrgMode,
}

pub enum MetaValue {
    String,
    Vec(Vec<String>),
    HashMap(HashMap<String, String>),
}

pub struct ContentMeta(HashMap<String, MetaValue>);

type Html = String;

pub trait ContentParser {
    fn parse(&self, content: &[u8]) -> (ContentMeta, Html);
}

#[derive(Debug)]
pub enum ParserImpl {
    Markdown(Markdown),
}

impl ContentParser for ParserImpl {
    fn parse(&self, content: &[u8]) -> (ContentMeta, Html) {
        match self {
            Self::Markdown(inner) => inner.parse(content),
        }
    }
}

pub fn parser_from_filetype(filetype: FileType) -> ParserImpl {
    match filetype {
        FileType::Markdown => ParserImpl::Markdown(markdown::Markdown),
        FileType::OrgMode => todo!(),
    }
}
