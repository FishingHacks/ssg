mod markdown;

use std::collections::HashMap;
use std::ffi::OsStr;

use markdown::{FrontmatterError, Markdown};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FileType {
    Markdown,
    Org,
}

impl From<Option<&OsStr>> for FileType {
    fn from(extension: Option<&OsStr>) -> Self {
        match extension.and_then(|os_str| os_str.to_str()) {
            Some("md") | Some("markdown") => FileType::Markdown,
            Some("org") => FileType::Org,
            _ => todo!(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum MetaValue {
    String(String),
    Vec(Vec<String>),
    HashMap(HashMap<String, String>),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UnresolvedContentMeta {
    pub title: String,
    pub draft: bool,
    // TODO: i'm not sure if date should be an option or a required field
    // same goes for tags and description, for now only date is required
    //
    // also, maybe we should use `DateTime` from chrono here?
    pub date: String,
    pub tags: Option<Vec<String>>,
    pub description: Option<String>,
    pub custom: Option<HashMap<String, MetaValue>>,
}

#[derive(Debug)]
pub struct ContentMeta {
    pub title: String,
    pub draft: bool,
    pub date: String,
    pub tags: Vec<String>,
    pub description: String,
    pub custom: HashMap<String, MetaValue>,
}

impl From<UnresolvedContentMeta> for ContentMeta {
    fn from(meta: UnresolvedContentMeta) -> Self {
        Self {
            title: meta.title,
            draft: meta.draft,
            date: meta.date,
            tags: meta.tags.unwrap_or_default(),
            description: meta.description.unwrap_or_default(),
            custom: meta.custom.unwrap_or_default(),
        }
    }
}

type Html = String;

#[derive(Debug, Error)]
pub enum ContentParseError {
    #[error("{0}")]
    FrontmatterError(#[from] FrontmatterError),
}

pub trait ContentParser {
    fn parse<S: AsRef<str>>(&self, content: S) -> Result<(ContentMeta, Html), ContentParseError>;
}

#[derive(Debug)]
pub enum ParserImpl {
    Markdown(Markdown),
}

impl ContentParser for ParserImpl {
    fn parse<S: AsRef<str>>(&self, content: S) -> Result<(ContentMeta, Html), ContentParseError> {
        match self {
            Self::Markdown(inner) => inner.parse(content),
        }
    }
}

pub fn parser_from_filetype(filetype: FileType) -> ParserImpl {
    match filetype {
        FileType::Markdown => ParserImpl::Markdown(markdown::Markdown),
        FileType::Org => todo!(),
    }
}
