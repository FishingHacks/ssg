mod markdown;

use std::collections::HashMap;
use std::ffi::OsStr;
use std::sync::Arc;

use markdown::{FrontmatterError, Markdown};
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FileType {
    Markdown,
    Org,
    // TODO: HTML, Asciidoc, reStructedText
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
#[serde(untagged)]
pub enum MetaValue {
    String(String),
    Vec(Vec<String>),
    Bool(bool),
    Number(i32),
    Float(f32),
    HashMap(HashMap<String, String>),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UnresolvedContentMeta(HashMap<String, MetaValue>);

#[derive(Debug)]
pub struct ContentMeta {
    pub title: String,
    pub draft: bool,
    pub date: String,
    pub tags: Vec<String>,
    pub description: String,
    pub custom: HashMap<String, MetaValue>,
}

#[derive(Debug)]
pub struct ContentPage {
    source: Arc<String>,
    meta: ContentMeta,
    html: Html,
}

#[derive(Debug, Error, Diagnostic)]
#[error("While parsing content metadata")]
#[diagnostic()]
pub struct InvalidMetadata {
    #[source_code]
    source_code: NamedSource<Arc<String>>,
    #[label = "in this section"]
    at: SourceSpan,
    #[help]
    help: Option<String>,
}

#[derive(Debug, Diagnostic, Error)]
pub enum ContentParseError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    FrontmatterError(#[from] FrontmatterError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidMetadata(#[from] InvalidMetadata),
}

pub struct MetaResolveContext<'ctx, S>
where
    S: Into<SourceSpan>,
{
    file_name: &'ctx str,
    source_code: &'ctx Arc<String>,
    unresolved: UnresolvedContentMeta,
    meta_span: S,
}

impl ContentMeta {
    pub fn from_unresolved<S>(mut ctx: MetaResolveContext<'_, S>) -> Result<Self, ContentParseError>
    where
        S: Into<SourceSpan>,
    {
        let mut base_error = InvalidMetadata {
            source_code: NamedSource::new(ctx.file_name, ctx.source_code.clone()),
            at: ctx.meta_span.into(),
            help: None,
        };

        let title = match ctx.unresolved.0.remove("title") {
            Some(MetaValue::String(title)) => title,
            Some(_) => {
                base_error.help = Some(String::from(
                    "The `title` key must be a string. Example: \n\
                    `title = \"My cat pictures.\"`",
                ));
                return Err(base_error.into());
            }
            None => {
                base_error.help = Some(String::from(
                    "Every content page must have a `title` key. Example:\n\
                    `title = \"My cat pictures.\"`",
                ));
                return Err(base_error.into());
            }
        };

        let draft = match ctx.unresolved.0.remove("draft") {
            Some(MetaValue::Bool(draft)) => draft,
            Some(_) => {
                base_error.help = Some(String::from(
                    "The `draft` key must be a boolean (`true` or `false`). Example:\n\
                    `draft = true`",
                ));
                return Err(base_error.into());
            }
            None => {
                base_error.help = Some(String::from(
                    "The `draft` key is missing. If this is a draft, explicitly set:\n\
                    `draft = true`",
                ));
                return Err(base_error.into());
            }
        };

        let date = match ctx.unresolved.0.remove("date") {
            Some(MetaValue::String(date)) => date,
            Some(_) => {
                base_error.help = Some(String::from(
                    "The `date` key must be a date string. Example:\n\
                    `date = \"2025-04-01\"` \n\
                    `date = \"2025-04-01 13:02:44\"`",
                ));
                return Err(base_error.into());
            }
            None => {
                base_error.help = Some(String::from(
                    "A `date` key is recommended for sorting content. Example:\n\
                    `date = \"2025-04-01\"` \n\
                    `date = \"2025-04-01 13:02:44\"`",
                ));
                return Err(base_error.into());
            }
        };

        let tags = match ctx.unresolved.0.remove("tags") {
            Some(MetaValue::Vec(tags)) => tags,
            Some(_) => {
                base_error.help = Some(String::from(
                    "The `tags` key must be a list of strings. Example:\n\
                    `tags = [\"cat\", \"memes\"]`",
                ));
                return Err(base_error.into());
            }
            None => Default::default(),
        };

        let description = match ctx.unresolved.0.remove("description") {
            Some(MetaValue::String(tags)) => tags,
            Some(_) => {
                base_error.help = Some(String::from(
                    "The `description` key must be a string. Example:\n\
                    `description = \"My collection of cat memes.\"`",
                ));
                return Err(base_error.into());
            }
            None => Default::default(),
        };

        Ok(Self {
            title,
            draft,
            date,
            tags,
            description,
            custom: ctx.unresolved.0,
        })
    }
}

type Html = String;

pub struct ParsePageCtx {
    pub source: Arc<String>,
    pub file_name: String,
}

pub trait ContentParser {
    fn parse(&self, parse_ctx: ParsePageCtx) -> Result<ContentPage, ContentParseError>;
}

#[derive(Debug)]
pub enum ParserImpl {
    Markdown(Markdown),
}

impl ContentParser for ParserImpl {
    fn parse(&self, parse_ctx: ParsePageCtx) -> Result<ContentPage, ContentParseError> {
        match self {
            Self::Markdown(inner) => inner.parse(parse_ctx),
        }
    }
}

pub fn parser_from_filetype(filetype: FileType) -> ParserImpl {
    match filetype {
        FileType::Markdown => ParserImpl::Markdown(markdown::Markdown),
        FileType::Org => todo!(),
    }
}
