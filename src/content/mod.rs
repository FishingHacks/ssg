mod markdown;

use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::sync::Arc;

use markdown::{FrontmatterError, Markdown};
use miette::{Diagnostic, NamedSource, SourceSpan};
use path_absolutize::Absolutize;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::config::SiteConfig;

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
    // TODO: Make this be vec<MEtavalue>
    Vec(Vec<String>),
    Bool(bool),
    Number(i32),
    Float(f32),
    // TODO: Make this be hashma<string, MEtavalue>
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
    pub template: PathBuf,
}

#[derive(Debug)]
pub struct ContentPage {
    pub source: Arc<String>,
    pub meta: ContentMeta,
    pub html: String,
    pub path: String,
}

impl ContentPage {
    pub fn meta(&self) -> &ContentMeta {
        &self.meta
    }
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

    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error("Task join error: {0}")]
    Join(#[from] tokio::task::JoinError),

    #[error("No content found")]
    NoContent,
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
    pub fn from_unresolved<S>(
        config: Arc<SiteConfig>,
        mut ctx: MetaResolveContext<'_, S>,
    ) -> Result<Self, ContentParseError>
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

        let template = match ctx.unresolved.0.remove("template") {
            Some(MetaValue::String(template)) => template,
            Some(_) => {
                base_error.help = Some(String::from(
                    r#"The `template` key must be a string

Examples:
template = "base.html"
template = "subdir/page.html"
template = "./subdir/page.html"#,
                ));
                return Err(base_error.into());
            }
            None => {
                base_error.help = Some(String::from(
                    r#"Every content page must have a `template` key.

For templates in subdirectories, you can use relative paths starting from the
`templates` directory.

Examples:
1. template = "base.html"
    - Template would be loaded from <templates_dir>/base.html.

2. template = "subdir/page.html"
   template = "./subdir/page.html"
    - Template would be loaded from <templates_dir>/subdir/base.html"#,
                ));
                return Err(base_error.into());
            }
        };

        // TODO: maybe check if template exists and error early if not
        let template = config
            .templates_dir
            .join(&template)
            .absolutize()
            .expect("template path here should be valid")
            .to_path_buf();

        Ok(Self {
            title,
            draft,
            date,
            tags,
            template,
            description,
            custom: ctx.unresolved.0,
        })
    }
}

pub struct ParsePageCtx {
    pub source: Arc<String>,
    pub file_name: String,
    pub path: PathBuf,
}

pub trait ContentParser {
    fn parse(
        &self,
        config: Arc<SiteConfig>,
        parse_ctx: ParsePageCtx,
    ) -> Result<ContentPage, ContentParseError>;
}

#[derive(Debug)]
pub enum ParserImpl {
    Markdown(Markdown),
}

impl ContentParser for ParserImpl {
    fn parse(
        &self,
        config: Arc<SiteConfig>,
        parse_ctx: ParsePageCtx,
    ) -> Result<ContentPage, ContentParseError> {
        match self {
            Self::Markdown(inner) => inner.parse(config, parse_ctx),
        }
    }
}

pub fn parser_from_filetype(filetype: FileType) -> ParserImpl {
    match filetype {
        FileType::Markdown => ParserImpl::Markdown(markdown::Markdown),
        FileType::Org => todo!(),
    }
}

pub async fn parse_content(
    config: Arc<SiteConfig>,
) -> miette::Result<Vec<ContentPage>, ContentParseError> {
    if !config.content_dir.exists() {
        return Err(ContentParseError::NoContent);
    }

    let handles = walkdir::WalkDir::new(&config.content_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|entry| {
            let path = entry.path().to_path_buf();
            let file_name = path.file_name().unwrap().to_string_lossy().to_string();
            let config = config.clone();

            tokio::spawn(async move {
                let content = Arc::new(tokio::fs::read_to_string(&path).await?);
                let parser = parser_from_filetype(FileType::from(path.extension()));

                parser.parse(
                    config,
                    ParsePageCtx {
                        source: content,
                        file_name,
                        path,
                    },
                )
            })
        });

    let pages = futures::future::try_join_all(handles)
        .await?
        .into_iter()
        .collect::<Result<_, _>>()?;

    Ok(pages)
}
