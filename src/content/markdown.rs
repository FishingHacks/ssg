use std::sync::Arc;

use comrak::{ComrakOptions, markdown_to_html};
use gray_matter::Matter;
use gray_matter::engine::{TOML, YAML};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::{ContentPage, ContentParseError, ContentParser, ParsePageCtx};
use crate::config::SiteConfig;
use crate::content::{ContentMeta, MetaResolveContext, UnresolvedContentMeta};

#[derive(Debug, PartialEq)]
pub enum FrontmatterEngine {
    Yaml,
    Toml,
}

impl FrontmatterEngine {
    pub fn delimiter(&self) -> &'static str {
        match self {
            Self::Yaml => "---",
            Self::Toml => "+++",
        }
    }
}

#[derive(Debug)]
pub struct Markdown;

#[derive(Debug, Error, Diagnostic)]
#[error("While parsing markdown frontmatter")]
#[diagnostic()]
pub struct FrontmatterError {
    #[source_code]
    src: Arc<String>,
    #[label = "in this section"]
    at: SourceSpan,
    #[help]
    help: String,
}

fn detect_frontmatter_engine(content: &Arc<String>) -> Result<FrontmatterEngine, FrontmatterError> {
    if content.starts_with("---") {
        Ok(FrontmatterEngine::Yaml)
    } else if content.starts_with("+++") {
        Ok(FrontmatterEngine::Toml)
    } else {
        Err(FrontmatterError {
            src: content.clone(),
            at: (0..4).into(),
            help: "invalid frontmatter prefix".into(),
        })
    }
}

impl ContentParser for Markdown {
    fn parse(
        &self,
        config: Arc<SiteConfig>,
        parse_ctx: ParsePageCtx,
    ) -> Result<ContentPage, ContentParseError> {
        let engine = detect_frontmatter_engine(&parse_ctx.source)?;

        let result = match engine {
            FrontmatterEngine::Yaml => {
                let matter = Matter::<YAML>::new();
                matter.parse(&parse_ctx.source)
            }
            FrontmatterEngine::Toml => {
                let mut matter = Matter::<TOML>::new();
                matter.delimiter = "+++".to_string();
                matter.parse(&parse_ctx.source)
            }
        };

        let Some(meta) = result.data else {
            let max_ctx = parse_ctx.source.len().min(100);
            return Err(FrontmatterError {
                at: (0..max_ctx).into(),
                src: parse_ctx.source,
                help: format!(
                    "content frontmatter must be closed by the `{}` delimiter",
                    engine.delimiter()
                ),
            }
            .into());
        };

        let meta = meta
            .deserialize::<UnresolvedContentMeta>()
            .expect("encountered valid toml that wasn't supported by ContentMeta.");

        let mut options = ComrakOptions::default();
        options.render.unsafe_ = true;
        let html = markdown_to_html(&result.content, &options);

        let end = result.matter.len() + engine.delimiter().len() * 2;
        let meta_span = 0..end;

        let meta = ContentMeta::from_unresolved(
            config,
            MetaResolveContext {
                source_code: &parse_ctx.source,
                file_name: &parse_ctx.file_name,
                meta_span,
                unresolved: meta,
            },
        )?;

        Ok(ContentPage {
            html,
            meta,
            source: parse_ctx.source,
        })
    }
}

#[cfg(test)]
mod tests {
    use path_absolutize::Absolutize;

    use super::*;

    #[test]
    fn test_detect_frontmatter_engine() {
        let content = "---
date: 2024-02-02T04:14:54-08:00
draft: false
title: Example
---
# My Title
        ";

        let engine = detect_frontmatter_engine(&Arc::new(content.into())).unwrap();
        assert_eq!(engine, FrontmatterEngine::Yaml);

        let content = r#"+++
date = "2024-02-02T04:14:54-08:00"
draft = false
title = "Example"
+++
# My Title
        "#;

        let engine = detect_frontmatter_engine(&Arc::new(content.into())).unwrap();
        assert_eq!(engine, FrontmatterEngine::Toml);

        let content = "
        # My Title
        ";

        let result = detect_frontmatter_engine(&Arc::new(content.into()));
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_yaml() {
        let content = "---
date: 2024-02-02T04:14:54-08:00
draft: false
title: Example
template: index.html
---
# My Title
        ";

        let config = Arc::new(SiteConfig::default());
        let context = ParsePageCtx {
            source: Arc::new(content.into()),
            file_name: "index.md".into(),
        };
        let result = Markdown.parse(config.clone(), context).unwrap();
        assert_eq!(result.meta.title, "Example");
        assert!(!result.meta.draft);
        assert_eq!(
            result.meta.template,
            config
                .templates_dir
                .join("index.html")
                .absolutize()
                .unwrap()
        );
        assert_eq!(result.meta.date, "2024-02-02T04:14:54-08:00");
        assert_eq!(result.html, "<h1>My Title</h1>\n");
    }

    #[test]
    fn test_parse_toml() {
        let content = r#"+++
date = "2024-02-02T04:14:54-08:00"
draft = false
title = "Example"
template = "index.html"
+++
# My Title
        "#;

        let config = Arc::new(SiteConfig::default());
        let context = ParsePageCtx {
            source: Arc::new(content.into()),
            file_name: "index.md".into(),
        };
        let result = Markdown.parse(config.clone(), context).unwrap();
        assert_eq!(result.meta.title, "Example");
        assert!(!result.meta.draft);
        assert_eq!(
            result.meta.template,
            config
                .templates_dir
                .join("index.html")
                .absolutize()
                .unwrap()
        );
        assert_eq!(result.meta.date, "2024-02-02T04:14:54-08:00");
        assert_eq!(result.html, "<h1>My Title</h1>\n");
    }
}
