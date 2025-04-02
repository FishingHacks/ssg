use comrak::{ComrakOptions, markdown_to_html};
use gray_matter::Matter;
use gray_matter::engine::{TOML, YAML};
use thiserror::Error;

use super::{ContentMeta, ContentParseError, ContentParser, Html};
use crate::content::UnresolvedContentMeta;

#[derive(Debug, PartialEq)]
enum FrontmatterEngine {
    Yaml,
    Toml,
}

#[derive(Debug, Error, PartialEq)]
pub enum FrontmatterError {
    #[error("No frontmatter found")]
    NoFrontmatter,
}

#[derive(Debug)]
pub struct Markdown;

fn detect_frontmatter_engine<S: AsRef<str>>(content: S) -> Result<FrontmatterEngine, FrontmatterError> {
    let content = content.as_ref();
    if content.starts_with("---") {
        Ok(FrontmatterEngine::Yaml)
    } else if content.starts_with("+++") {
        Ok(FrontmatterEngine::Toml)
    } else {
        Err(FrontmatterError::NoFrontmatter)
    }
}

impl ContentParser for Markdown {
    fn parse<S: AsRef<str>>(&self, content: S) -> Result<(ContentMeta, Html), ContentParseError> {
        let content_str = content.as_ref();

        let engine = detect_frontmatter_engine(content_str)?;

        let result = match engine {
            FrontmatterEngine::Yaml => {
                let matter = Matter::<YAML>::new();
                matter.parse_with_struct::<UnresolvedContentMeta>(content_str)
            }
            FrontmatterEngine::Toml => {
                let mut matter = Matter::<TOML>::new();
                matter.delimiter = "+++".to_string();
                matter.parse_with_struct::<UnresolvedContentMeta>(content_str)
            }
        };

        let Some(result) = result else {
            return Err(FrontmatterError::NoFrontmatter.into());
        };

        let content_meta = result.data;
        let content_body = result.content;

        let mut options = ComrakOptions::default();
        options.render.unsafe_ = true;
        let html = markdown_to_html(&content_body, &options);

        Ok((content_meta.into(), html))
    }
}

#[cfg(test)]
mod tests {
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

        let engine = detect_frontmatter_engine(content).unwrap();
        assert_eq!(engine, FrontmatterEngine::Yaml);

        let content = r#"+++
date = "2024-02-02T04:14:54-08:00"
draft = false
title = "Example"
+++
# My Title
        "#;

        let engine = detect_frontmatter_engine(content).unwrap();
        assert_eq!(engine, FrontmatterEngine::Toml);

        let content = "
        # My Title
        ";

        let engine = detect_frontmatter_engine(content).unwrap_err();
        assert_eq!(engine, FrontmatterError::NoFrontmatter);
    }

    #[test]
    fn test_parse_yaml() {
        let content = "---
date: 2024-02-02T04:14:54-08:00
draft: false
title: Example
---
# My Title
        ";

        let result = Markdown.parse(content).unwrap();
        assert_eq!(result.0.title, "Example");
        assert_eq!(result.1, "<h1>My Title</h1>\n");
    }

    #[test]
    fn test_parse_toml() {
        let content = r#"+++
date = "2024-02-02T04:14:54-08:00"
draft = false
title = "Example"
+++
# My Title
        "#;

        let result = Markdown.parse(content).unwrap();
        assert_eq!(result.0.title, "Example");
        assert_eq!(result.1, "<h1>My Title</h1>\n");
    }
}
