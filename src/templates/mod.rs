use std::collections::HashMap;
use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error;

use crate::config::{ResolvePath, SiteConfig};
use crate::scripting::{self, ParsingError};

mod inlining;

#[derive(Debug, Error, Diagnostic)]
pub enum TemplateError {
    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error("Task join error: {0}")]
    Join(#[from] tokio::task::JoinError),

    #[error("No templates found")]
    NoTemplates,

    #[error("{0}")]
    CyclicDependency(String),

    #[error("{0}")]
    TemplateNotFound(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    ScriptingError(#[from] ParsingError),
}

#[derive(Debug, Clone)]
pub struct PageTemplate {
    pub path: PathBuf,
    pub ast: scripting::Ast,
}

#[derive(Debug)]
pub struct Templates(pub HashMap<PathBuf, PageTemplate>);

pub async fn load_templates(config: &SiteConfig) -> Result<Templates, TemplateError> {
    if !config.templates_dir.exists() {
        return Err(TemplateError::NoTemplates);
    }

    let handles = walkdir::WalkDir::new(&config.templates_dir)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .map(|entry| {
            let path = entry.into_path().resolve(&config.root_dir);
            tokio::spawn(async move {
                let content = tokio::fs::read_to_string(&path).await?;
                let ast = scripting::parse_template(content)?;
                Result::<_, TemplateError>::Ok(PageTemplate { ast, path })
            })
        });

    let templates = futures::future::try_join_all(handles)
        .await?
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|template| (template.path.clone(), template))
        .collect::<HashMap<_, _>>();

    Ok(Templates(templates))
}
