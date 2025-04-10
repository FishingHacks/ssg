use std::path::PathBuf;
use std::sync::Arc;

use miette::Diagnostic;
use thiserror::Error;

use crate::config::{ConfigReadError, SiteConfig};
use crate::content::{ContentPage, ContentParseError};
use crate::resources::ContextPipelineError;
use crate::templates::{TemplateError, Templates};

#[derive(Debug, Error, Diagnostic)]
pub enum BuildError {
    #[error(transparent)]
    Config(#[from] ConfigReadError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    ContentParse(#[from] ContentParseError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    TemplateError(#[from] TemplateError),

    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error("Task join error: {0}")]
    Join(#[from] tokio::task::JoinError),

    #[error(transparent)]
    Pipeline(#[from] ContextPipelineError),
}

pub async fn build(config: SiteConfig) -> miette::Result<(), BuildError> {
    let config = Arc::new(config);
    // ensure output dir exists
    std::fs::create_dir_all(&config.out_dir)?;

    config.pipeline_cfg.run_pre_build_pipeline(&config).await?;
    let resources = config.pipeline_cfg.index_resources(&config);

    let templates = crate::templates::load_templates(&config).await?;
    let pages = crate::content::parse_content(config.clone()).await?;
    resolve_pages(templates, pages).await;

    let resource_handles =
        futures::future::join_all(resources.iter().map(PathBuf::from).map(|path| {
            let config = config.clone();
            tokio::spawn(async move {
                let config = config;
                config
                    .pipeline_cfg
                    .run_pipeline_for_file(path, &config)
                    .await
            })
        }))
        .await;

    for handle in resource_handles {
        handle??;
    }

    config.pipeline_cfg.run_post_build_pipeline(&config).await?;

    Ok(())
}

async fn resolve_pages(templates: Templates, pages: Vec<ContentPage>) {
    let templates = Arc::new(templates);

    for page in pages {
        let _templates = templates.clone();

        tokio::spawn(async move {
            let templates = _templates;
            let Some(template) = templates.0.get(&page.meta().template) else { todo!() };
        });
    }
}
