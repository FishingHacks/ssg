use std::path::PathBuf;
use std::sync::Arc;

use miette::Diagnostic;
use thiserror::Error;
use tokio::task::JoinError;

use crate::config::{ConfigReadError, SiteConfig};
use crate::content::{ContentPage, ContentParseError};
use crate::resources::ContextPipelineError;
use crate::templates::{
    Executor, ProcessedTemplates, TemplateError, TemplateProcessingError, Templates,
};

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
    #[diagnostic(transparent)]
    TemplateProcessing(#[from] TemplateProcessingError),

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
    let processed_templates = crate::templates::process_templates(templates, config.clone())?;
    let pages = crate::content::parse_content(config.clone()).await?;
    resolve_pages(processed_templates, pages, config.clone()).await?;

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

async fn resolve_pages(
    templates: ProcessedTemplates,
    pages: Vec<ContentPage>,
    config: Arc<SiteConfig>,
) -> Result<(), BuildError> {
    let mut executor = Executor::new(&templates);
    let mut handles = Vec::new();
    for page in pages {
        let output = executor.render_content(page.meta, page.html);

        let mut output_path = config.out_dir.join(page.path);
        handles.push(tokio::spawn(async move {
            tokio::fs::create_dir_all(&output_path).await?;
            output_path.push("index.html");
            tokio::fs::write(output_path, output).await
        }));
    }
    futures::future::try_join_all(handles)
        .await?
        .into_iter()
        .collect::<Result<(), _>>()
        .map_err(Into::into)
}
