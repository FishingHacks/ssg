mod config;
mod content;
mod resources;
mod scripting;
mod templates;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use clap::{Parser, Subcommand};
use config::{ConfigReadError, SiteConfig};
use content::ContentParseError;
use miette::{Diagnostic, miette};
use resources::ContextPipelineError;
use templates::TemplateError;
use thiserror::Error;

static DEFAULT_PORT: u16 = 8000;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Serve {
        #[arg(long)]
        path: Option<PathBuf>,
        #[arg(long)]
        port: Option<u16>,
    },
    Build {
        #[arg(short, long)]
        path: Option<PathBuf>,
    },
}

#[derive(Debug, Default)]
struct BuildStep {
    contents: HashMap<PathBuf, Arc<String>>,
}

#[derive(Debug, Error, Diagnostic)]
enum BuildError {
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

#[tokio::main]
async fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Serve { path, port } => {
            let port = port.unwrap_or(DEFAULT_PORT);
            let config = config::load(path, port).map_err(BuildError::from)?;
            build(config).await?;
        }
        Commands::Build { .. } => todo!(),
    }

    Ok(())
}

async fn build(config: SiteConfig) -> miette::Result<BuildStep, BuildError> {
    let config = Arc::new(config);
    let mut build_step = BuildStep::default();

    config.pipeline_cfg.run_pre_build_pipeline(&config).await?;
    let resources = config.pipeline_cfg.index_resources(&config);

    let templates = templates::load_templates(&config).await?;
    let pages = content::parse_content(&config).await?;

    let resource_handles = futures::future::join_all(resources.iter().map(PathBuf::from).map(|path| {
        let config = config.clone();
        tokio::spawn(async move {
            let config = config;
            config.pipeline_cfg.run_pipeline_for_file(path, &config).await
        })
    }))
    .await;

    for handle in resource_handles {
        handle??;
    }

    config.pipeline_cfg.run_post_build_pipeline(&config).await?;

    Ok(build_step)
}
