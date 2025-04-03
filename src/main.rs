mod config;
mod content;
mod resources;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use clap::{Parser, Subcommand};
use config::{ConfigReadError, SiteConfig};
use content::{ContentParseError, ContentParser, FileType, ParsePageCtx, parser_from_filetype};
use miette::Diagnostic;
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
    IO(#[from] std::io::Error),

    #[error("Task join error: {0}")]
    Join(#[from] tokio::task::JoinError),
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
    std::fs::create_dir_all(&config.out_dir)?;
    let mut build_step = BuildStep::default();

    let content_dir = &config.content_dir;
    if content_dir.exists() {
        let entries: Vec<_> = walkdir::WalkDir::new(content_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .collect();

        let mut handles = vec![];
        for entry in entries {
            let path = entry.path().to_path_buf();
            let file_name = path.file_name().unwrap().to_string_lossy().to_string();

            if path.is_dir() {
                continue;
            }

            let filetype = FileType::from(path.extension());
            let content = Arc::new(tokio::fs::read_to_string(&path).await?);
            build_step.contents.insert(path, content.clone());

            let handle = tokio::spawn(async move {
                match filetype {
                    FileType::Markdown => {
                        let parser = parser_from_filetype(FileType::Markdown);
                        let page = parser.parse(ParsePageCtx {
                            source: content.clone(),
                            file_name,
                        })?;
                        println!("{page:?}");
                        // TODO: actually do something here lol
                    }
                    _ => todo!(),
                }
                Ok::<_, BuildError>(())
            });

            handles.push(handle);
        }

        for handle in handles {
            handle.await??;
        }
    }

    Ok(build_step)
}
