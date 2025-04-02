mod config;
mod content;
mod resources;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use config::{ConfigReadError, SiteConfig};
use content::{ContentParseError, ContentParser, FileType, parser_from_filetype};
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    match args.command {
        Commands::Serve { path, port } => {
            let port = port.unwrap_or(DEFAULT_PORT);
            let config = config::load(path, port)?;
            build(config).await?;
        }
        Commands::Build { .. } => todo!(),
    }

    Ok(())
}

#[derive(Debug, Error)]
enum BuildError {
    #[error(transparent)]
    Config(#[from] ConfigReadError),
    #[error(transparent)]
    ContentParse(#[from] ContentParseError),
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("Task join error: {0}")]
    Join(#[from] tokio::task::JoinError),
}

async fn build(config: SiteConfig) -> Result<(), BuildError> {
    std::fs::create_dir_all(&config.out_dir)?;

    let content_dir = &config.content_dir;
    if content_dir.exists() {
        let entries: Vec<_> = walkdir::WalkDir::new(content_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .collect();

        let mut handles = vec![];
        for entry in entries {
            let path = entry.path().to_path_buf();

            if path.is_dir() {
                continue;
            }

            let filetype = FileType::from(path.extension());
            let handle = tokio::spawn(async move {
                match filetype {
                    FileType::Markdown => {
                        let content = tokio::fs::read_to_string(&path).await?;
                        let parser = parser_from_filetype(FileType::Markdown);
                        let (_meta, _html) = parser.parse(&content)?;
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

    Ok(())
}
