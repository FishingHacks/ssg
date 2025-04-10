mod build;
mod config;
mod content;
mod resources;
mod scripting;
mod templates;

use std::path::PathBuf;

use build::BuildError;
use clap::{Parser, Subcommand};

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
async fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Serve { path, port } => {
            let config = config::load(path, port).map_err(BuildError::from)?;
            build::build(config).await?;
        }
        Commands::Build { path } => {
            let config = config::load(path, None).map_err(BuildError::from)?;
            build::build(config).await?;
        }
    }

    Ok(())
}
