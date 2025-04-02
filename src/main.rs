mod config;
mod content;

use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use config::{SiteConfig, UnresolvedSiteConfig};
use thiserror::Error;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Serve {
        #[arg(short, long)]
        path: Option<PathBuf>,
        #[arg(short, long)]
        port: Option<u16>,
    },
    Build {
        #[arg(short, long)]
        path: Option<PathBuf>,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Serve { path, port } => {
            let path = path
                .map(Ok)
                .unwrap_or_else(std::env::current_dir)
                .expect("failed to get the current directory");
            let port = port.unwrap_or(8000);
            let config = build(&path).expect("failed to build");
            // serve (open up a server)
        }
        Commands::Build { path: Some(path) } => _ = build(&path).expect("failed to build"),
        Commands::Build { path: None } => _ = build(&std::env::current_dir().unwrap()).expect("failed to build"),
    }
}

#[derive(Debug, Error)]
enum BuildError {
    #[error("{0}")]
    Config(#[from] ConfigReadError),
}

#[derive(Debug, Error)]
enum ConfigReadError {
    #[error("{0}")]
    Toml(#[from] toml::de::Error),
    #[error("{0}")]
    IO(#[from] std::io::Error),
}

fn read_config(root_dir: &Path) -> Result<SiteConfig, ConfigReadError> {
    let config_path = root_dir.join("config.toml");
    if !config_path.exists() {
        return Ok(SiteConfig::from_unresolved(root_dir, Default::default()));
    }
    let config_str = std::fs::read_to_string(&config_path)?;
    let unresolved_config = toml::from_str::<UnresolvedSiteConfig>(&config_str)?;
    Ok(SiteConfig::from_unresolved(root_dir, unresolved_config))
}

fn build(root_dir: &Path) -> Result<SiteConfig, BuildError> {
    // read config file
    let config = read_config(root_dir)?;

    // index resources
    // index templates
    // parse content
    // build website (copy files and stuff)
    todo!()
}
