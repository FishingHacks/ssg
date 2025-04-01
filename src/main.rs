mod config;
mod content;

use std::path::PathBuf;

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
        Commands::Serve { .. } => {
            // read config file
            // index resources
            // index templates
            // parse content
            // build website (copy files and stuff)
            // serve (open up a server)
        }
        Commands::Build { .. } => {
            // read config file
            // index resources
            // index templates
            // parse content
            // build website (copy files and stuff)
        }
    }
}
