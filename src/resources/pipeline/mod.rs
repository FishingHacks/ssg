use serde::{Deserialize, Serialize};
use std::{fmt::Debug, fmt::Display, path::PathBuf, process::ExitStatus};

use thiserror::Error;

use crate::config::SiteConfig;

#[derive(Debug, Error)]
pub enum PipelineError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("Process exited unsuccessfully with {0}:\n{strn:?}", strn = std::str::from_utf8(_1))]
    Command(ExitStatus, Vec<u8>),
}

#[derive(Debug, Error)]
pub struct ContextPipelineError {
    pipeline_typ: &'static str,
    file: PathBuf,
    source: std::io::Error,
}

impl Display for ContextPipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Error: ")?;
        Display::fmt(&self.source, f)?;
        f.write_str("\nWhile running pipeline ")?;
        f.write_str(self.pipeline_typ)?;
        f.write_str(" for file ")?;
        Display::fmt(&self.file.display(), f)
    }
}

pub trait ResourcePipeline {
    fn output(&self, input: PathBuf) -> PathBuf;
    fn run(&self, input: Vec<u8>, config: &SiteConfig) -> Result<Vec<u8>, PipelineError>;
}

macro_rules! pipeline_struct {
    ($($module:ident :: $struct:ident $key:literal),* $(,)?) => {
        $(mod $module;)*

        const _: fn() = || {
            struct TraitVerifier<'a, T: Debug + Serialize + Deserialize<'a> + ResourcePipeline>(std::marker::PhantomData<&'a T>);
            $(const _: TraitVerifier<'_, $module::$struct> = TraitVerifier(std::marker::PhantomData);)*
        };

        #[derive(Debug, Serialize, Deserialize)]
        #[serde(tag = "type")]
        pub enum PipelineEntry {
            $(#[serde(rename = $key)] $struct($module::$struct)),*
        }

        impl ResourcePipeline for PipelineEntry {
            fn output(&self, input: PathBuf) -> PathBuf { match self { $(Self::$struct(v) => v.output(input),)* } }
            fn run(&self, input: Vec<u8>, config: &SiteConfig) -> Result<Vec<u8>, PipelineError> { match self { $(Self::$struct(v) => v.run(input, config),)* } }
        }

        impl PipelineEntry {
            pub fn name(&self) -> &'static str { match self { $(Self::$struct(_) => $key,)* } }
        }
    };
}

pipeline_struct! {
    run_command::RunCommand "run.command",
    none::None "none",
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Pipeline {
    entries: Vec<PipelineEntry>,
}
