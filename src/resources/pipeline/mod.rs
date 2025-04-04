use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::process::ExitStatus;

use serde::{Deserialize, Serialize};
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

#[derive(Debug, Serialize, Deserialize)]
pub struct PipelineCommand {
    name: Option<String>,
    command: String,
    arguments: Vec<String>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct PipelineConfig {
    #[serde(default = "Vec::new")]
    pre_build: Vec<PipelineCommand>,
    #[serde(default = "Vec::new")]
    post_build: Vec<PipelineCommand>,
    #[serde(default = "HashMap::new")]
    /// Specifies the pipeline to run for an extension. By default, it's the pipeline with the same
    /// name. Example: `{ "tsx": "pipeline.ts" }`
    extension_map: HashMap<String, String>,
    #[serde(default = "HashMap::new")]
    /// Specifies the pipeline to run for special files.
    /// Example: { "tailwind.scss": "pipeline.tailwind" }
    special_files: HashMap<String, String>,
}
