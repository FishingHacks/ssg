use std::collections::HashMap;
use std::fmt::{Debug, Display, Write};
use std::path::{Path, PathBuf};
use std::process::ExitStatus;

use run_command::{RunCommand, run_commands_pipeline};
use serde::{Deserialize, Serialize};
use tempfile::NamedTempFile;
use thiserror::Error;

use crate::config::SiteConfig;

mod run_command;

#[derive(Debug, Error)]
pub enum PipelineError {
    // TODO: Specify why/where this IO error occurred (e.g. while copying a file from resource to
    // dist)
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("Process exited unsuccessfully with {0}:\n{strn:?}", strn = std::str::from_utf8(_1))]
    Command(ExitStatus, Vec<u8>),
}

#[derive(Debug, Error)]
pub struct ContextPipelineError {
    pipeline_name: String,
    file: Option<PathBuf>,
    step: usize,
    source: PipelineError,
}

impl ContextPipelineError {
    pub fn new(
        name: impl Into<String>,
        file: impl Into<Option<PathBuf>>,
        step: usize,
        source: impl Into<PipelineError>,
    ) -> Self {
        Self {
            pipeline_name: name.into(),
            file: file.into(), //.map(Into::into),
            step,
            source: source.into(),
        }
    }
}

impl Display for ContextPipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Error: ")?;
        Display::fmt(&self.source, f)?;
        f.write_str("\nWhile running pipeline ")?;
        f.write_str(&self.pipeline_name)?;
        if let Some(file) = &self.file {
            f.write_str(" for file ")?;
            Display::fmt(&file.display(), f)?;
        }
        f.write_str(" (Step ")?;
        Display::fmt(&self.step, f)?;
        f.write_str(")")
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Pipeline {
    entries: Vec<RunCommand>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PipelineCommand {
    cmd: String,
    arguments: Vec<String>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct UnresolvedPipelineConfig {
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

#[derive(Debug)]
pub struct PipelineConfig {
    pre_build: Vec<PipelineCommand>,
    post_build: Vec<PipelineCommand>,
    /// Specifies the pipeline to run for an extension. By default, it's the pipeline with the same
    /// name. Example: `{ "tsx": "pipeline.ts" }`
    extension_map: HashMap<String, String>,
    /// Specifies the pipeline to run for special files.
    /// Example: { "tailwind.scss": "pipeline.tailwind" }
    special_files: HashMap<String, String>,
    single_file_pipelines: HashMap<String, Pipeline>,
}

impl PipelineConfig {
    pub fn from_unresolved(
        unresolved: UnresolvedPipelineConfig,
        single_file_pipelines: HashMap<String, Pipeline>,
    ) -> Self {
        Self {
            pre_build: unresolved.pre_build,
            post_build: unresolved.post_build,
            extension_map: unresolved.extension_map,
            special_files: unresolved.special_files,
            single_file_pipelines,
        }
    }
}

impl Pipeline {
    pub async fn run_pipeline(
        &self,
        path: PathBuf,
        name: &str,
        config: &SiteConfig,
    ) -> Result<PathBuf, ContextPipelineError> {
        assert!(path.is_relative());
        let mut outpath = config.out_dir.join(&path);
        match tokio::fs::create_dir_all(
            outpath
                .parent()
                .expect("outpath has to have a parent directory"),
        )
        .await
        {
            Ok(_) => {}
            Err(e) => return Err(ContextPipelineError::new(name, Some(path), 0, e)),
        }
        let inpath = config.root_dir.join(&path);
        if self.entries.is_empty() {
            println!("Running pipeline {name} for {} (Step 0/0)", path.display());
            return match tokio::fs::copy(&inpath, &outpath).await {
                Ok(_) => Ok(outpath),
                Err(e) => Err(ContextPipelineError::new(name, Some(path), 0, e)),
            };
        }
        for step in self.entries.iter() {
            outpath = step.output_file(outpath);
        }

        // PERF: This creates between 1 unused files: the `infile` (here the `outfile`, will be
        // re-assigned in the first iteration), that's unused by the first loop due to using
        // `inpath`
        let mut infile;
        let mut outfile = NamedTempFile::new().expect("failed to create a tempfile");

        let entries = self.entries.len();
        for (idx, entry) in self.entries.iter().enumerate() {
            println!(
                "Running pipeline {name} for {} (Step {}/{entries})",
                idx + 1,
                path.display()
            );
            // make the previous outfile this infile and create a new outfile
            infile = outfile;
            let input = if idx == 0 { &inpath } else { infile.path() };

            // last iteration
            if idx == entries - 1 {
                if let Err(e) = entry.run(input, &outpath, config).await {
                    return Err(ContextPipelineError::new(name, Some(path), idx, e));
                }
                break;
            }
            outfile = NamedTempFile::new().expect("failed to create a tempfile");

            if let Err(e) = entry.run(input, outfile.path(), config).await {
                return Err(ContextPipelineError::new(name, Some(path), idx, e));
            }
        }

        Ok(outpath)
    }

    pub fn result_file(&self, mut file: PathBuf) -> PathBuf {
        assert!(file.is_relative());
        for entry in self.entries.iter() {
            file = entry.output_file(file)
        }
        file
    }
}

pub struct IndexedResources(HashMap<PathBuf, PathBuf>);

impl IndexedResources {
    pub fn get_output_file(&self, file: &Path) -> Option<&Path> {
        self.0.get(file).map(PathBuf::as_path)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Path> {
        self.0.keys().map(PathBuf::as_path)
    }
}

impl Display for IndexedResources {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("--- Indexed Resources: --------------------------------------------------------------\n")?;
        for (f1, f2) in self.0.iter() {
            Display::fmt(&f1.display(), f)?;
            f.write_str(" => ")?;
            Display::fmt(&f2.display(), f)?;
            f.write_char('\n')?;
        }
        f.write_str(
            "-------------------------------------------------------------------------------------",
        )
    }
}

impl PipelineConfig {
    pub fn pipeline_for_file<'a, 'b: 'a, 'c: 'a>(&'b self, path: &'c Path) -> Option<&'a str> {
        assert!(path.is_relative());
        if let Some(pipeline) = path
            .file_name()
            .and_then(|v| v.to_str())
            .and_then(|v| self.special_files.get(v))
        {
            if let Some(pipeline_name) = pipeline.strip_prefix("pipeline.") {
                return Some(pipeline_name);
            };
        }
        let extension = path.extension()?.to_str()?;
        let Some(pipeline) = self.extension_map.get(extension) else { return Some(extension) };
        Some(pipeline.strip_prefix("pipeline.").unwrap_or(extension))
    }

    pub async fn run_pre_build_pipeline(
        &self,
        config: &SiteConfig,
    ) -> Result<(), ContextPipelineError> {
        run_commands_pipeline(config, &self.pre_build, "pre-build").await
    }

    pub async fn run_post_build_pipeline(
        &self,
        config: &SiteConfig,
    ) -> Result<(), ContextPipelineError> {
        run_commands_pipeline(config, &self.post_build, "post-build").await
    }

    pub async fn run_pipeline_for_file(
        &self,
        path: PathBuf,
        config: &SiteConfig,
    ) -> Result<(), ContextPipelineError> {
        let Some(pipeline) = self.pipeline_for_file(&path) else {
            println!(
                "Warning: no pipeline registered for file {}",
                path.display()
            );
            return Ok(());
        };
        let Some((name, pipeline)) = self.single_file_pipelines.get_key_value(pipeline) else {
            println!(
                "Warning: no pipeline registered for file {}",
                path.display()
            );
            return Ok(());
        };
        pipeline.run_pipeline(path, name, config).await?;
        Ok(())
    }

    pub fn index_resources(&self, config: &SiteConfig) -> IndexedResources {
        let mut resources = IndexedResources(HashMap::new());
        for resource_dir in config.resources_dirs.iter() {
            self.inner_index_resources(config, &mut resources, resource_dir);
        }
        resources
    }

    fn inner_index_resources(
        &self,
        config: &SiteConfig,
        resources: &mut IndexedResources,
        path: &Path,
    ) {
        let Ok(dir_entries) = std::fs::read_dir(path) else { return };
        let path = path.strip_prefix(&config.root_dir).unwrap_or(path);
        for entry in dir_entries {
            let Ok(entry) = entry else { continue };
            let Ok(ft) = entry.file_type() else { continue };
            let new_path = path.join(entry.file_name());
            if ft.is_dir() {
                self.inner_index_resources(config, resources, &new_path);
            } else if ft.is_file() {
                // we skip resources we cannot process because for getting the resulting file of a
                // resource, we have to know its output, and as such also know its pipeline.
                let Some(pipeline) = self.pipeline_for_file(&new_path) else { continue };
                let Some(pipeline) = self.single_file_pipelines.get(pipeline) else { continue };
                let result = pipeline.result_file(new_path.clone());
                resources.0.insert(new_path, result);
            }
        }
    }
}
