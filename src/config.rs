use std::collections::HashMap;
use std::path::{Path, PathBuf};

use path_absolutize::Absolutize;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::resources::{Pipeline, PipelineConfig, UnresolvedPipelineConfig};

#[derive(Debug)]
pub struct SiteConfig {
    pub port: u16,
    pub resources_dirs: Vec<PathBuf>,
    pub templates_dir: PathBuf,
    pub content_dir: PathBuf,
    pub out_dir: PathBuf,
    pub root_dir: PathBuf,
    pub index_page: String,
    pub pipeline_cfg: PipelineConfig,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct UnresolvedSiteConfig {
    resources_dirs: Option<Vec<PathBuf>>,
    templates_dir: Option<PathBuf>,
    content_dir: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    index_page: Option<String>,
    pipeline: Option<HashMap<String, Pipeline>>,
    pipelines: Option<UnresolvedPipelineConfig>,
}

impl SiteConfig {
    pub fn from_unresolved(root_dir: PathBuf, port: u16, unresolved: UnresolvedSiteConfig) -> Self {
        let resources_dirs = match unresolved.resources_dirs {
            Some(dirs) => dirs.into_iter().map(|v| root_dir.join(v)).collect(),
            None => vec![root_dir.join("resources")],
        };

        let templates_dir = unresolved
            .templates_dir
            .resolve_with_default(&root_dir, Path::new("templates"));

        let content_dir = unresolved
            .content_dir
            .resolve_with_default(&root_dir, Path::new("content"));

        let out_dir = unresolved
            .out_dir
            .resolve_with_default(&root_dir, Path::new("dist"));

        let index_page = unresolved.index_page.unwrap_or_else(|| "_index.md".into());

        let pipeline_cfg = PipelineConfig::from_unresolved(
            unresolved.pipelines.unwrap_or_default(),
            unresolved.pipeline.unwrap_or_default(),
        );

        Self {
            root_dir,
            port,
            resources_dirs,
            templates_dir,
            content_dir,
            out_dir,
            index_page,
            pipeline_cfg,
        }
    }
}

pub trait ResolvePath {
    fn resolve_with_default<T: AsRef<Path>, U: AsRef<Path>>(
        self,
        base_dir: T,
        default: U,
    ) -> PathBuf;
    fn resolve<T: AsRef<Path>>(self, base_dir: T) -> PathBuf;
}

impl<T: AsRef<Path>> ResolvePath for Option<T> {
    fn resolve_with_default<P: AsRef<Path>, U: AsRef<Path>>(
        self,
        base_dir: P,
        default: U,
    ) -> PathBuf {
        let Some(path) = self else { return base_dir.as_ref().join(default) };

        if !path.as_ref().exists() {
            return base_dir.as_ref().join(default);
        }

        base_dir.as_ref().join(path)
    }

    fn resolve<P: AsRef<Path>>(self, base_dir: P) -> PathBuf {
        let Some(path) = self else { return base_dir.as_ref().to_path_buf() };

        if !path.as_ref().exists() {
            return base_dir.as_ref().to_path_buf();
        }

        base_dir.as_ref().join(path)
    }
}

impl<T: AsRef<Path>> ResolvePath for &T {
    fn resolve_with_default<P: AsRef<Path>, U: AsRef<Path>>(
        self,
        base_dir: P,
        default: U,
    ) -> PathBuf {
        if !self.as_ref().exists() {
            return base_dir.as_ref().join(default);
        }
        base_dir.as_ref().join(self)
    }

    fn resolve<P: AsRef<Path>>(self, base_dir: P) -> PathBuf {
        if !self.as_ref().exists() {
            return base_dir.as_ref().to_path_buf();
        }
        base_dir.as_ref().join(self)
    }
}

#[derive(Debug, Error)]
pub enum ConfigReadError {
    #[error("{0}")]
    Toml(#[from] toml::de::Error),
    #[error("{0}")]
    IO(#[from] std::io::Error),
}

pub fn load(root_dir: Option<PathBuf>, port: u16) -> Result<SiteConfig, ConfigReadError> {
    let root_dir = match root_dir {
        Some(v) => v.absolutize()?.to_path_buf(),
        None => std::env::current_dir()?,
    };

    let config_path = root_dir.join("config.toml");
    if !config_path.exists() {
        return Ok(SiteConfig::from_unresolved(
            root_dir,
            port,
            Default::default(),
        ));
    }

    let config_str = std::fs::read_to_string(&config_path)?;
    let unresolved_config = toml::from_str::<UnresolvedSiteConfig>(&config_str)?;
    Ok(SiteConfig::from_unresolved(
        root_dir,
        port,
        unresolved_config,
    ))
}
