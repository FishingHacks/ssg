use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct SiteConfig {
    resources_dirs: Vec<PathBuf>,
    templates_dir: PathBuf,
    content_dir: PathBuf,
    out_dir: PathBuf,
    index_page: String,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct UnresolvedSiteConfig {
    resources_dirs: Option<Vec<PathBuf>>,
    templates_dir: Option<PathBuf>,
    content_dir: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    index_page: Option<String>,
}

impl SiteConfig {
    pub fn from_unresolved(root_dir: &Path, unresolved: UnresolvedSiteConfig) -> Self {
        let resources_dirs = match unresolved.resources_dirs {
            Some(dirs) => dirs.into_iter().map(|v| root_dir.join(v)).collect(),
            None => vec![root_dir.join("resources")],
        };
        let templates_dir = resolve_path(root_dir, unresolved.templates_dir, Path::new("templates"));
        let content_dir = resolve_path(root_dir, unresolved.content_dir, Path::new("content"));
        let out_dir = resolve_path(root_dir, unresolved.out_dir, Path::new("dist"));
        let index_page = unresolved.index_page.unwrap_or_else(|| "_index.md".into());
        Self {
            resources_dirs,
            templates_dir,
            content_dir,
            out_dir,
            index_page,
        }
    }
}

fn resolve_path(root_dir: &Path, path: Option<PathBuf>, default: &'static Path) -> PathBuf {
    let Some(path) = path else { return root_dir.join(default) };
    // this sucks, but i would like to have absolute paths
    root_dir.join(path)
}
