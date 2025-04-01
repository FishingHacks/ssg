use std::path::PathBuf;

#[derive(Debug, Default)]
pub enum ContentKind {
    #[default]
    Markdown,
}

#[derive(Debug)]
pub struct SiteConfig {
    resources_dir: PathBuf,
    templates_dir: PathBuf,
    content_dir: PathBuf,
}
