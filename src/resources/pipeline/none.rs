use serde::{Deserialize, Serialize};

use super::ResourcePipeline;

#[derive(Debug, Serialize, Deserialize)]
pub struct None;

impl ResourcePipeline for None {
    fn output(&self, input: std::path::PathBuf) -> std::path::PathBuf {
        input
    }

    fn run(&self, input: Vec<u8>, _: &crate::config::SiteConfig) -> Result<Vec<u8>, super::PipelineError> {
        Ok(input)
    }
}
