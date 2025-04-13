//! template inlining (evaluating "extend .."s and render ".."s)

#![allow(dead_code)]

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::PageTemplate;
use crate::config::SiteConfig;
use crate::scripting::AstNode;

#[derive(Debug)]
pub struct ProcessedPage {
    pub extends: Option<PathBuf>,
    // if `ProcessedPage::extends` is `None`, this only has a single field, "".
    pub blocks: HashMap<String, Vec<AstNode>>,
    slots: Vec<String>,
    pub source: Arc<String>,
    pub path: PathBuf,
}

pub struct TemplateInliner {
    templates: HashMap<PathBuf, PageTemplate>,
    // if a template is not in todo and not in output, it means it's currently processing, meaning
    // a cyclic dependency was detected.
    todo: BTreeSet<PathBuf>,
    cfg: Arc<SiteConfig>,
    output: HashMap<PathBuf, ProcessedPage>,
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("While processing template")]
#[diagnostic()]
pub struct TemplateProcessingError {
    #[source_code]
    pub(super) src: Arc<String>,
    #[label = "in this section"]
    pub(super) at: SourceSpan,
    #[help]
    pub(super) help: String,
}

impl TemplateInliner {
    pub fn new(templates: HashMap<PathBuf, PageTemplate>, cfg: Arc<SiteConfig>) -> Self {
        Self {
            todo: templates.keys().cloned().collect(),
            templates,
            cfg,
            output: HashMap::new(),
        }
    }

    // Gets the finished templates
    //
    // # Panics
    //
    // Panics when there are still templates left to process
    pub fn into_inner(self) -> HashMap<PathBuf, ProcessedPage> {
        assert!(self.todo.is_empty());
        self.output
    }

    /// returns whether there is stuff left to process
    pub fn process(&mut self) -> Result<bool, TemplateProcessingError> {
        if self.todo.is_empty() {
            return Ok(false);
        }
        self.process_page(None)?;
        Ok(!self.todo.is_empty())
    }

    fn process_page(
        &mut self,
        parent_data: Option<(&Path, &Arc<String>, SourceSpan)>,
    ) -> Result<(), TemplateProcessingError> {
        let page = match parent_data {
            None => self.todo.pop_first().expect("process_page called without path while expecting there to be another unprocessed page, even tho there wasn't"),
            Some((page, parent_source, span)) => {
                if self.output.contains_key(page) {
                    return Ok(());
                }

                let Some(page) = self.todo.take(page) else {
                    return Err(TemplateProcessingError {
                        src: parent_source.clone(),
                        at: span,
                        help: "Cyclic dependency detected!".to_string(),
                    });
                };
                page
            }
        };

        let mut template = self.templates.remove(&page).expect("template missing");

        let extends_node = match template.ast.nodes.first() {
            Some(AstNode::Extend { template, offset }) => Some((*template, *offset)),
            _ => None,
        };
        if extends_node.is_some() {
            template.ast.nodes.remove(0);
        }
        let extends_path = if let Some((t_off, offset)) = extends_node {
            let path = self
                .cfg
                .templates_dir
                .join(&template.ast.source[t_off.range()]);
            self.process_page(Some((&path, &template.ast.source, offset.into())))?;
            path
        } else {
            let mut slots = Vec::new();
            self.process_block(&template.ast.nodes, &template.ast.source, &mut slots)?;
            self.output.insert(
                page.to_path_buf(),
                ProcessedPage {
                    extends: None,
                    blocks: HashMap::from([(String::new(), template.ast.nodes)]),
                    slots,
                    source: template.ast.source,
                    path: template.path,
                },
            );
            return Ok(());
        };

        let mut slots = Vec::new();
        let mut blocks = HashMap::new();
        for node in template.ast.nodes {
            match node {
                AstNode::Html(offset) if template.ast.source[offset.range()].trim().is_empty() => {}
                AstNode::Block { name, offset, body } => {
                    let name = &template.ast.source[name.range()];
                    if blocks.contains_key(name) {
                        return Err(TemplateProcessingError {
                            help: format!("Block `{name}` was already used"),
                            src: template.ast.source,
                            at: offset.into(),
                        });
                    }
                    if !self.output[&extends_path].slots.iter().any(|v| v == name) {
                        return Err(TemplateProcessingError {
                            at: offset.into(),
                            help: format!("Parent template has no slot for block `{name}`"),
                            src: template.ast.source,
                        });
                    }
                    self.process_block(&body, &template.ast.source, &mut slots)?;
                    blocks.insert(name.to_string(), body);
                }
                _ => {
                    return Err(TemplateProcessingError {
                        src: template.ast.source,
                        at: node.loc().into(),
                        help: "only block nodes are allowed in the top-level context of templates that extend another one".into(),
                    });
                }
            }
        }

        self.output.insert(
            page.to_path_buf(),
            ProcessedPage {
                extends: Some(extends_path),
                blocks,
                slots,
                source: template.ast.source,
                path: template.path,
            },
        );
        Ok(())
    }

    // returns a vector of slots this template has
    fn process_node(
        &mut self,
        node: &AstNode,
        source: &Arc<String>,
        slots: &mut Vec<String>,
    ) -> Result<(), TemplateProcessingError> {
        match node {
            AstNode::Html(_)
            | AstNode::Identifier(_)
            | AstNode::IntLiteral(..)
            | AstNode::FloatLiteral(..)
            | AstNode::BooleanLiteral(..)
            | AstNode::StringLiteral(_) => (),
            AstNode::MemberAccess { object: value, .. }
            | AstNode::Variable { value, .. }
            | AstNode::Minus { expr: value, .. }
            | AstNode::Not { expr: value, .. } => self.process_node(value, source, slots)?,
            AstNode::BinaryOp { left, right, .. } => {
                self.process_node(left, source, slots)?;
                self.process_node(right, source, slots)?;
            }
            AstNode::FunctionCall {
                function: list,
                args: body,
                ..
            }
            | AstNode::Render {
                component: list,
                body,
                ..
            }
            | AstNode::ForLoop { list, body, .. } => {
                self.process_node(list, source, slots)?;
                self.process_block(body, source, slots)?;
            }
            AstNode::Extend { offset, .. } => {
                return Err(TemplateProcessingError {
                    src: source.clone(),
                    at: (*offset).into(),
                    help: "extend is only allowed at the start of the file".into(),
                });
            }
            AstNode::Block { offset, .. } => {
                return Err(TemplateProcessingError {
                    src: source.clone(),
                    at: (*offset).into(),
                    help: "blocks are only allowed at the top-level".into(),
                });
            }
            AstNode::IfStatement {
                condition,
                truthy,
                falsy,
                ..
            } => {
                self.process_node(condition, source, slots)?;
                self.process_block(truthy, source, slots)?;
                self.process_block(falsy, source, slots)?;
            }
            AstNode::Slot {
                name: Some(name), ..
            } => {
                if !slots.iter().any(|v| v == &source[name.range()]) {
                    slots.push(source[name.range()].to_string());
                }
            }
            AstNode::Slot { name: None, .. } => (),
        }
        Ok(())
    }

    fn process_block(
        &mut self,
        blocks: &[AstNode],
        source: &Arc<String>,
        slots: &mut Vec<String>,
    ) -> Result<(), TemplateProcessingError> {
        for block in blocks {
            self.process_node(block, source, slots)?;
        }
        Ok(())
    }
}
