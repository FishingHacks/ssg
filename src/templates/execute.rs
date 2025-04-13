use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::inlining::ProcessedPage;
use crate::config::SiteConfig;
use crate::content::{ContentMeta, MetaValue};
use crate::scripting::{AstNode, Operator};

pub struct Executor<'a> {
    templates: &'a HashMap<PathBuf, ProcessedPage>,
    scopes: Scopes,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Char(char),
    Bool(bool),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Undefined,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(v) => Display::fmt(v, f),
            Value::Number(v) => Display::fmt(v, f),
            Value::Char(c) => Display::fmt(c, f),
            Value::Bool(v) => Display::fmt(v, f),
            Value::List(values) => f
                .debug_list()
                .entries(values.iter().map(DebugDisplay))
                .finish(),
            Value::Map(map) => f
                .debug_map()
                .entries(map.iter().map(|(k, v)| (k, DebugDisplay(v))))
                .finish(),
            Value::Undefined => f.write_str("undefined"),
        }
    }
}

macro_rules! from_value {
    ($v:ident($ty:ty)) => {
        impl From<$ty> for Value {
            fn from(val: $ty) -> Self {
                Self::$v(val)
            }
        }
    };
}

from_value!(String(String));
from_value!(Number(f64));
from_value!(Char(char));
from_value!(Bool(bool));
from_value!(List(Vec<Value>));
from_value!(Map(HashMap<String, Value>));

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Undefined
    }
}

impl From<MetaValue> for Value {
    fn from(val: MetaValue) -> Self {
        match val {
            MetaValue::String(s) => Value::String(s),
            MetaValue::Vec(items) => Value::List(items.into_iter().map(Value::String).collect()),
            MetaValue::Bool(v) => Value::Bool(v),
            MetaValue::Number(v) => Value::Number(v as f64),
            MetaValue::Float(v) => Value::Number(v as f64),
            MetaValue::HashMap(map) => Value::Map(
                map.into_iter()
                    .map(|(k, v)| (k, Value::String(v)))
                    .collect(),
            ),
        }
    }
}

struct Scopes {
    inner: Vec<HashMap<String, Value>>,
}

impl Scopes {
    pub fn get<Q>(&self, k: &Q) -> Option<&Value>
    where
        Q: ?Sized + Hash + Eq,
        String: Borrow<Q>,
    {
        for v in self.inner.iter().rev() {
            if let Some(v) = v.get(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn insert(&mut self, k: String, v: Value) {
        self.inner
            .last_mut()
            .expect("there should be at least one scope")
            .insert(k, v);
    }

    pub fn push_scope(&mut self) {
        self.inner.push(Default::default());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.inner.len() > 1);
        self.inner.pop();
    }

    pub fn new() -> Self {
        Self {
            inner: vec![HashMap::new()],
        }
    }
}

macro_rules! value {
    ($ty:ident) => {
        Cow::Owned(Value::$ty)
    };
    ($ty:ident($($value:expr),* $(,)?)) => {
        Cow::Owned(Value::$ty($($value),*))
    };
    () => {
        Cow::Owned(Value::Undefined)
    };
}

impl<'a> Executor<'a> {
    pub fn new(templates: &'a HashMap<PathBuf, ProcessedPage>) -> Self {
        Self {
            templates,
            scopes: Scopes::new(),
        }
    }

    pub fn add_frontmatter(&mut self, front_matter: ContentMeta) -> PathBuf {
        let mut metadata_map = HashMap::new();
        metadata_map.insert("title".into(), Value::String(front_matter.title));
        metadata_map.insert("draft".into(), Value::Bool(front_matter.draft));
        metadata_map.insert("date".into(), Value::String(front_matter.date));
        metadata_map.insert("tags".into(), MetaValue::Vec(front_matter.tags).into());
        metadata_map.insert(
            "description".into(),
            Value::String(front_matter.description),
        );
        for (k, v) in front_matter.custom {
            metadata_map.insert(k, v.into());
        }

        self.scopes.insert("page".into(), Value::Map(metadata_map));
        front_matter.template
    }

    pub fn insert_into_scope(&mut self, s: impl Into<String>, v: impl Into<Value>) {
        self.scopes.insert(s.into(), v.into());
    }

    pub fn clear_scope(&mut self) {
        self.scopes = Scopes::new();
    }

    pub fn evaluate_statement(
        &mut self,
        node: &AstNode,
        source: &str,
        slots: &HashMap<String, String>,
        output: &mut String,
    ) {
        match node {
            AstNode::Html(offset) => output.push_str(&source[offset.range()]),
            AstNode::Variable { ident, value } => {
                let value = self.evaluate_expr(value, source).into_owned();
                self.scopes.insert(source[ident.range()].to_string(), value);
            }
            AstNode::ForLoop {
                identifier,
                index: index_offset,
                list,
                body,
                ..
            } => {
                for index in 0.. {
                    let list = self.evaluate_expr(list, source);
                    let Some(element) = (match list.as_ref() {
                        Value::String(s) => s.chars().nth(index).map(Value::Char),
                        Value::Number(v) => (*v < index as f64).then_some(Value::Number(*v)),
                        Value::List(values) => values.get(index).cloned(),
                        Value::Bool(_) | Value::Map(_) | Value::Undefined | Value::Char(_) => None,
                    }) else {
                        break;
                    };
                    self.scopes.push_scope();
                    self.scopes
                        .insert(source[identifier.range()].to_string(), element);
                    if let Some(index_offset) = index_offset {
                        self.scopes.insert(
                            source[index_offset.range()].to_string(),
                            Value::Number(index as f64),
                        );
                    }
                    for stmt in body {
                        self.evaluate_statement(stmt, source, slots, output);
                    }
                    self.scopes.pop_scope();
                }
            }
            AstNode::IfStatement {
                condition,
                truthy,
                falsy,
                ..
            } => {
                let cond = self.evaluate_expr(condition, source);
                let is_truthy = match cond.as_ref() {
                    Value::String(s) => !s.is_empty(),
                    Value::Number(v) => *v != 0.0,
                    Value::Bool(b) => *b,
                    Value::Char(_) | Value::List(_) | Value::Map(_) => true,
                    Value::Undefined => false,
                };
                self.scopes.push_scope();
                if is_truthy {
                    for stmt in truthy {
                        self.evaluate_statement(stmt, source, slots, output);
                    }
                } else {
                    for stmt in falsy {
                        self.evaluate_statement(stmt, source, slots, output);
                    }
                }
                self.scopes.pop_scope();
            }
            AstNode::Slot { name, .. } => {
                if let Some(v) = slots.get(name.map(|name| &source[name.range()]).unwrap_or("")) {
                    output.push_str(v);
                }
            }
            AstNode::BinaryOp { .. }
            | AstNode::Identifier(..)
            | AstNode::IntLiteral(..)
            | AstNode::FloatLiteral(..)
            | AstNode::BooleanLiteral(..)
            | AstNode::StringLiteral(..)
            | AstNode::Not { .. }
            | AstNode::Minus { .. }
            | AstNode::MemberAccess { .. }
            | AstNode::FunctionCall { .. } => {
                write!(output, "{}", self.evaluate_expr(node, source))
                    .expect("writing to string should never fail");
            }
            AstNode::Render { .. } => todo!("render"),
            AstNode::Block { .. } => unreachable!("blocks should have been removed"),
            AstNode::Extend { .. } => unreachable!("extends should have been removed"),
        }
    }

    fn evaluate_expr(&self, node: &AstNode, source: &str) -> Cow<'_, Value> {
        match node {
            AstNode::Identifier(byte_offset) => {
                match self.scopes.get(&source[byte_offset.range()]) {
                    Some(v) => Cow::Borrowed(v),
                    None => value!(),
                }
            }
            AstNode::IntLiteral(_, v) => value!(Number(*v as f64)),
            AstNode::FloatLiteral(_, v) => value!(Number(*v)),
            AstNode::BooleanLiteral(_, v) => value!(Bool(*v)),
            AstNode::StringLiteral(byte_offset) => {
                value!(String(source[byte_offset.range()].to_string()))
            }
            AstNode::BinaryOp {
                left,
                right,
                operator,
                ..
            } => {
                let left = self.evaluate_expr(left, source);
                let right = self.evaluate_expr(right, source);
                match operator {
                    Operator::Equal => value!(Bool(left == right)),
                    Operator::NotEqual => value!(Bool(left != right)),
                    Operator::GreaterEqual => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Bool(*l >= *r)),
                        _ => value!(),
                    },
                    Operator::Greater => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Bool(*l > *r)),
                        _ => value!(),
                    },
                    Operator::LesserEqual => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Bool(*l <= *r)),
                        _ => value!(),
                    },
                    Operator::Lesser => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Bool(*l < *r)),
                        _ => value!(),
                    },
                    Operator::Either => matches!(left.as_ref(), Value::Undefined)
                        .then_some(right)
                        .unwrap_or(left),
                    Operator::And => match (left.as_ref(), right.as_ref()) {
                        (Value::Bool(l), Value::Bool(r)) => value!(Bool(*l && *r)),
                        _ => value!(),
                    },
                    Operator::Or => match (left.as_ref(), right.as_ref()) {
                        (Value::Bool(l), Value::Bool(r)) => value!(Bool(*l || *r)),
                        _ => value!(),
                    },
                    Operator::Minus => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Number(*l - *r)),
                        _ => value!(),
                    },
                    Operator::Mul => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Number(*l * *r)),
                        _ => value!(),
                    },
                    Operator::Div => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Number(*l / *r)),
                        _ => value!(),
                    },
                    Operator::Modulo => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Number((*l % *r + *r) % *r)),
                        _ => value!(),
                    },
                    Operator::Plus => match (left.as_ref(), right.as_ref()) {
                        (Value::Number(l), Value::Number(r)) => value!(Number(*l + *r)),
                        (Value::Number(l), Value::String(r)) => value!(String(format!("{l}{r}"))),
                        (Value::String(l), Value::Number(r)) => value!(String(format!("{l}{r}"))),
                        (Value::String(l), Value::String(r)) => value!(String(format!("{l}{r}"))),
                        (Value::Char(l), Value::String(r)) => value!(String(format!("{l}{r}"))),
                        (Value::String(l), Value::Char(r)) => value!(String(format!("{l}{r}"))),
                        (Value::Char(l), Value::Number(r)) => value!(String(format!("{l}{r}"))),
                        (Value::Number(l), Value::Char(r)) => value!(String(format!("{l}{r}"))),
                        (Value::Char(l), Value::Char(r)) => value!(String(format!("{l}{r}"))),
                        _ => value!(),
                    },
                    _ => unreachable!("not a binary op"),
                }
            }
            AstNode::MemberAccess {
                object, property, ..
            } => {
                let object = self.evaluate_expr(object, source);
                match object {
                    Cow::Borrowed(Value::Map(map)) => map
                        .get(&source[property.range()])
                        .map(Cow::Borrowed)
                        .unwrap_or(Cow::Owned(Value::Undefined)),
                    Cow::Owned(Value::Map(mut map)) => map
                        .remove(&source[property.range()])
                        .map(Cow::Owned)
                        .unwrap_or(Cow::Owned(Value::Undefined)),
                    _ => Cow::Owned(Value::Undefined),
                }
            }
            AstNode::FunctionCall { .. } => todo!("function calls"),
            AstNode::Not { expr, .. } => match self.evaluate_expr(expr, source).as_ref() {
                Value::Bool(v) => Cow::Owned(Value::Bool(!*v)),
                _ => Cow::Owned(Value::Undefined),
            },
            AstNode::Minus { expr, .. } => match self.evaluate_expr(expr, source).as_ref() {
                Value::Number(v) => Cow::Owned(Value::Number(-*v)),
                _ => Cow::Owned(Value::Undefined),
            },
            _ => unreachable!("not an expression"),
        }
    }

    pub fn evaluate_template(&mut self, template: &Path, slots: HashMap<String, String>) -> String {
        // remove all scopes but the top one (holds metadata)
        while self.scopes.inner.len() > 1 {
            self.scopes.inner.pop();
        }
        // add a scope to do the templating in
        self.scopes.push_scope();
        let template = &self.templates[template];
        let Some(extends) = template.extends.as_ref() else {
            let mut output = String::new();
            // block for a root template
            let content = &template.blocks[""];
            for node in content {
                self.evaluate_statement(node, &template.source, &slots, &mut output);
            }
            return output;
        };
        let mut blocks = HashMap::new();

        for (block_name, block_content) in template.blocks.iter() {
            let mut output = String::new();
            for node in block_content {
                self.evaluate_statement(node, &template.source, &slots, &mut output);
            }
            blocks.insert(block_name.clone(), output);
        }

        self.evaluate_template(extends, blocks)
    }

    pub fn render_content(&mut self, meta: ContentMeta, html: String) -> String {
        self.clear_scope();
        let template = self.add_frontmatter(meta);
        self.evaluate_template(&template, HashMap::from([("".to_string(), html)]))
    }
}

struct DebugDisplay<'a, T: Display>(&'a T);
impl<T: Display> Debug for DebugDisplay<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}
