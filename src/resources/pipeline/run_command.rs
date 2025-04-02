use std::{
    fmt::Write as _,
    io::{Read, Write as _},
    path::Path,
    process::{Command, Stdio},
};

use serde::{Deserialize, Serialize};
use tempfile::NamedTempFile;

use super::{PipelineError, ResourcePipeline};

#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum RunCommandOutput {
    File,
    #[default]
    Stdout,
}

#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum RunCommandInput {
    File,
    #[default]
    Stdin,
}

/// Available variables to be used:
/// $infile - if input = "file"
/// $outfile - if output = "file"
/// $contentdir - content directory
/// $templatedir - template directory
/// $distdir - build directory
/// $outdir - build directory
/// $$ - literal `$`

#[derive(Debug, Serialize, Deserialize)]
pub struct RunCommand {
    new_extension: Option<String>,
    #[serde(default = "Default::default")]
    input: RunCommandInput,
    #[serde(default = "Default::default")]
    output: RunCommandOutput,
    command: String,
    arguments: Vec<String>,
}

impl ResourcePipeline for RunCommand {
    fn output(&self, mut input: std::path::PathBuf) -> std::path::PathBuf {
        if let Some(new_ext) = &self.new_extension {
            input.set_extension(new_ext);
        }
        input
    }

    fn run(&self, input: Vec<u8>, config: &crate::config::SiteConfig) -> Result<Vec<u8>, PipelineError> {
        let mut infile = if let RunCommandInput::File = self.input { Some(NamedTempFile::new()?) } else { None };
        let outfile = if let RunCommandOutput::File = self.output { Some(NamedTempFile::new()?) } else { None };
        if let Some(infile) = &mut infile {
            infile.write_all(&input)?;
        }
        let vars = Vars {
            infile: infile.as_ref().map(NamedTempFile::path),
            outfile: outfile.as_ref().map(NamedTempFile::path),
            contentdir: &config.content_dir,
            templatedir: &config.templates_dir,
            distdir: &config.out_dir,
        };
        let mut cmd = Command::new(&self.command);
        cmd.args(self.arguments.iter().map(|v| replace_vars(v, vars)));
        cmd.stdin(Stdio::piped());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        let mut output = run_cmd(cmd, if let RunCommandInput::Stdin = self.input { &input } else { &[] })?;
        if let Some(mut outfile) = outfile {
            output.clear();
            outfile.read_to_end(&mut output)?;
        }
        Ok(output)
    }
}

fn run_cmd(mut cmd: Command, input: &[u8]) -> Result<Vec<u8>, PipelineError> {
    let mut child = cmd.spawn()?;
    if let Some(stdin) = child.stdin.as_mut() {
        stdin.write_all(input)?;
    }
    let output = child.wait_with_output()?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        let mut out = output.stdout;
        out.extend(output.stderr);
        Err(PipelineError::Command(output.status, out))
    }
}

#[derive(Clone, Copy)]
struct Vars<'a> {
    infile: Option<&'a Path>,
    outfile: Option<&'a Path>,
    contentdir: &'a Path,
    templatedir: &'a Path,
    distdir: &'a Path,
}

fn replace_vars(input: &str, vars: Vars) -> String {
    let mut output = String::with_capacity(input.len());
    let mut start_idx = 0;
    let mut current_idx = 0;
    let mut is_var = false;

    while current_idx < input.len() {
        let Some(c) = input[current_idx..].chars().next() else { break };
        if is_var {
            if !c.is_ascii_alphabetic() {
                let default = &input[start_idx..current_idx];
                let value = match &input[start_idx + '$'.len_utf8()..current_idx] {
                    "infile" => vars.infile,
                    "outfile" => vars.outfile,
                    "contentdir" => Some(vars.contentdir),
                    "templatedir" => Some(vars.templatedir),
                    "distdir" => Some(vars.distdir),
                    "outdir" => Some(vars.distdir),
                    _ => None,
                };
                let Some(v) = value else {
                    output.push_str(default);
                    continue;
                };
                output
                    .write_fmt(format_args!("{}", v.display()))
                    .expect("writing to a string should never fail");
            }
        } else if c == '$' {
            if let Some('$') = input[current_idx + c.len_utf8()..].chars().next() {
                current_idx += '$'.len_utf8();
                output.push('$');
            } else {
                is_var = true;
                start_idx = current_idx;
            }
        } else {
            output.push(c);
        }
        current_idx += c.len_utf8();
    }
    if is_var {
        let default = &input[start_idx..];
        let value = match &input[start_idx + '$'.len_utf8()..current_idx] {
            "infile" => vars.infile,
            "outfile" => vars.outfile,
            "contentdir" => Some(vars.contentdir),
            "templatedir" => Some(vars.templatedir),
            "distdir" => Some(vars.distdir),
            "outdir" => Some(vars.distdir),
            _ => None,
        };
        let Some(v) = value else {
            output.push_str(default);
            return output;
        };
        output
            .write_fmt(format_args!("{}", v.display()))
            .expect("writing to a string should never fail");
    }
    output
}
