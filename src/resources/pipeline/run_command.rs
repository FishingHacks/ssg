use std::fmt::Write as _;
use std::path::Path;
use std::process::Stdio;

use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;

use super::{ContextPipelineError, PipelineCommand, PipelineError};
use crate::config::SiteConfig;

#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum RunCommandOutput {
    #[default]
    File,
    Stdout,
}

#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum RunCommandInput {
    #[default]
    File,
    Stdin,
}

// Available variables to be used:
// $infile - if input = "file"
// $outfile - if output = "file"
// $contentdir - content directory
// $templatedir - template directory
// $rootdir - root directory
// $distdir - build directory
// $outdir - build directory
// $$ - literal `$`

pub async fn run_commands_pipeline(
    config: &SiteConfig,
    pipeline: &[PipelineCommand],
    name: &str,
) -> Result<(), ContextPipelineError> {
    let vars = Vars {
        infile: None,
        outfile: None,
        contentdir: &config.content_dir,
        templatedir: &config.templates_dir,
        distdir: &config.out_dir,
        rootdir: &config.root_dir,
    };
    for (i, entry) in pipeline.iter().enumerate() {
        println!(
            "Running {name} pipeline (Step {}/{})",
            i + 1,
            pipeline.len()
        );
        if let Err(e) = run_cmd(entry, vars, &[]).await {
            return Err(ContextPipelineError::new(name, None, i, e));
        }
    }
    Ok(())
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RunCommand {
    new_extension: Option<String>,
    #[serde(default = "Default::default")]
    input: RunCommandInput,
    #[serde(default = "Default::default")]
    output: RunCommandOutput,
    #[serde(flatten)]
    command: PipelineCommand,
}

impl RunCommand {
    pub fn output_file(&self, mut input: std::path::PathBuf) -> std::path::PathBuf {
        if let Some(new_ext) = &self.new_extension {
            input.set_extension(new_ext);
        }
        input
    }

    pub async fn run(
        &self,
        input: &Path,
        output: &Path,
        config: &crate::config::SiteConfig,
    ) -> Result<(), PipelineError> {
        let vars = Vars {
            infile: Some(input),
            outfile: Some(output),
            contentdir: &config.content_dir,
            templatedir: &config.templates_dir,
            distdir: &config.out_dir,
            rootdir: &config.root_dir,
        };

        let input = if let RunCommandInput::Stdin = self.input {
            tokio::fs::read(input).await?
        } else {
            const { Vec::new() }
        };
        let stdout = run_cmd(&self.command, vars, &input).await?;
        if let RunCommandOutput::Stdout = self.output {
            tokio::fs::write(output, stdout).await?
        }
        Ok(())
    }
}

async fn run_cmd(
    command: &PipelineCommand,
    vars: Vars<'_>,
    input: &[u8],
) -> Result<Vec<u8>, PipelineError> {
    let mut cmd = Command::new(&command.cmd);
    cmd.args(command.arguments.iter().map(move |v| replace_vars(v, vars)));
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    let mut child = cmd.spawn()?;
    if !input.is_empty() {
        if let Some(stdin) = child.stdin.as_mut() {
            stdin.write_all(input).await?;
        }
    }
    let output = child.wait_with_output().await?;
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
    rootdir: &'a Path,
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
                is_var = false;
                let default = &input[start_idx..current_idx];
                let value = match &input[start_idx + '$'.len_utf8()..current_idx] {
                    "infile" => vars.infile,
                    "outfile" => vars.outfile,
                    "contentdir" => Some(vars.contentdir),
                    "templatedir" => Some(vars.templatedir),
                    "distdir" => Some(vars.distdir),
                    "outdir" => Some(vars.distdir),
                    "rootdir" => Some(vars.rootdir),
                    _ => None,
                };
                let Some(v) = value else {
                    output.push_str(default);
                    continue;
                };
                output
                    .write_fmt(format_args!("{}", v.display()))
                    .expect("writing to a string should never fail");
                continue;
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
