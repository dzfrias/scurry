use clap::{Parser as ArgParser, ValueHint};
use std::path::PathBuf;

#[derive(Debug, ArgParser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[arg(conflicts_with_all = ["no_color", "vi_mode"], value_hint = ValueHint::FilePath)]
    pub file: Option<PathBuf>,

    #[arg(long)]
    pub no_color: bool,
    #[arg(long)]
    pub vi_mode: bool,
}
