use clap::{Parser as ArgParser, ValueHint};
use std::path::PathBuf;

#[derive(Debug, ArgParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(conflicts_with_all = ["no_color", "vi_mode"], value_hint = ValueHint::FilePath)]
    file: Option<PathBuf>,

    #[arg(long)]
    no_color: bool,
    #[arg(long)]
    vi_mode: bool,
}
