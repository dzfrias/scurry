use clap::CommandFactory;
use clap_complete::generate_to;
use clap_complete::shells::*;
use std::env;
use std::io::Error;

include!("src/cli.rs");

fn main() -> Result<(), Error> {
    let outdir = match env::var_os("OUT_DIR") {
        None => return Ok(()),
        Some(outdir) => outdir,
    };

    let mut cmd = Args::command();
    let path = generate_to(Zsh, &mut cmd, "scurry", &outdir)?;
    generate_to(Bash, &mut cmd, "scurry", &outdir)?;

    println!("cargo:warning=completion file is generated: {:?}", path);

    Ok(())
}
