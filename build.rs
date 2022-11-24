use clap::CommandFactory;
use clap_complete::generate_to;
use clap_complete::shells::*;
use clap_mangen::Man;
use std::env;
use std::io::Error;

include!("src/cli.rs");

fn main() -> Result<(), Error> {
    let outdir = match env::var_os("OUT_DIR") {
        None => return Ok(()),
        Some(outdir) => outdir,
    };

    let mut cmd = Args::command();

    // Generate completions
    generate_to(Zsh, &mut cmd, "scurry", &outdir)?;
    generate_to(Bash, &mut cmd, "scurry", &outdir)?;
    println!("cargo:warning=completion scripts generated to: {outdir:?}");

    // Generate man page
    let man = Man::new(cmd);
    let mut buffer: Vec<u8> = Default::default();
    man.render(&mut buffer)?;
    let out_path = PathBuf::from(outdir).join("scurry.1");
    std::fs::write(&out_path, buffer)?;
    println!("cargo:warning=manpage generated to: {out_path:?}",);

    Ok(())
}
