use anyhow::Result;
use bundle::Bundler;
use clap::Parser;
use full_moon::{parse, visitors::VisitorMut};
use pack::Packer;
use std::fs;
use std::path::PathBuf;

mod bundle;
mod pack;

#[derive(Parser)]
#[command(
    author = "The Unnamed Goose",
    version,
    about = "A basic rust application for efficiently bundling Lua scripts into monolithic releases."
)]
struct Args {
    #[arg(short, long)]
    banner: Option<PathBuf>,
    #[arg(short, long)]
    output: PathBuf,
    #[arg(short, long)]
    file: PathBuf,
    #[cfg(feature="lz4")]
    #[arg(short, long)]
    lz4: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut output: Vec<u8> = Vec::new();
    if let Some(banner) = args.banner {
        output.extend_from_slice(&fs::read(&banner)?);
    };

    let bundle = Bundler::bundle(args.file)?;
    let ast = parse(&bundle).map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
    output.extend_from_slice(&format!("{}", Packer::new().visit_ast(ast)).as_bytes());
    #[cfg(feature="lz4")]
    if args.lz4 {
      output.extend_from_slice(b"lz4decompress(\"");
      output.extend_from_slice(&lz4_flex::compress_prepend_size(&output));
      output.extend_from_slice(b"\")");
      return Ok(());
    }
    
    fs::write(&args.output, output)?;
    Ok(())
}
