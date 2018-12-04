extern crate regex;

use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use regex::Regex;

fn main() -> io::Result<()> {
    let pattern = Regex::new(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)").unwrap();

    let f = File::open("day03.txt")?;
    let mut reader = BufReader::new(f);
    let mut ln = String::new();

    reader.read_line(&mut ln)?;
    let caps = pattern.captures(&ln).unwrap();

    println!("{}", caps.get(1).unwrap().as_str());
    Ok(())
}