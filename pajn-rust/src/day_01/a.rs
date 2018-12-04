use std::fs::File;
use std::io::prelude::*;

pub fn run() {
    let mut f = File::open("src/day_01/a.input").expect("File not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("Something went wrong reading the file");

    let val: i32 = contents
      .split("\n")
      .filter(|row| !row.is_empty())
      .map(|row| row.parse::<i32>().expect("Value not number?"))
      .sum();

    println!("Value: {}", val);
}