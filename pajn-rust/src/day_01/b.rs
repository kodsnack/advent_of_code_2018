use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

pub fn run() {
    let mut f = File::open("src/day_01/a.input").expect("File not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("Something went wrong reading the file");

    let val = contents
      .split("\n")
      .filter(|row| !row.is_empty())
      .cycle()
      .map(|row| row.parse::<i32>().expect("Value not number?"))
      .try_fold((0, HashSet::new()), |(freq, mut visited_values), diff| {
        let new_freq = freq + diff;

        if visited_values.contains(&new_freq) {
          return Err(new_freq)
        }

        visited_values.insert(new_freq);
        
        Ok((new_freq, visited_values))
      })
      .unwrap_err();

    println!("Visited Twice: {}", val);
}