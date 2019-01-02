use std::collections::HashMap;
use std::io;
use std::io::Read;
use std::process;

fn main() {
    let mut twos = 0;
    let mut threes = 0;
    let mut input = String::new();

    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read stdin.");

    for line in input.lines() {
        let mut data = HashMap::new();
        let chars: Vec<char> = line.chars().collect();
        for c in chars {
            let count = data.entry(c).or_insert(0);
            *count += 1;
        }

        let mut a: bool = false;
        let mut b: bool = false;
        for k in data.keys() {
            if data.get(k).unwrap_or(&0) == &2 {
                a = true;
            }
            if data.get(k).unwrap_or(&0) == &3 {
                b = true;
            }
        }
        if a {
            twos += 1;
        }
        if b {
            threes += 1;
        }
    }

    let checksum = twos * threes;
    println!("checksum = {}", checksum);
    process::exit(0)
}
