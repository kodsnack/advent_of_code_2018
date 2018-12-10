use std::process;
use std::io::Read;
use std::io;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).expect("Failed to read stdin.");

    let mut result = 0;
    
    for line in input.lines() {
        let change: i32 = line.parse().expect("Couldn't cast line to i32"); 
        result += change;
    }
    println!("Result = {}", result);
    process::exit(0)
}
