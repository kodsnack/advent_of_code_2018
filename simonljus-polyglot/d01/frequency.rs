use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;

fn solve_frequency(instructions: Vec<String>) ->bool{
    let mut seen: HashSet<i32> = HashSet::new();
    let mut _sum: i32 =0;
    let mut iteration: i32 =0;
    let mut p2_solved = false;
    let mut p1_solved = false;
    loop{
        for mut frequency in instructions.iter(){
            if frequency.len() >0{
                let _val = frequency.parse::<i32>().unwrap();
                _sum+=_val;
                if seen.insert(_sum)== false && p2_solved ==false{
                    println!("Problem 2 solved: Duplicated frequency: {} at iteration {}\n",_sum,iteration);
                    p2_solved = true;
                    if p1_solved{
                        return true;
                    }
                }
            }
        }
        if iteration ==0 {
            println!("Problem 1 solved: The sum of frequencies: {}\n",_sum);
            p1_solved = true;
            if p2_solved{
                return true;
            }
        }
        iteration+=1;
    }
}
fn main() {
    let filename = "input.txt";
    let mut f = File::open(filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");
    let instructions: Vec<String> = contents.split("\n").map(|s| s.to_string()).collect();
    solve_frequency(instructions);
}