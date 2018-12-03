use std::collections::BTreeSet;
use std::io::BufReader;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let filename = "input-a.txt";

    let fd = File::open(filename)
        .expect(&format!("Failure opening {}", filename));

    let buf = BufReader::new(fd);
    let mut sum = 0;
    let mut nums = Vec::new();
    let lines = buf.lines();
    for line in lines {
        let num = line.unwrap().parse::<i32>().unwrap();
        nums.push(num);
        sum += num;
    }
    println!("Part 1: {}", sum);

    let mut done_part2 = false;
    let mut sums: BTreeSet<i32> = BTreeSet::new();
    sum = 0;
    sums.insert(sum);
    while !done_part2 {
        for num in nums.iter() {
            sum += num;
            if sums.contains(&sum) {
                println!("Part 2: {}", sum);
                done_part2 = true;
                break;
            }
            sums.insert(sum);
        }
    }
}
