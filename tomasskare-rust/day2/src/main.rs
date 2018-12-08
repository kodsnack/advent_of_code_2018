use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn part1() {
    let filename = "input.txt";

    let fd = File::open(filename)
        .expect(&format!("Failure opening {}", filename));

    let buf = BufReader::new(fd);
    let mut num_two = 0;
    let mut num_three = 0;
    for line in buf.lines() {
        let s = line.unwrap();
        let mut cnts: HashMap<char, i32> = HashMap::new();
        for c in s.chars() {
            let cnt = cnts.entry(c).or_insert(0);
            *cnt += 1;
        }
        let mut have_two = false;
        let mut have_three = false;
        for key in cnts.keys() {
            if !have_two && cnts.get(key) == Some(&2) {
                num_two += 1;
                have_two = true;
            } else if !have_three && cnts.get(key) == Some(&3) {
                num_three += 1;
                have_three = true;
            }
        }
    }
    let sum = num_two * num_three;
    println!("Part 1: {}", sum);
}

fn part2() {
    let filename = "input.txt";

    let fd = File::open(filename)
        .expect(&format!("Failure opening {}", filename));

    let buf = BufReader::new(fd);
    let mut v: Vec<Vec<char>> = Vec::new();
    for line in buf.lines() {
        v.push(line.unwrap().chars().collect());
    }

    for i in 0..v.len() {
        let vi = &v[i];
        for j in i+1..v.len() {
            let vj = &v[j];
            if vi.len() != vj.len() {
                continue;
            }

            let mut diff = 0;
            let mut diffpos = 0;
            for x in 0..vi.len() {
                if vi[x] != vj[x] {
                    diff += 1;
                    diffpos = x;
                }
            }
            if diff == 1 {
                let common1: String = vi.iter().skip(0).take(diffpos).collect();
                let common2: String = vi.iter().skip(diffpos + 1).take(vi.len() - diffpos).collect();
                let common = format!("{}{}", &common1, &common2);
                println!("Part 2: {}", common);
            }
        }
    }
}

fn main() {
    part1();
    part2();
}
