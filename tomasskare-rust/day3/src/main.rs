extern crate regex;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use regex::Regex;

fn part1() {
    let filename = "input.txt";

    let fd = File::open(filename)
        .expect(&format!("Failure opening {}", filename));

    // #1 @ 1,3: 4x4
    let re = Regex::new(r"#(?P<id>\d+) +@ +(?P<x>\d+),(?P<y>\d+): +(?P<w>\d+)x(?P<h>\d+)").unwrap();

    // 1000x1000 fabric
    let mut state = [[0u8; 1000]; 1000];
    let buf = BufReader::new(fd);
    for line in buf.lines() {
        let claim = line.unwrap();
        let caps = re.captures(&claim).unwrap();
        let x1: u32 = caps["x"].parse::<u32>().unwrap();
        let y1: u32 = caps["y"].parse::<u32>().unwrap();
        let x2: u32 = x1 + caps["w"].parse::<u32>().unwrap();
        let y2: u32 = y1 + caps["h"].parse::<u32>().unwrap();

        for y in y1..y2 {
            for x in x1..x2 {
                state[x as usize][y as usize] += 1;
            }
        }
    }

    let mut cnt = 0;
    for y in 0..1000 {
        for x in 0..1000 {
            if state[x as usize][y as usize] > 1 {
                cnt += 1;
            }
        }
    }

    println!("Part 1: {}", cnt);
}

fn part2() {
    let filename = "input.txt";

    let fd = File::open(filename)
        .expect(&format!("Failure opening {}", filename));

    // #1 @ 1,3: 4x4
    let re = Regex::new(r"#(?P<id>\d+) +@ +(?P<x>\d+),(?P<y>\d+): +(?P<w>\d+)x(?P<h>\d+)").unwrap();

    // 1000x1000 fabric
    let mut state = [[0u32; 1000]; 1000];
    let mut valid_claims: Vec<u32> = Vec::new();
    let buf = BufReader::new(fd);
    for line in buf.lines() {
        let claim = line.unwrap();
        let caps = re.captures(&claim).unwrap();
        let id: u32 = caps["id"].parse::<u32>().unwrap();
        let x1: u32 = caps["x"].parse::<u32>().unwrap();
        let y1: u32 = caps["y"].parse::<u32>().unwrap();
        let x2: u32 = x1 + caps["w"].parse::<u32>().unwrap();
        let y2: u32 = y1 + caps["h"].parse::<u32>().unwrap();

        valid_claims.push(id);

        for y in y1..y2 {
            for x in x1..x2 {
                let curr = state[x as usize][y as usize];
                if curr != 0 {
                    let index = valid_claims.iter().position(|x| *x == curr);
                    if index != None {
                        valid_claims.remove(index.unwrap());
                    }

                    let index = valid_claims.iter().position(|x| *x == id);
                    if index != None {
                        valid_claims.remove(index.unwrap());
                    }
                }
                state[x as usize][y as usize] = id;
            }
        }
    }

    println!("Part 1: {}", valid_claims[0]);
}

fn main() {
    part1();
    part2();
}
