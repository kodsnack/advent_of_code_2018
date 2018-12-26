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
        if has_two(line) {
            twos = twos +1;
        }

        if has_three(line) {
            threes = threes +1;
        }
    }
    
    let checksum = twos * threes;

    println!("checksum = {}", checksum);
    process::exit(0)
}

fn has_two(s: &str) -> bool {
    let sorted = sort(s);
    true
}

fn has_three(s: &str) -> bool {
    let sorted = sort(s);
    false
}

fn sort(s: &str) -> String {
    let mut temp = s.chars().collect::<Vec<char>>();
    temp.sort();
    temp.into_iter().collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_two() {
        assert_eq!(has_two("abbcde"), true);
    }

    #[test]
    fn test_has_three() {
        assert_eq!(has_three("abcccd"), true);
    }

    #[test]
    fn test_doesnt_have_two() {
        assert_eq!(has_three("abcdef"), false);
    }

    #[test]
    fn test_doesnt_have_three() {
        assert_eq!(has_three("abcdef"), false);
    }

    #[test]
    fn test_sort() {
        assert_eq!(sort("example"), "aeelmpx");
    }
}

/*
abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
*/

