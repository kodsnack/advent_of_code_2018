use std::collections::BTreeMap;
use std::fs::File;
use std::io::prelude::*;

fn check_row(row: &str) -> (bool, bool) {
  let mut heatmap = BTreeMap::new();
  
  for c in row.chars() {
    heatmap.entry(c)
      .and_modify(|count| { *count += 1 })
      .or_insert(1);
  }

  let has_twins = heatmap.values().any(|count| *count == 2);
  let has_triplets = heatmap.values().any(|count| *count == 3);

  (has_twins, has_triplets)
}

fn bool_to_int(val: bool) -> i32 {
  match val {
    true => 1,
    false => 0
  }
}

pub fn run() {
    let mut f = File::open("src/day_02/a.input").expect("File not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("Something went wrong reading the file");

    let (num_twins, num_triplets) = contents
      .split("\n")
      .filter(|row| !row.is_empty())
      .fold((0, 0), |(num_twins, num_triplets), row| {
        let (has_twins, has_triplets) = check_row(row);
        let twin_diff = bool_to_int(has_twins);
        let triplet_diff = bool_to_int(has_triplets);

        (num_twins + twin_diff, num_triplets + triplet_diff)
      });

    println!("Value: {}", num_twins * num_triplets);
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_has_twins_and_triplets() {
    let (has_twins, has_triplets) = check_row("aabbb");

    assert!(has_twins);
    assert!(has_triplets);
  }

  #[test]
  fn test_has_no_twins_or_triplets() {
    let (has_twins, has_triplets) = check_row("abcdef");

    assert!(!has_twins);
    assert!(!has_triplets);
  }

  #[test]
  fn test_has_only_twins() {
    let (has_twins, has_triplets) = check_row("abcdde");

    assert!(has_twins);
    assert!(!has_triplets);
  }

  #[test]
  fn test_has_only_twins2() {
    let (has_twins, has_triplets) = check_row("abcddeeee");

    assert!(has_twins);
    assert!(!has_triplets);
  }

  #[test]
  fn test_has_only_triplets() {
    let (has_twins, has_triplets) = check_row("adcdde");

    assert!(!has_twins);
    assert!(has_triplets);
  }
}