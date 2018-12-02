use std::fs::File;
use std::io::prelude::*;

fn check_row(row1: &str, row2: &str) -> i32 {
  row1.chars().zip(row2.chars()).fold(0, |changes, (a, b)| {
    match a == b {
      true => changes,
      false => changes + 1
    }
  })
}

pub fn run() {
    let mut f = File::open("src/day_02/a.input").expect("File not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("Something went wrong reading the file");

    let rows: Vec<&str> = contents
      .split("\n")
      .filter(|row| !row.is_empty())
      .collect();

    for row1 in &rows {
      for row2 in &rows {
        if check_row(row1, row2) == 1 {
          println!("Row1: {}, Row2: {}", row1, row2);

          let common: String = row1.chars().zip(row2.chars())
            .filter(|(a, b)| a == b)
            .map(|(a, _)| a)
            .collect();

          println!("Common chars: {}", common);
        }
      }
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_no_changes() {
    let changes = check_row("abcdef", "abcdef");

    assert_eq!(changes, 0);
  }

  #[test]
  fn test_one_change() {
    let changes1 = check_row("abcdef", "abcxef");
    let changes2 = check_row("adcdef", "abcdef");

    assert_eq!(changes1, 1);
    assert_eq!(changes2, 1);
  }

  #[test]
  fn test_two_changes() {
    let changes1 = check_row("adcdef", "abcxef");
    let changes2 = check_row("adddef", "abcdef");

    assert_eq!(changes1, 2);
    assert_eq!(changes2, 2);
  }

  #[test]
  fn test_three_changes() {
    let changes1 = check_row("adcdef", "abcxee");
    let changes2 = check_row("adddef", "abcdeh");

    assert_eq!(changes1, 3);
    assert_eq!(changes2, 3);
  }
}