fn main() {
    let home_team = "Liverpool";
    let result = " beat ";
    let away_team = "Manchester United";
    let home_score = '3';
    let away_score = "-0";

    let mut the_string = format!("{}{}{} ", home_team, result, away_team);
    the_string.push(home_score);
    the_string.push_str(away_score);
    println!("{}", the_string);
}
