use regex::Regex;
use shared::puzzle_input;

fn color_max(rounds: &str, color_name: &str) -> usize {
    let re = Regex::new(&format!(r"(\d+) {color_name}")).unwrap();
    rounds
        .split(';')
        .flat_map(|s| re.captures(s))
        .map(|caps| caps.get(1).unwrap().as_str().parse().unwrap())
        .max()
        .unwrap_or(0)
}

fn p1(line: &str) -> Option<usize> {
    let re = Regex::new(r"Game (\d+): (((\d+) (\w+))([^\d]*(\d+) (\w+))*)").ok()?;
    let caps = re.captures(line)?;
    let game_id: usize = caps.get(1)?.as_str().parse().ok()?;
    let rounds = caps.get(2)?.as_str();
    if color_max(rounds, "red") > 12 {
        return None;
    }
    if color_max(rounds, "green") > 13 {
        return None;
    }
    if color_max(rounds, "blue") > 14 {
        return None;
    }
    Some(game_id)
}

fn p2(line: &str) -> Option<usize> {
    let re = Regex::new(r"[^:]: (((\d+) (\w+))([^\d]*(\d+) (\w+))*)").ok()?;
    let caps = re.captures(line)?;
    let rounds = caps.get(1)?.as_str();
    Some(color_max(rounds, "red") * color_max(rounds, "green") * color_max(rounds, "blue"))
}

fn main() {
    let input = puzzle_input!();
    println!("{}", input.split('\n').flat_map(p1).sum::<usize>());
    println!("{}", input.split('\n').flat_map(p2).sum::<usize>());
}
