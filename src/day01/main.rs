use itertools::Itertools;
use lazy_static::lazy_static;
use shared::puzzle_input;

lazy_static! {
    static ref PARSE_TABLE: Vec<(usize, &'static str)> = "zero one two three four five six seven eight nine"
    .split_ascii_whitespace().enumerate().collect_vec();
}

fn read_p2(line: &str) -> usize {
    let mut line = line;
    let mut v = Vec::new();
    'next_digit: while let Some(c) = line.chars().next() {
        if c.is_ascii_digit() {
            v.push(c.to_digit(10).unwrap() as usize);
            line = &line[1..];
        } else {
            for (value, s) in &*PARSE_TABLE {
                if line.starts_with(s) {
                    v.push(*value);
                    line = &line[(s.len())..];
                    continue 'next_digit;
                }
            }
            line = &line[1..];
            continue 'next_digit;
        }
    }
    v.first().unwrap() * 10 + v.last().unwrap()
}

fn main() {
    let input = puzzle_input!();
    let lines = input.split_ascii_whitespace();
    let p1 = lines
        .clone()
        .map(|line| {
            let mut i1 = line.chars().filter(|c| char::is_numeric(*c));
            let mut i2 = i1.clone().rev();
            let a = i1.next().unwrap_or('0').to_digit(10).unwrap();
            let b = i2.next().unwrap_or('0').to_digit(10).unwrap();
            a * 10 + b
        })
        .sum::<u32>();
    println!("part 1: {p1}");
    println!("part 2: {}", lines.map(read_p2).sum::<usize>());
}
