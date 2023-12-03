use std::collections::HashSet;

use itertools::Itertools;
use shared::puzzle_input;

/// string starting with digits to the parsed number and it's length.
/// EXAMPLE "123..." -> (123, 3)
fn read_num(s: &str) -> (usize, usize) {
    let mut value = 0usize;
    for (n, c) in s.chars().enumerate() {
        if let Some(digit) = c.to_digit(10) {
            value = value * 10 + digit as usize;
        } else {
            return (value, n);
        }
    }
    (value, s.len())
}

#[derive(Debug)]
struct XYLenVal(usize, usize, usize, usize);

fn neighbors(x: usize, y: usize, len: usize) -> Vec<(usize, usize)> {
    let xmin = if x == 0 { x } else { x - 1 };
    let ymin = if y == 0 { y } else { y - 1 };
    (ymin..=(y + 1))
        .flat_map(|y| (xmin..(x + len + 1)).map(move |x| (x, y)))
        .collect_vec()
}
fn to_part_number(data: &HashSet<(usize, usize)>, xylenval: &XYLenVal) -> Option<usize> {
    let &XYLenVal(x, y, len, val) = xylenval;
    if neighbors(x, y, len).iter().any(|xy| data.contains(xy)) {
        Some(val)
    } else {
        None
    }
}
fn neighbor_numbers(
    numbers: &'_ [XYLenVal],
    gear_x: usize,
    gear_y: usize,
) -> impl Iterator<Item = usize> + '_ {
    numbers.iter().flat_map(move |XYLenVal(x, y, len, val)| {
        let xmin = if *x == 0 { *x } else { *x - 1 };
        let ymin = if *y == 0 { *y } else { *y - 1 };
        if (xmin..(x + len + 1)).contains(&gear_x) && (ymin..(y + 2)).contains(&gear_y) {
            Some(*val)
        } else {
            None
        }
    })
}

fn main() {
    let input = puzzle_input!();
    // let input = EXAMPLE;
    let lines = input.split('\n').enumerate();
    let numbers = lines
        .clone()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, c)| {
                let starts_a_number = c.is_ascii_digit()
                    && ((x == 0) || !line.chars().nth(x - 1).unwrap().is_ascii_digit());
                if starts_a_number {
                    let (value, length) = read_num(&line[x..]);
                    Some(XYLenVal(x, y, length, value))
                } else {
                    None
                }
            })
        })
        .collect_vec();
    let symbols: HashSet<(usize, usize)> = lines
        .clone()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, c)| {
                if !c.is_ascii_digit() && (c != '.') {
                    Some((x, y))
                } else {
                    None
                }
            })
        })
        .collect();

    let p1 = numbers
        .iter()
        .filter_map(|xylenval| to_part_number(&symbols, xylenval))
        .sum::<usize>();
    let p2: usize = lines
        .flat_map(|(y, line)| {
            let numbers = &numbers;
            line.chars().enumerate().filter_map(move |(x, c)| {
                if c == '*' {
                    let nn = neighbor_numbers(numbers, x, y).collect_vec();
                    if nn.len() == 2 {
                        Some(nn[0] * nn[1])
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        })
        .sum();
    println!("{p1}\n{p2}"); // 527369 73074886
}
