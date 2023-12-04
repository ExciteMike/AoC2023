use std::collections::HashSet;

use itertools::Itertools;
use shared::puzzle_input;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Number {
    row: usize,
    start: usize,
    end: usize,
    value: usize,
}

fn p1_value(symbols: &HashSet<(usize, usize)>, n: &Number) -> usize {
    let min_row = n.row.max(1) - 1;
    let min_col = n.start.max(1) - 1;
    if (min_row..(n.row + 2))
        .cartesian_product(min_col..n.end + 1)
        .any(|xy| symbols.contains(&xy))
    {
        n.value
    } else {
        0
    }
}

fn p2_value(numbers: &[Number], (row, col): &(usize, usize)) -> usize {
    let mut neighbor_parts = numbers.iter().filter(|n| {
        (n.row..n.row + 3).contains(&(row + 1)) && (n.start..n.end + 2).contains(&(col + 1))
    });
    let a = neighbor_parts.next();
    let b = neighbor_parts.next();
    let c = neighbor_parts.next();
    if let (Some(a), Some(b), None) = (a, b, c) {
        a.value * b.value
    } else {
        0
    }
}

fn main() {
    let input = puzzle_input!();
    let mut cur_number = None;
    let mut numbers = Vec::new();
    let mut gears = HashSet::<(usize, usize)>::new();
    let mut symbols = HashSet::<(usize, usize)>::new();
    let mut cur_row = 0usize;
    let mut cur_col = 0usize;
    for c in input.chars() {
        if let Some(digit) = c.to_digit(10) {
            cur_number = Some(cur_number.map_or_else(
                || Number {
                    row: cur_row,
                    start: cur_col,
                    end: cur_col + 1,
                    value: digit as usize,
                },
                |n: Number| Number {
                    end: n.end + 1,
                    value: n.value * 10 + digit as usize,
                    ..n
                },
            ));
        } else if let Some(value) = cur_number {
            numbers.push(value);
            cur_number = None;
        }
        match c {
            '.' => {}
            '\n' => {
                cur_row += 1;
                cur_col = 0;
                continue;
            }
            '*' => {
                gears.insert((cur_row, cur_col));
                symbols.insert((cur_row, cur_col));
            }
            _ if !c.is_ascii_digit() => {
                symbols.insert((cur_row, cur_col));
            }
            _ => {}
        }
        cur_col += 1;
    }
    if let Some(value) = cur_number {
        numbers.push(value);
    }
    let p1 = numbers.iter().map(|n| p1_value(&symbols, n)).sum::<usize>();
    let p2 = gears.iter().map(|w| p2_value(&numbers, w)).sum::<usize>();
    println!("{p1}\n{p2}"); // 527369 73074886
}
