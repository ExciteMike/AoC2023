use itertools::Itertools;
use shared::puzzle_input;
use std::collections::HashSet;

struct Block {
    pub mirrors: HashSet<(i64, i64)>,
    pub max_x: i64,
    pub max_y: i64,
}

impl Block {
    fn test_reflection<F>(&self, p1: &(i64, i64), i: i64, xform: F) -> bool
    where
        F: Fn(i64, (i64, i64)) -> (i64, i64),
    {
        let p2 = xform(i, *p1);
        ((p2.0 < 0) || (p2.0 >= self.max_x) || (p2.1 < 0) || (p2.1 >= self.max_y))
            || self.mirrors.contains(&p2)
    }
    pub fn find_line<F>(&self, missing: i64, max: i64, xform: F) -> Option<i64>
    where
        F: Fn(i64, (i64, i64)) -> (i64, i64),
    {
        for i in 1..max {
            let it = self.mirrors.iter();
            let n = it.filter(|p1| self.test_reflection(p1, i, &xform)).count() as i64;
            if self.mirrors.len() as i64 - missing == n {
                return Some(i);
            }
        }
        None
    }
    pub fn h_line(&self, missing: i64) -> Option<i64> {
        self.find_line(missing, self.max_y, |i, (x, y)| (x, 2 * i - y - 1))
    }
    pub fn v_line(&self, missing: i64) -> Option<i64> {
        self.find_line(missing, self.max_x, |i, (x, y)| (2 * i - x - 1, y))
    }
}

fn summarize(block: &Block, missing: i64) -> i64 {
    block
        .v_line(missing)
        .unwrap_or_else(|| 100 * block.h_line(missing).unwrap())
}

fn parse_row((y, line): (usize, &&str)) -> Vec<(i64, i64)> {
    let parse_tile = |x: usize, y: usize, c: char| (c == '#').then_some((x as i64, y as i64));
    line.chars()
        .enumerate()
        .filter_map(move |(x, c)| parse_tile(x, y, c))
        .collect_vec()
}

fn parse(block: &str) -> Block {
    let lines = block.split_whitespace().collect_vec();
    Block {
        mirrors: lines.iter().enumerate().flat_map(parse_row).collect(),
        max_x: lines[0].len() as i64,
        max_y: lines.len() as i64,
    }
}

fn main() {
    let pats = puzzle_input!().split("\n\n").map(parse).collect_vec();
    println!("{}", pats.iter().map(|b| summarize(b, 0)).sum::<i64>()); // 33122
    println!("{}", pats.iter().map(|b| summarize(b, 1)).sum::<i64>()); // 32312
}
