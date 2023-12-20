use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use shared::puzzle_input;

fn parse_line(line: &str) -> Option<(char, isize)> {
    let mut it = line.split_whitespace();
    let dir = it.next()?.chars().next()?;
    let amnt = it.next()?.parse::<isize>().ok()?;
    Some((dir, amnt))
}
fn parse_line2(line: &str) -> Option<(char, isize)> {
    let hex = &line[line.find('#')? + '#'.len_utf8()..line.find(')')?];
    let amnt = isize::from_str_radix(&hex[..5], 16).ok()?;
    let dir = match hex.chars().last()? {
        '0' => 'R',
        '1' => 'D',
        '2' => 'L',
        '3' => 'U',
        _ => unreachable!(),
    };
    Some((dir, amnt))
}

fn dig<F>(input: &str, f: F) -> Option<HashSet<(isize, isize)>>
where
    F: Fn(&str) -> Option<(char, isize)>,
{
    let mut p = (0isize, 0isize);
    let mut trenches: HashSet<(isize, isize)> = HashSet::new();
    for line in input.split('\n') {
        if line.is_empty() {
            continue;
        }
        let (dir, amnt) = f(line)?;
        match dir {
            'U' => {
                for _ in 0..amnt {
                    trenches.insert(p.clone());
                    p = (p.0, p.1 - 1);
                }
            }
            'D' => {
                for _ in 0..amnt {
                    trenches.insert(p.clone());
                    p = (p.0, p.1 + 1);
                }
            }
            'L' => {
                for _ in 0..amnt {
                    trenches.insert(p.clone());
                    p = (p.0 - 1, p.1);
                }
            }
            'R' => {
                for _ in 0..amnt {
                    trenches.insert(p.clone());
                    p = (p.0 + 1, p.1);
                }
            }
            _ => unreachable!(),
        };
    }
    Some(trenches)
}

fn to_coords(instructions: &[(char, isize)]) -> Vec<(isize, isize)> {
    let mut p = (0, 0);
    let mut v = vec![];
    for (dir, amnt) in instructions {
        p = match dir {
            'U' => (p.0, p.1 - amnt),
            'D' => (p.0, p.1 + amnt),
            'L' => (p.0 - amnt, p.1),
            'R' => (p.0 + amnt, p.1),
            _ => unreachable!(),
        };
        v.push(p);
    }
    v
}

fn ys_by_x(coords: &[(isize, isize)]) -> HashMap<isize, Vec<isize>> {
    let mut m: HashMap<isize, Vec<isize>> = HashMap::new();
    for (x, y) in coords {
        match m.entry(*x) {
            std::collections::hash_map::Entry::Occupied(mut e) => {
                e.get_mut().push(*y);
            }
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(vec![*y]);
            }
        }
    }
    for (x, v) in m.iter_mut() {
        *v = v.iter().copied().unique().sorted().collect_vec();
        println!("ys by x. x={x}. ys={v:?}");
        assert!(v.len() % 2 == 0);
    }
    m
}

fn xs(coords: &[(isize, isize)]) -> impl Iterator<Item = isize> {
    coords.iter().map(|(x, _)| x).copied().unique().sorted()
}

fn fill(m: &mut HashSet<(isize, isize)>) {
    let start = {
        let min_x = m.iter().map(|p| p.0).min().unwrap();
        let min_y = m.iter().map(|p| p.1).min().unwrap();
        let mut p = (min_x, min_y);
        while !m.contains(&p) {
            p = (p.0 + 1, p.1);
        }
        (p.0 + 1, p.1 + 1)
    };

    let mut stack = vec![start];
    while let Some(p @ (x, y)) = stack.pop() {
        if !m.contains(&p) {
            m.insert(p);
            for p2 in [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)] {
                if !m.contains(&p2) {
                    stack.push(p2);
                }
            }
        }
    }
}

fn score(v: &[(char, isize)]) -> isize {
    let mut total = 0;
    let coords = to_coords(&v);
    println!("{coords:?}");
    let ys = ys_by_x(&coords);
    let mut cur_ys = Vec::<isize>::new();
    for (x1, x2) in xs(&coords).tuple_windows() {
        if cur_ys.is_empty() {
            // initialize cur_ys and count first column
            let src_ys = ys.get(&x1).unwrap();
            src_ys.clone_into(&mut cur_ys);
            for (y1, y2) in src_ys.iter().tuple_windows() {
                total += y2 - y1 + 1;
            }
            continue;
        }
        // count everything in x2
        let current_column = ys.get(&x2).unwrap();
        for (y1, y2) in current_column.iter().tuple_windows() {
            total += y2 - y1 + 1;

            // update cur_ys
        }
        // count everything between columns x1 and x2
        for (y1, y2) in cur_ys.iter().tuple_windows() {
            total += (x2 - x1) * (y2 - y1 + 1);
        }
        for y in current_column {
            match cur_ys.binary_search(y) {
                Ok(i) => {
                    cur_ys.remove(i);
                }
                Err(i) => cur_ys.insert(i, *y),
            }
        }
        assert!(cur_ys.len() % 2 == 0);
    }
    total
}

fn p1(input: &str) -> Option<usize> {
    let mut trenches = dig(&input, parse_line)?;
    fill(&mut trenches);
    Some(trenches.len())
}

fn solution() {
    let input = puzzle_input!();
    println!("{}", p1(&input).unwrap()); // 41019
}

fn main() {
    const N_TIMES: u32 = 1;
    use std::time::Instant;
    let now = Instant::now();
    for _ in 0..N_TIMES {
        solution();
    }

    let elapsed = now.elapsed();
    eprintln!("Completed in average of {:.2?}", elapsed / N_TIMES);
}

#[cfg(test)]
const EXAMPLE: &str = r"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
";

#[test]
fn p1test() {
    let v = EXAMPLE
        .split('\n')
        .filter_map(|line| parse_line(line))
        .collect_vec();
    assert_eq!(score(&v), 62);
}

#[allow(dead_code)]
fn dump(m: &HashSet<(isize, isize)>) {
    let min_x = m.iter().map(|p| p.0).min().unwrap();
    let max_x = m.iter().map(|p| p.0).max().unwrap();
    let min_y = m.iter().map(|p| p.1).min().unwrap();
    let max_y = m.iter().map(|p| p.1).max().unwrap();

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if m.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

#[test]
fn p2test() {
    let v = EXAMPLE
        .split('\n')
        .filter_map(|line| parse_line2(line))
        .collect_vec();
    assert_eq!(
        &v,
        &[
            ('R', 461937),
            ('D', 56407),
            ('R', 356671),
            ('D', 863240),
            ('R', 367720),
            ('D', 266681),
            ('L', 577262),
            ('U', 829975),
            ('L', 112010),
            ('D', 829975),
            ('L', 491645),
            ('U', 686074),
            ('L', 5411),
            ('U', 500254),
        ]
    );
    assert_eq!(score(&v), 952408144115);
}
