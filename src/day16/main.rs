use std::collections::{HashMap, HashSet, VecDeque};

use shared::puzzle_input;

fn parse(s: &str) -> HashMap<(u8, u8), char> {
    s.split_whitespace()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(x, c)| (c != '.').then_some(((x as u8, y as u8), c)))
        })
        .collect()
}

trait BeamState
where
    Self: Sized,
{
    fn u(&self) -> Option<Self>;
    fn d(&self) -> Option<Self>;
    fn l(&self) -> Option<Self>;
    fn r(&self) -> Option<Self>;
    fn fwd(&self) -> Option<Self>;
    fn ud(&self) -> Vec<Self>;
    fn lr(&self) -> Vec<Self>;
}
impl BeamState for ((u8, u8), char) {
    fn u(&self) -> Option<Self> {
        (self.0 .1 > 0).then_some(((self.0 .0, self.0 .1.wrapping_sub(1)), '^'))
    }
    fn d(&self) -> Option<Self> {
        Some(((self.0 .0, self.0 .1 + 1), 'v'))
    }
    fn l(&self) -> Option<Self> {
        (self.0 .0 > 0).then_some(((self.0 .0.wrapping_sub(1), self.0 .1), '<'))
    }
    fn r(&self) -> Option<Self> {
        Some(((self.0 .0 + 1, self.0 .1), '>'))
    }
    fn fwd(&self) -> Option<Self> {
        match self.1 {
            '>' => self.r(),
            'v' => self.d(),
            '<' => self.l(),
            '^' => self.u(),
            _ => unreachable!(),
        }
    }
    fn ud(&self) -> Vec<Self> {
        [self.u(), self.d()].into_iter().flatten().collect()
    }
    fn lr(&self) -> Vec<Self> {
        [self.l(), self.r()].into_iter().flatten().collect()
    }
}

fn sim_beam(m: &HashMap<(u8, u8), char>, start: ((u8, u8), char)) -> HashSet<(u8, u8)> {
    let w = 1 + m.keys().map(|p| p.0).max().unwrap();
    let h = 1 + m.keys().map(|p| p.1).max().unwrap();
    let mut q = VecDeque::from([start]);
    let mut visited = HashSet::<((u8, u8), char)>::new();
    while let Some(state) = q.pop_front() {
        if visited.contains(&state) {
            continue;
        }
        if state.0 .0 >= w {
            continue;
        }
        if state.0 .1 >= h {
            continue;
        }
        visited.insert(state);

        match (m.get(&state.0), state.1) {
            (None, _)
            | (Some('|'), '^')
            | (Some('|'), 'v')
            | (Some('-'), '>')
            | (Some('-'), '<') => q.extend(state.fwd()),
            (Some('/'), '>') | (Some('\\'), '<') => q.extend(state.u()),
            (Some('/'), 'v') | (Some('\\'), '^') => q.extend(state.l()),
            (Some('/'), '<') | (Some('\\'), '>') => q.extend(state.d()),
            (Some('/'), '^') | (Some('\\'), 'v') => q.extend(state.r()),
            (Some('|'), '>') | (Some('|'), '<') => q.extend(state.ud().iter()),
            (Some('-'), 'v') | (Some('-'), '^') => q.extend(state.lr().iter()),
            _ => unreachable!("unhandled case"),
        }
    }
    visited.iter().map(|x| x.0).collect()
}

fn p1(m: &HashMap<(u8, u8), char>, start: ((u8, u8), char)) -> usize {
    sim_beam(m, start).len()
}

fn p2(m: HashMap<(u8, u8), char>) -> usize {
    let w = 1 + m.iter().map(|x| x.0 .0).max().unwrap();
    let h = 1 + m.iter().map(|x| x.0 .1).max().unwrap();
    (0..w)
        .map(|x| ((x, 0), 'v'))
        .chain((0..w).map(|x| ((x, h - 1), '^')))
        .chain((0..h).map(|y| ((0, y), '>')))
        .chain((0..h).map(|y| ((w - 1, y), '<')))
        .map(|start| p1(&m, start))
        .max()
        .unwrap()
}

fn solution() {
    let input = puzzle_input!();
    let m = parse(&input);
    println!("{}", p1(&m, ((0u8, 0u8), '>'))); // 7111
    println!("{}", p2(m)); // 7831
}

fn main() {
    const N_TIMES: u32 = 10;
    use std::time::Instant;
    let now = Instant::now();
    for _ in 0..N_TIMES {
        solution();
    }

    let elapsed = now.elapsed();
    eprintln!("Completed in average of {:.2?}", elapsed / N_TIMES);
}

#[cfg(test)]
const EXAMPLE: &str = r".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
";

#[test]
fn p1test() {
    assert_eq!(p1(&parse(EXAMPLE), ((0u8, 0u8), '>')), 46);
}

#[allow(dead_code)]
fn dump(s: &HashSet<(u8, u8)>) {
    let w = 1 + s.iter().map(|p| p.0).max().unwrap();
    let h = 1 + s.iter().map(|p| p.1).max().unwrap();
    for y in 0..h {
        for x in 0..w {
            if s.contains(&(x, y)) {
                eprint!("#");
            } else {
                eprint!(".");
            }
        }
        eprintln!();
    }
    eprintln!();
}

#[test]
fn p2test() {
    assert_eq!(p2(parse(EXAMPLE)), 51);
}
