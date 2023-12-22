use std::collections::{HashMap, HashSet, VecDeque};

use shared::puzzle_input;

struct Garden {
    rocks: HashSet<(isize, isize)>,
    start: (isize, isize),
    max_x: isize,
    max_y: isize,
}

fn parse(s: &str) -> Garden {
    let mut start = (0isize, 0isize);
    let mut max_x = 0isize;
    let mut max_y = 0isize;
    let mut rocks: HashSet<(isize, isize)> = HashSet::new();
    for (y, line) in s.trim().split('\n').enumerate() {
        let y = y as isize;
        for (x, c) in line.chars().enumerate() {
            let x = x as isize;
            match c {
                '#' => {
                    rocks.insert((x, y));
                }
                'S' => {
                    start = (x, y);
                }
                _ => {
                    max_x = max_x.max(x);
                    max_y = max_y.max(y);
                }
            }
        }
    }
    Garden {
        rocks,
        start,
        max_x,
        max_y,
    }
}

trait Neighbors {
    fn neighbors(&self, max_x: isize, max_y: isize) -> Vec<Self>
    where
        Self: Sized;
}
impl Neighbors for (isize, isize) {
    fn neighbors(&self, max_x: isize, max_y: isize) -> Vec<Self> {
        let mut v = Vec::with_capacity(4);
        if self.1 > 0 {
            v.push((self.0, self.1 - 1));
        }
        if self.1 < max_y {
            v.push((self.0, self.1 + 1));
        }
        if self.0 > 0 {
            v.push((self.0 - 1, self.1));
        }
        if self.0 < max_x {
            v.push((self.0 + 1, self.1));
        }
        v
    }
}

fn track_visits(garden: &Garden, stop_at: isize, wrap: bool) -> HashMap<(isize, isize), isize> {
    let mut visits: HashMap<(isize, isize), isize> = HashMap::new();
    let mut q = VecDeque::from([(garden.start, 0isize)]);
    while let Some((p, step)) = q.pop_front() {
        match visits.entry(p) {
            std::collections::hash_map::Entry::Occupied(_) => {
                continue;
            }
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(step);
            }
        }
        if step < stop_at {
            let neighbors = if wrap {
                vec![
                    (p.0, p.1 - 1),
                    (p.0, p.1 + 1),
                    (p.0 - 1, p.1),
                    (p.0 + 1, p.1),
                ]
            } else {
                p.neighbors(garden.max_x, garden.max_y)
            };
            for neighbor in neighbors {
                let end_x = garden.max_x + 1;
                let end_y = garden.max_y + 1;
                let wrapped = (
                    ((neighbor.0 % end_x) + end_x) % end_x,
                    ((neighbor.1 % end_y) + end_y) % end_y,
                );
                if !garden.rocks.contains(&wrapped) && !visits.contains_key(&neighbor) {
                    q.push_back((neighbor, step + 1));
                }
            }
        }
    }
    visits
}

fn solution() {
    let input = puzzle_input!();
    let garden = parse(&input);
    let m = track_visits(&garden, 64, false);
    let p1 = m.values().filter(|&&steps| steps % 2 == 0).count();
    println!("{p1}"); // 3642

    // once you catch that if you limit it to steps of 2*size, it must follow a
    // quadratic curve, you can take some samples then algebra it out
    let x = 26501365i128;
    let (a, b, c) = (14871i128, 32425i128, -137664i128);
    let denom = 17161i128;
    let predicted: i128 = (a * (x * x) + b * x + c) / denom;
    eprintln!("{}", predicted); // 608603023105276
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
const EXAMPLE: &str = r"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";

#[allow(dead_code)]
fn dump(garden: &Garden, m: &HashMap<(isize, isize), isize>) {
    for y in 0..=garden.max_y {
        for x in 0..=garden.max_x {
            let p = (x, y);
            if garden.rocks.contains(&p) {
                eprint!("#");
            } else if let Some(n) = m.get(&p) {
                eprint!("{}", n % 10);
            } else {
                eprint!(".");
            }
        }
        eprintln!();
    }
    eprintln!();
}

#[test]
fn p1_example() {
    let garden = parse(EXAMPLE);
    let m = track_visits(&garden, 6, false);
    let sixes = m.values().filter(|&&steps| steps % 2 == 0).count();
    assert_eq!(sixes, 16);
}

#[test]
fn p2_example() {
    let garden = parse(EXAMPLE);
    let m = track_visits(&garden, 10, true);
    assert_eq!(m.values().filter(|&&steps| steps % 2 == 0).count(), 50);
    let m: HashMap<(isize, isize), isize> = track_visits(&garden, 50, true);
    assert_eq!(m.values().filter(|&&steps| steps % 2 == 0).count(), 1594);
    let m: HashMap<(isize, isize), isize> = track_visits(&garden, 100, true);
    assert_eq!(m.values().filter(|&&steps| steps % 2 == 0).count(), 6536);
    let m: HashMap<(isize, isize), isize> = track_visits(&garden, 500, true);
    assert_eq!(m.values().filter(|&&steps| steps % 2 == 0).count(), 167004);
}
#[test]
fn p2_test() {
    let input = puzzle_input!();
    let garden = parse(&input);

    assert_eq!(garden.max_x, 130);
    assert_eq!(garden.max_y, 130);
    assert_eq!(26501365 % (garden.max_x + 1), 65);
    let (a, b, c) = (14871i128, 32425i128, -137664i128);
    let denom = 17161i128;

    for steps in (327i128..=1113).step_by(262) {
        let m: HashMap<(isize, isize), isize> = track_visits(&garden, steps as isize, true);
        let simulated = m.values().filter(|&&steps| steps % 2 == 1).count() as isize;
        let predicted = (a * (steps * steps) + b * steps + c) / denom;
        eprintln!(
            "{steps:8}\t{simulated:8}\t{:8}",
            predicted - (simulated as i128)
        );
    }
}
