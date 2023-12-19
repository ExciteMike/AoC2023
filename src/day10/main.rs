use std::collections::{HashMap, VecDeque};

use itertools::Itertools;
use shared::puzzle_input;

trait Coord {
    fn n(&self) -> Option<Self>
    where
        Self: Sized;
    fn s(&self) -> Option<Self>
    where
        Self: Sized;
    fn e(&self) -> Option<Self>
    where
        Self: Sized;
    fn w(&self) -> Option<Self>
    where
        Self: Sized;
}
impl Coord for (u8, u8) {
    fn n(&self) -> Option<Self>
    where
        Self: Sized,
    {
        (self.1 > 0).then_some((self.0, self.1.wrapping_sub(1)))
    }

    fn s(&self) -> Option<Self>
    where
        Self: Sized,
    {
        Some((self.0, self.1 + 1))
    }

    fn e(&self) -> Option<Self>
    where
        Self: Sized,
    {
        Some((self.0 + 1, self.1))
    }

    fn w(&self) -> Option<Self>
    where
        Self: Sized,
    {
        (self.0 > 0).then_some((self.0.wrapping_sub(1), self.1))
    }
}

fn p2(ds: &HashMap<(u8, u8), u32>, pipes: &HashMap<(u8, u8), char>) -> usize {
    (0..140u8)
        .cartesian_product(0..140u8)
        .filter(|p| test_inside(ds, pipes, *p))
        .count()
}

fn test_inside(ds: &HashMap<(u8, u8), u32>, pipes: &HashMap<(u8, u8), char>, p: (u8, u8)) -> bool {
    if ds.contains_key(&p) {
        return false;
    }
    let mut is_inside = false;
    let mut began_wall_with = '.';
    // walk
    for x in 0..p.0 {
        let q = (x, p.1);
        if ds.contains_key(&q) {
            match pipes.get(&q) {
                Some(pipe @ ('F' | 'L')) => {
                    began_wall_with = *pipe;
                }
                Some('|') => {
                    is_inside = !is_inside;
                    began_wall_with = '.';
                }
                Some('J') if began_wall_with == 'F' => {
                    is_inside = !is_inside;
                    began_wall_with = '.';
                }
                Some('7') if began_wall_with == 'L' => {
                    is_inside = !is_inside;
                    began_wall_with = '.';
                }
                _ => {}
            }
        }
    }
    is_inside
}

fn read_pipes(s: &str, s_is: char) -> (HashMap<(u8, u8), char>, (u8, u8)) {
    const CAP: usize = 20_000;
    let mut m = HashMap::with_capacity(CAP);
    let mut start = (0, 0);
    for (y, line) in s.split_whitespace().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                'S' => {
                    start = (x as u8, y as u8);
                    m.insert(start, s_is);
                }
                'F' | '-' | '7' | '|' | 'J' | 'L' => {
                    m.insert((x as u8, y as u8), c);
                }
                _ => {}
            }
        }
    }
    assert!(m.capacity() <= 2 * CAP);
    (m, start)
}

fn dijkstra_map(pipes: &HashMap<(u8, u8), char>, start: (u8, u8)) -> (HashMap<(u8, u8), u32>, u32) {
    const DM_CAP: usize = 10_000;
    const Q_CAP: usize = 1_000;
    let mut dm = HashMap::with_capacity(DM_CAP);
    let mut queue = VecDeque::with_capacity(Q_CAP);
    let mut worst = 0;
    queue.push_back((start, 0u32));
    loop {
        let Some((p, distance)) = queue.pop_front() else {
            break;
        };
        if dm.contains_key(&p) {
            continue;
        }
        if distance > worst {
            worst = distance;
        }
        dm.insert(p, distance);
        let ps = match pipes.get(&p).unwrap() {
            '|' => [p.n(), p.s()],
            '-' => [p.e(), p.w()],
            'L' => [p.n(), p.e()],
            'J' => [p.n(), p.w()],
            '7' => [p.w(), p.s()],
            'F' => [p.e(), p.s()],
            _ => unreachable!(),
        };
        queue.extend(ps.iter().flatten().map(|p| (*p, distance + 1)));
    }
    assert!(queue.capacity() <= 2 * Q_CAP);
    assert!(dm.capacity() <= 2 * DM_CAP);
    (dm, worst)
}

fn main() {
    let (pipes, start) = read_pipes(&puzzle_input!(), '7');
    let (dm, worst) = dijkstra_map(&pipes, start);
    println!("{}", worst); // 6907
    println!("{}", p2(&dm, &pipes)); // 541
}

#[cfg(test)]
const P1_EXAMPLE: &str = "..F7.
.FJ|.
SJ.L7
|F--J
LJ...
";

#[cfg(test)]
const P2_EXAMPLE: &str = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
";

#[test]
fn p1test() {
    let (pipes, start) = read_pipes(P1_EXAMPLE, 'F');
    let (_, worst) = dijkstra_map(&pipes, start);
    assert_eq!(worst, 8);
}

#[test]
fn p2test() {
    let (pipes, start) = read_pipes(P2_EXAMPLE, '7');
    let dm = dijkstra_map(&pipes, start).0;

    println!("{}", P2_EXAMPLE);

    for y in 0..10 {
        for x in 0..20 {
            let p = (x, y);
            if test_inside(&dm, &pipes, p) {
                print!("x");
            } else if dm.contains_key(&p) {
                print!(
                    "{}",
                    match pipes.get(&p).unwrap() {
                        'F' => '┏',
                        '-' => '━',
                        '7' => '┓',
                        '|' => '┃',
                        'J' => '┛',
                        'L' => '┗',
                        _ => panic!(),
                    }
                );
            } else {
                print!(" ");
            }
        }
        println!();
    }

    assert_eq!(p2(&dm, &pipes), 10);
}
