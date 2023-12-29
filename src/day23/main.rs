use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use shared::puzzle_input;

type Graph = HashMap<(u8, u8), HashMap<(u8, u8), usize>>;

fn parse(input: &str) -> (HashMap<(u8, u8), char>, (u8, u8)) {
    let mut tiles = HashMap::new();
    let mut dst = (0u8, 0u8);
    for (y, line) in input.split('\n').enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '.' | '>' | '<' | '^' | 'v' => {
                    tiles.insert((x as u8, y as u8), c);
                    if y as u8 > dst.1 {
                        dst = (x as u8, y as u8);
                    }
                }
                '#' => {}
                _ => unreachable!("unhandled case {c:?}"),
            }
        }
    }
    (tiles, dst)
}

fn to_graph(m: &HashMap<(u8, u8), char>, dst: (u8, u8), respect_arrows: bool) -> Graph {
    let mut edges: HashMap<(u8, u8), HashMap<(u8, u8), usize>> = HashMap::new();
    let mut stack = vec![(1u8, 0u8, 1u8, 1u8, 'v', 1usize)];
    let mut seen = HashSet::new();
    while let Some((from_x, from_y, x, y, direction, steps)) = stack.pop() {
        if (x, y) == dst {
            edges
                .entry((from_x, from_y))
                .or_default()
                .insert((x, y), steps);
            continue;
        }
        if respect_arrows {
            match m.get(&(x, y)).unwrap() {
                '^' => {
                    stack.push((from_x, from_y, x, y - 1, '^', steps + 1));
                    continue;
                }
                'v' => {
                    stack.push((from_x, from_y, x, y + 1, 'v', steps + 1));
                    continue;
                }
                '<' => {
                    stack.push((from_x, from_y, x - 1, y, '<', steps + 1));
                    continue;
                }
                '>' => {
                    stack.push((from_x, from_y, x + 1, y, '>', steps + 1));
                    continue;
                }
                '.' => {}
                _ => unreachable!(),
            }
        }
        let u = ((direction != 'v') && (y > 0))
            .then(|| m.get(&(x, y - 1)))
            .flatten()
            .copied();
        let d = (direction != '^')
            .then(|| m.get(&(x, y + 1)))
            .flatten()
            .copied();
        let l = ((direction != '>') && (x > 0))
            .then(|| m.get(&(x - 1, y)))
            .flatten()
            .copied();
        let r = (direction != '<')
            .then(|| m.get(&(x + 1, y)))
            .flatten()
            .copied();
        match [u, d, l, r].iter().flatten().count() {
            1 => {
                if u.is_some() {
                    stack.push((from_x, from_y, x, y - 1, '^', steps + 1));
                }
                if d.is_some() {
                    stack.push((from_x, from_y, x, y + 1, 'v', steps + 1));
                }
                if l.is_some() {
                    stack.push((from_x, from_y, x - 1, y, '<', steps + 1));
                }
                if r.is_some() {
                    stack.push((from_x, from_y, x + 1, y, '>', steps + 1));
                }
            }
            _ => {
                edges
                    .entry((from_x, from_y))
                    .or_default()
                    .insert((x, y), steps);
                if u.is_some() && seen.insert((x, y, '^')) {
                    stack.push((x, y, x, y - 1, '^', 1));
                }
                if d.is_some() && seen.insert((x, y, 'v')) {
                    stack.push((x, y, x, y + 1, 'v', 1));
                }
                if l.is_some() && seen.insert((x, y, '<')) {
                    stack.push((x, y, x - 1, y, '<', 1));
                }
                if r.is_some() && seen.insert((x, y, '>')) {
                    stack.push((x, y, x + 1, y, '>', 1));
                }
            }
        }
    }
    edges
}

fn path_len(graph: &Graph, path: &[(u8, u8)]) -> usize {
    path.iter()
        .tuple_windows()
        .map(|(a, b)| graph.get(a).unwrap().get(b).unwrap())
        .sum()
}

fn longest_path(graph: &Graph, start: (u8, u8), end: (u8, u8)) -> usize {
    let mut longest = 0usize;
    let mut stack = vec![vec![start]];
    while let Some(path) = stack.pop() {
        let current = path.iter().last().unwrap();
        let path_len = path_len(graph, &path);
        if *current == end {
            if path_len > longest {
                longest = path_len;
            }
            continue;
        }
        for dst in graph.get(current).unwrap().keys() {
            if !path.contains(dst) {
                let mut path2 = path.clone();
                path2.push(*dst);
                stack.push(path2);
            }
        }
    }
    longest
}

fn main() {
    let input = puzzle_input!();
    let (m, dst) = parse(&input);
    let graph = to_graph(&m, dst, true);
    let graph2 = to_graph(&m, dst, false);
    let p1 = longest_path(&graph, (1, 0), dst);
    let p2 = longest_path(&graph2, (1, 0), dst);
    println!("{p1}"); // 2370
    println!("{p2}"); // 6546
}

#[cfg(test)]
const EXAMPLE: &str = r"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
";

#[test]
fn p1_example() {
    let (m, dst) = parse(EXAMPLE);
    let graph = to_graph(&m, dst, true);
    assert_eq!(longest_path(&graph, (1, 0), dst), 94);
}

#[test]
fn p2_example() {
    let (m, dst) = parse(EXAMPLE);
    let graph = to_graph(&m, dst, false);
    assert_eq!(longest_path(&graph, (1, 0), dst), 154);
}
