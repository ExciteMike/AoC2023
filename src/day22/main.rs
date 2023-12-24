use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;
use shared::puzzle_input;

#[derive(Clone, Copy)]
struct StackData {
    brick_id: u16,
    height: u16,
}

fn parse(input: &str) -> (HashMap<u16, HashSet<u16>>, HashMap<u16, HashSet<u16>>) {
    let bricks = input
        .trim()
        .split('\n')
        .map(|line| {
            line.split('~')
                .map(|coord| {
                    coord
                        .split(',')
                        .map(|s| s.parse::<u16>().unwrap())
                        .collect_tuple::<(_, _, _)>()
                        .unwrap()
                })
                .collect_tuple::<(_, _)>()
                .unwrap()
        })
        .enumerate()
        .map(|(i, rest)| (i as u16 + 1, rest))
        .sorted_by(|a, b| a.1 .0 .2.cmp(&b.1 .0 .2));
    let mut stack_data = [[StackData {
        brick_id: 0,
        height: 0,
    }; 10]; 10];
    let mut supports: HashMap<u16, HashSet<u16>> = bricks.clone().map(|(id,_)|(id, HashSet::new())).collect();
    let mut is_supported_by: HashMap<u16, HashSet<u16>> = supports.clone();
    for (brick_id, ((x1, y1, z1), (x2, y2, z2))) in bricks.clone() {
        debug_assert!(x2 >= x1);
        debug_assert!(y2 >= y1);
        debug_assert!(z2 >= z1);
        debug_assert!(matches!((x2-x1, y2-y1, z2-z1), (0,0,_) | (0,_,0) | (_, 0, 0)));
        let brick_xys = (x1..=x2).cartesian_product(y1..=y2);
        let supported_at_height = brick_xys
            .clone()
            .map(|(x, y)| stack_data[y as usize][x as usize].height)
            .max()
            .unwrap();
        if supported_at_height > 0 {
            for (x, y) in brick_xys.clone() {
                let data = &stack_data[y as usize][x as usize];
                if data.brick_id != 0 {
                    if data.height == supported_at_height {
                        supports.entry(data.brick_id).or_default().insert(brick_id);
                        is_supported_by
                            .entry(brick_id)
                            .or_default()
                            .insert(data.brick_id);
                    }
                }
            }
        }
        let brick_top = supported_at_height + 1 + z2 - z1;
        for (x, y) in brick_xys {
            stack_data[y as usize][x as usize].brick_id = brick_id;
            stack_data[y as usize][x as usize].height = brick_top;
        }
    }
    (supports, is_supported_by)
}

fn p1(supports: &HashMap<u16, HashSet<u16>>, is_supported_by: &HashMap<u16, HashSet<u16>>) -> usize {
    supports
        .keys()
        .filter(|id| {
            if let Some(supported_bricks) = supports.get(id) {
                let can_go = supported_bricks.iter().all(|supported_id| {
                    is_supported_by
                        .get(supported_id)
                        .map(|s| s.len() > 1)
                        .unwrap_or(false)
                });
                can_go
            } else {
                true
            }
        })
        .count()
}

fn p2(supports: &HashMap<u16, HashSet<u16>>, is_supported_by: &HashMap<u16, HashSet<u16>>) -> usize {
    supports
        .keys()
        .map(|id| {
            let mut falls: HashSet<u16> = HashSet::new();

            let mut q = VecDeque::from([*id]);
            while let Some(id) = q.pop_front() {
                if let Some(supported_ids) = supports.get(&id) {
                    for supported_id in supported_ids {
                        if let Some(supporters) = is_supported_by.get(supported_id) {
                            let supporters: HashSet<_> = supporters.difference(&falls).collect();
                            if supporters.is_empty() || (supporters.len() == 1 && supporters.contains(&id)) {
                                if falls.insert(*supported_id) {
                                    q.push_back(*supported_id);
                                }
                            }
                        }
                    }
                }
            }
            falls.len()
        })
        .sum::<usize>()
}

fn main() {
    let input = puzzle_input!();
    let (supports, is_supported_by) = parse(&input);
    println!("{}", p1(&supports, &is_supported_by)); // 480
    println!("{}", p2(&supports, &is_supported_by) ); // 84021
}

#[cfg(test)]
const EXAMPLE: &str = r"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
";

#[cfg(test)]
const EXAMPLE2: &str = r"0,0,1~0,5,1
0,0,11~0,5,11
0,0,21~0,5,21
0,6,1~0,9,1
0,0,2~0,0,2
0,3,2~0,8,2
";

#[test]
fn p1_example() {
    let (supports, is_supported_by) = parse(EXAMPLE);
    assert_eq!(p1(&supports, &is_supported_by), 5);
}
#[test]
fn p1_example_2() {
    let (supports, is_supported_by) = parse(EXAMPLE2);
    assert_eq!(p1(&supports, &is_supported_by), 4);
}

#[test]
fn p2_example() {
    let (supports, is_supported_by) = parse(EXAMPLE);
    assert_eq!(p2(&supports, &is_supported_by), 7);
}
