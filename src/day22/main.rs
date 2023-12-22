use std::collections::{HashMap, HashSet};

use itertools::Itertools;
use shared::puzzle_input;

#[derive(Clone, Copy)]
struct StackData {
    brick_id: u16,
    height: u16,
}

fn p1(input: &str) -> usize {
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
    let mut supports: HashMap<u16, HashSet<u16>> = HashMap::new();
    let mut is_supported_by: HashMap<u16, HashSet<u16>> = HashMap::new();
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
                    }
                    is_supported_by
                        .entry(brick_id)
                        .or_default()
                        .insert(data.brick_id);
                }
            }
        }
        let brick_top = supported_at_height + 1 + z2 - z1;
        for (x, y) in brick_xys {
            stack_data[y as usize][x as usize].brick_id = brick_id;
            stack_data[y as usize][x as usize].height = brick_top;
        }

        // eprintln!("added brick {brick_id}");
        // for row in &stack_data {
        //     for data in row {
        //         let star = if data.brick_id == brick_id {
        //             '*'
        //         } else {
        //             ' '
        //         };
        //         eprint!("{:4}{star}", data.height);
        //     }
        //     eprintln!();
        // }
        // eprintln!();
    }

    bricks
        .filter(|(id, _)| {
            if let Some(supported_bricks) = supports.get(id) {
                supported_bricks.iter().all(|supported_id| {
                    is_supported_by
                        .get(supported_id)
                        .map(|s| s.len() > 1)
                        .unwrap_or(false)
                })
            } else {
                true
            }
        })
        .count()
}

fn main() {
    let input = puzzle_input!();
    println!("{}", p1(&input)); // < 1152
}

#[cfg(test)]
const EXAMPLE: &str = r"1,0,1~1,2,1
0,0,2~2,0,2
1,1,8~1,1,9
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
";

#[test]
fn p1_example() {
    assert_eq!(p1(EXAMPLE), 5);
}

#[test]
fn p2_example() {}
