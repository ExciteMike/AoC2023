use std::collections::HashSet;

use itertools::Itertools;
use shared::puzzle_input;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Dir {
    L,
    R,
    U,
    D,
    None,
}

struct ParseResult {
    data: Box<[usize]>,
    w: u8,
    h: u8,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct State {
    x: u8,
    y: u8,
    loss: usize,
    dir: Dir,
    streak: u8,
}

fn parse(input: &str) -> ParseResult {
    let w = input.find('\n').unwrap() as u8;
    let h = 1 + input.trim_end().chars().filter(|c| *c == '\n').count() as u8;
    let data = input
        .chars()
        .filter_map(|c| c.is_ascii_digit().then_some(c))
        .filter_map(|c| c.to_digit(10))
        .map(|v| v as usize)
        .collect_vec()
        .into_boxed_slice();
    ParseResult { data, w, h }
}

fn sort_func(s1: &State, s2: &State) -> std::cmp::Ordering {
    (s2.loss, s2.x as u32 + s2.y as u32).cmp(&(s1.loss, s1.x as u32 + s1.y as u32))
}

fn solve<F>(losses: &ParseResult, move_rules: F) -> usize
where
    F: Fn(&State) -> Moves,
{
    let mut states = Vec::with_capacity(16_384);
    states.push(State {
        x: 0,
        y: 0,
        loss: 0,
        dir: Dir::None,
        streak: 0,
    });
    let mut visited = HashSet::with_capacity(762_355);
    while let Some(
        state @ State {
            x,
            y,
            loss,
            dir,
            streak,
        },
    ) = states.pop()
    {
        let sig = (x, y, dir, streak);
        if visited.contains(&sig) {
            continue;
        }
        visited.insert(sig);
        if (x + 1, y + 1) == (losses.w, losses.h) {
            return loss;
        }

        let (xusize, yusize, wusize) = (x as usize, y as usize, losses.w as usize);
        let moves = move_rules(&state);

        if moves.right && (x + 1 < losses.w) {
            states.push(State {
                x: x + 1,
                y,
                loss: loss + losses.data[yusize * wusize + xusize + 1],
                dir: Dir::R,
                streak: if dir == Dir::R { streak + 1 } else { 1 },
            });
        }
        if moves.down && (y + 1 < losses.h) {
            states.push(State {
                x,
                y: y + 1,
                loss: loss + losses.data[(yusize + 1) * wusize + xusize],
                dir: Dir::D,
                streak: if dir == Dir::D { streak + 1 } else { 1 },
            });
        }
        if moves.left && (x > 0) {
            states.push(State {
                x: x - 1,
                y,
                loss: loss + losses.data[yusize * wusize + xusize - 1],
                dir: Dir::L,
                streak: if dir == Dir::L { streak + 1 } else { 1 },
            });
        }
        if moves.up && (y > 0) {
            states.push(State {
                x,
                y: y - 1,
                loss: loss + losses.data[(yusize - 1) * wusize + xusize],
                dir: Dir::U,
                streak: if dir == Dir::U { streak + 1 } else { 1 },
            });
        }

        // keep lowest losses at top of list
        states.sort_unstable_by(sort_func);
    }
    unreachable!();
}

#[derive(Default)]
struct Moves {
    up: bool,
    down: bool,
    left: bool,
    right: bool
}

fn move_rules_p1(state: &State) -> Moves {
    let straight_ok = state.streak < 3;

    Moves {
        up: (state.dir != Dir::D) && ((state.dir != Dir::U) || straight_ok),
        down: (state.dir != Dir::U) && ((state.dir != Dir::D) || straight_ok),
        left: (state.dir != Dir::R) && ((state.dir != Dir::L) || straight_ok) ,
        right: (state.dir != Dir::L) && ((state.dir != Dir::R) || straight_ok),
    }
}

fn move_rules_p2(state: &State) -> Moves {
let straight_ok = state.streak < 10;
    let turn_ok = state.streak >= 4;
    Moves {
        up: (state.dir == Dir::None)
        || ((state.dir == Dir::U) && straight_ok)
        || ((state.dir == Dir::L) && turn_ok)
        || ((state.dir == Dir::R) && turn_ok),
        down: (state.dir == Dir::None)
        || ((state.dir == Dir::D) && straight_ok)
        || ((state.dir == Dir::L) && turn_ok)
        || ((state.dir == Dir::R) && turn_ok),
        left:   (state.dir == Dir::None)
        ||     ((state.dir == Dir::L) && straight_ok)
        ||     ((state.dir == Dir::U) && turn_ok)
        ||     ((state.dir == Dir::D) && turn_ok),
        right:  (state.dir == Dir::None)
        ||     ((state.dir == Dir::R) && straight_ok)
        ||     ((state.dir == Dir::U) && turn_ok)
        ||     ((state.dir == Dir::D) && turn_ok)
    }
}

fn solution() {
    let input = puzzle_input!();
    let losses = parse(&input);
    println!(
        "{}\n{}",
        solve(&losses, move_rules_p1), // 1155
        solve(&losses, move_rules_p2)  // 1283
    )
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
const EXAMPLE: &str = r"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
";

#[test]
fn p1test() {
    assert_eq!(solve(&parse(EXAMPLE), move_rules_p1), 102);
}

#[test]
fn p2test() {
    assert_eq!(solve(&parse(EXAMPLE), move_rules_p2), 94);
}