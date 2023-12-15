use std::collections::HashMap;

use itertools::Itertools;
use shared::puzzle_input;

fn parse(line: &str) -> (String, Box<[i64]>) {
    let (springs, groups) = line.split_once(' ').unwrap();
    (
        springs.to_owned(),
        groups
            .split(',')
            .map(|s| s.parse::<i64>().unwrap())
            .collect_vec()
            .into_boxed_slice(),
    )
}

fn unfold_s(s: &str) -> String {
    format!("{s}?{s}?{s}?{s}?{s}")
}

fn unfold_gs(gs: &[i64]) -> Box<[i64]> {
    let mut v = Vec::with_capacity(5 * gs.len());
    for _ in 0..5 {
        v.extend_from_slice(gs);
    }
    v.into_boxed_slice()
}

fn count_ways<'a>(
    memo: &'a mut HashMap<(String, Vec<i64>, i64), i64>,
    s: &'a str,
    gs: &'a [i64],
    group_in_progress: i64,
) -> i64 {
    let key = (s.to_owned(), gs.to_owned(), group_in_progress);
    if memo.contains_key(&key) {
        *memo.get(&key).unwrap()
    } else {
        let ways = count_ways_(memo, s, gs, group_in_progress);
        memo.insert(key, ways);
        ways
    }
}

macro_rules! p {
    ($($arg:tt)*) => {
        //println!($($arg)*)
    };
}

fn count_ways_<'a>(
    memo: &'a mut HashMap<(String, Vec<i64>, i64), i64>,
    s: &'a str,
    gs: &'a [i64],
    group_in_progress: i64,
) -> i64 {
    p!("count_ways_ s={s} gs={gs:?} gip={group_in_progress}");
    if s.is_empty() {
        // no springs left
        if gs.is_empty() {
            // satisfied!
            p!("  A: 1");
            1
        } else if group_in_progress >= gs[0] {
            p!("  B");
            count_ways(memo, s, &gs[1..], 0)            
        } else {
            p!("C: 0");
            0
        }
    } else {
        // still has springs
        let spring = s.chars().next().unwrap();
        if gs.is_empty() {
            if spring == '#' {
                // can't match this
                p!("D: 0");
                0
            } else {
                p!("E");
                count_ways(memo, &s[1..], gs, group_in_progress)
            }
        } else {
            let group = gs[0];
            if group_in_progress >= group {
                if (spring == '.') || (spring == '?') {
                    // finished a group
                    p!("F");
                    count_ways(memo, &s[1..], &gs[1..], 0)
                } else {
                    // too many '#'s
                    p!("G: 0");
                    0
                }
            } else if group_in_progress > 0 {
                if spring == '.' {
                    // in-progress group doesn't fit
                    p!("H: 0");
                    0
                } else {
                    // continue group
                    p!("I");
                    count_ways(memo, &s[1..], gs, group_in_progress+1)
                }
            } else {
                p!("J");
                // no group started
                match spring {
                    '.' => count_ways(memo, &s[1..], gs, 0),
                    '#' => count_ways(memo, &s[1..], gs, 1),
                    '?' => count_ways(memo, &s[1..], gs, 0) + count_ways(memo, &s[1..], gs, 1),
                    _ => panic!(),
                }
            }
        }
    }
}

fn solution() {
    let input = puzzle_input!();
    let lines = input.split('\n').map(parse).collect_vec();
    let mut memo: HashMap<(String, Vec<i64>, i64), i64> = HashMap::with_capacity(1024);
    let p1 = lines
        .iter()
        .map(|(s, gs)| count_ways(&mut memo, s, gs, 0))
        .sum::<i64>();
    let p2 = lines.iter().map(|(s, gs)|count_ways(&mut memo, &unfold_s(s), &unfold_gs(gs), 0)).sum::<i64>();
    println!("{p1}\n{p2}"); // 7260 1909291258644
}

fn main() {
    use std::time::Instant;
    let now = Instant::now();
    const N: u32 = 10;
    for _ in 0..10 {
        solution();
    }
    let elapsed = now.elapsed();
    eprintln!("Completed in average of {:.2?}", elapsed / N);
}

#[test]
fn a() {
    let mut memo: HashMap<(String, Vec<i64>, i64), i64> = HashMap::with_capacity(1024);
    assert_eq!(count_ways(&mut memo, "#.#.###", &[1,1,3], 0), 1);
    assert_eq!(count_ways(&mut memo, ".??..??...?##.", &[1,1,3], 0), 4);
    assert_eq!(count_ways(&mut memo, "?#?#?#?#?#?#?#?", &[1,3,1,6], 0), 1);
    assert_eq!(count_ways(&mut memo, "????.#...#...", &[4,1,1], 0), 1);
    assert_eq!(count_ways(&mut memo, "????.######..#####.", &[1,6,5], 0), 4);
    assert_eq!(count_ways(&mut memo, "?###????????", &[3,2,1], 0), 10);
}