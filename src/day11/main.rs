use std::collections::HashSet;

use itertools::Itertools;
use shared::puzzle_input;

fn expansion_remapping(max: i64, nonemptys: &HashSet<i64>, scale: i64) -> Box<[i64]> {
    let mut corrected = 0i64;
    (0i64..=max)
        .map(|i| {
            if !nonemptys.contains(&i) {
                corrected += scale;
            } else {
                corrected += 1;
            }
            corrected
        })
        .collect_vec()
        .into_boxed_slice()
}
fn expand_galaxies(
    galaxies: &[(i64, i64)],
    nonempty_rows: &HashSet<i64>,
    nonempty_cols: &HashSet<i64>,
    scale: i64,
) -> Box<[(i64, i64)]> {
    let row_max = nonempty_rows.iter().max().unwrap();
    let col_max = nonempty_cols.iter().max().unwrap();
    let remap_y = expansion_remapping(*row_max, nonempty_rows, scale);
    let remap_x = expansion_remapping(*col_max, nonempty_cols, scale);
    galaxies
        .iter()
        .map(|(x, y)| (remap_x[*x as usize], remap_y[*y as usize]))
        .collect_vec()
        .into_boxed_slice()
}
fn score_galaxies(galaxies: &[(i64, i64)]) -> i64 {
    galaxies
        .iter()
        .cartesian_product(galaxies.iter())
        .map(|((x1, y1), (x2, y2))| (x2 - x1).abs() + (y2 - y1).abs())
        .sum::<i64>()
        >> 1
}

fn main() {
    let (input, p2_scale) = (puzzle_input!(), 1_000_000);
    // let (input, p2_scale) = (EXAMPLE.to_string(), 100);
    let galaxies = input
        .split('\n')
        .enumerate()
        .flat_map(|(y, s)| {
            s.chars().enumerate().filter_map(move |(x, c)| {
                if c == '#' {
                    Some((x as i64, y as i64))
                } else {
                    None
                }
            })
        })
        .collect_vec();
    let nonempty_rows: HashSet<i64> = galaxies.iter().map(|(_, y)| *y).collect();
    let nonempty_cols: HashSet<i64> = galaxies.iter().map(|(x, _)| *x).collect();
    let p1 = score_galaxies(&expand_galaxies(
        &galaxies,
        &nonempty_rows,
        &nonempty_cols,
        2,
    ));
    let p2 = score_galaxies(&expand_galaxies(
        &galaxies,
        &nonempty_rows,
        &nonempty_cols,
        p2_scale,
    ));
    println!("{p1}\n{p2}"); // 10292708 790194712336
}
