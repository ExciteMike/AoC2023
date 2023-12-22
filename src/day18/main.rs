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

fn boundary_points(instructions: &[(char, isize)]) -> isize {
    instructions.iter().map(|(_, n)| n).sum()
}

fn to_coords(instructions: &[(char, isize)]) -> Vec<(isize, isize)> {
    let mut p = (0, 0);
    let mut v = vec![];
    for (dir, amnt) in instructions.iter() {
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

fn score(v: &[(char, isize)]) -> isize {
    let coords = to_coords(v);
    // shoelace formula for area
    let area = coords
        .iter()
        .circular_tuple_windows()
        .map(|(a, b, c)| b.0 * (c.1 - a.1))
        .sum::<isize>()
        .abs()
        / 2;
    let n_boundary_points = boundary_points(v);
    // Pick's formula gives the number of interior points
    let n_interior_points = area - (n_boundary_points / 2) + 1;
    n_boundary_points + n_interior_points
}

fn solution() {
    let input = puzzle_input!();
    let v: Vec<(char, isize)> = input.split('\n').filter_map(parse_line).collect_vec();
    println!("{}", score(&v)); // 41019
    let v: Vec<(char, isize)> = input.split('\n').filter_map(parse_line2).collect_vec();
    println!("{}", score(&v)); // 96116995735219
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
    let v = EXAMPLE.split('\n').filter_map(parse_line).collect_vec();
    assert_eq!(boundary_points(&v), 38);
    assert_eq!(score(&v), 62);
    let input = puzzle_input!();
    let v: Vec<(char, isize)> = input.split('\n').filter_map(parse_line).collect_vec();
    assert_eq!(score(&v), 41019);
}

#[test]
fn p2test() {
    let v = EXAMPLE.split('\n').filter_map(parse_line2).collect_vec();
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
