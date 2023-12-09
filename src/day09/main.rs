use itertools::Itertools;
use shared::puzzle_input;

fn do_line(line: &str) -> (i64, i64) {
    let data: Vec<i64> = line.split(' ').map(|s| s.parse().unwrap()).collect_vec();
    extrapolate(&data)
}

fn extrapolate(xs: &[i64]) -> (i64, i64) {
    if xs.iter().all(|x| *x == 0) {
        return (0, 0);
    }
    let deltas = xs.iter().tuple_windows().map(|(x, y)| y - x).collect_vec();
    let (fwd, back) = extrapolate(&deltas);
    (xs.last().unwrap() + fwd, xs[0] - back)
}

fn main() {
    let input = puzzle_input!();
    let (p1, p2) = input
        .split('\n')
        .map(do_line)
        .fold((0, 0), |x, y| (x.0 + y.0, x.1 + y.1));
    println!("{p1}\n{p2}"); // 1884768153 1031
}
