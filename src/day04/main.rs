use std::collections::HashSet;

use shared::puzzle_input;

fn check_card(line: &str) -> (usize, usize) {
    let parse_set =
        |s: &str| -> HashSet<usize> { s.split(' ').filter_map(|s| s.parse().ok()).collect() };
    let (winning, have) = line.split_once(':').unwrap().1.split_once('|').unwrap();
    match parse_set(winning).intersection(&parse_set(have)).count() {
        0 => (0, 0),
        x => (2usize.pow((x - 1) as u32), x),
    }
}

fn p2(card_values: &[usize]) -> usize {
    let n = card_values.len();
    let mut multipliers = vec![1; n];
    for (i, card_value) in card_values.iter().enumerate() {
        if 0 < *card_value {
            let n_copies = multipliers[i];
            for x in multipliers.iter_mut().skip(i + 1).take(*card_value) {
                *x += n_copies;
            }
        }
    }
    multipliers.iter().sum()
}

fn main() {
    let input = puzzle_input!();
    let (values, n_matches): (Vec<_>, Vec<_>) = input.split('\n').map(check_card).unzip();
    println!("{}\n{}", values.iter().sum::<usize>(), p2(&n_matches)); // 33950 14814534
}
