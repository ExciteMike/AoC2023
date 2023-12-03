use itertools::Itertools;
use shared::puzzle_input;

fn mk_parse_table(s: &str) -> Box<[(usize, &str)]> {
    s.split_ascii_whitespace()
        .enumerate()
        .map(|(i, prefix)| (i % 10, prefix))
        .collect_vec()
        .into_boxed_slice()
}

const P1_PREFICES: &str = "0 1 2 3 4 5 6 7 8 9";
const P2_PREFICES: &str = "0 1 2 3 4 5 6 7 8 9 zero one two three four five six seven eight nine";

fn read_p2(line: &str, parse_table: &[(usize, &'static str)]) -> usize {
    let mut v: Vec<usize> = vec![];
    let mut it = 0..line.len();
    while let Some(i) = it.next() {
        for (digit_value, prefix) in parse_table {
            if line[i..].starts_with(prefix) {
                v.push(*digit_value);
                if prefix.len() > 1 {
                    it.nth(prefix.len() - 2); // skip prefix
                }
                break;
            }
        }
    }
    v[0] * 10 + v[v.len() - 1]
}

fn main() {
    let input = puzzle_input!();
    let lines = input.split_ascii_whitespace();
    let table1 = mk_parse_table(P1_PREFICES);
    let table2 = mk_parse_table(P2_PREFICES);
    let read_p1 = |s| read_p2(s, &table1);
    let read_p2 = |s| read_p2(s, &table2);
    let p1: usize = lines.clone().map(read_p1).sum();
    let p2: usize = lines.map(read_p2).sum();
    println!("{p1}\n{p2}");
}
