use std::collections::{HashMap, HashSet};

use shared::puzzle_input;

// vert in left most tightly connected to right.
fn most_tightly_connected_vert<'a>(
    left: &mut HashSet<&'a str>,
    right: &mut HashSet<&'a str>,
    edges: &HashMap<&'a str, HashSet<&'a str>>,
) -> &'a str {
    let mut tightest_vert = "";
    let mut connections = 0;
    for vert in left.iter().copied() {
        let mut n_connections = 0;
        for other in edges.get(vert).unwrap().iter().copied() {
            if right.contains(other) {
                n_connections += 1;
            }
        }
        if n_connections > connections {
            connections = n_connections;
            tightest_vert = vert;
        }
    }
    tightest_vert
}

fn count_cuts<'a>(
    left: &HashSet<&'a str>,
    right: &HashSet<&'a str>,
    edges: &HashMap<&'a str, HashSet<&'a str>>,
) -> usize {
    let mut total = 0;
    for vert in left.iter().copied() {
        for other in edges.get(vert).unwrap().iter().copied() {
            if right.contains(other) {
                total += 1;
            }
        }
    }
    total
}

fn p1(input: &str) -> usize {
    let mut m = HashMap::<&str, HashSet<&str>>::new();
    for line in input.trim().split('\n') {
        let (a, bs) = line.split_once(": ").unwrap();
        for b in bs.split_ascii_whitespace() {
            m.entry(a).or_default().insert(b);
            m.entry(b).or_default().insert(a);
        }
    }
    let mut left = m.keys().copied().collect::<HashSet<_>>();
    let arbitrary_vert = m.keys().copied().next().unwrap();
    let mut right = HashSet::from([arbitrary_vert]);
    while left.len() > 1 {
        let v = most_tightly_connected_vert(&mut left, &mut right, &m);
        if !v.is_empty() {
            left.remove(v);
            right.insert(v);
        }
        let cuts = count_cuts(&left, &right, &m);
        if cuts == 3 {
            return left.len() * right.len();
        }
    }

    unreachable!();
}

fn main() {
    let input = puzzle_input!();
    println!("{}\n", p1(&input)); // 592171
}

#[cfg(test)]
const EXAMPLE: &str = r"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
";

#[test]
fn p1_example() {
    assert_eq!(p1(EXAMPLE), 54);
}
