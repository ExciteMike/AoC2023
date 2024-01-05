use rustc_hash::{FxHashMap, FxHashSet};
use shared::puzzle_input;

// vert in left most tightly connected to right.
fn most_tightly_connected_vert(
    left: &mut FxHashSet<u32>,
    right: &mut FxHashSet<u32>,
    edges: &FxHashMap<u32, Vec<u32>>,
) -> u32 {
    let mut tightest_vert = 0;
    let mut connections = 0;
    for vert in left.iter().copied() {
        let mut n_connections = 0;
        for other in edges.get(&vert).unwrap().iter() {
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

fn count_cuts(
    left: &FxHashSet<u32>,
    right: &FxHashSet<u32>,
    edges: &FxHashMap<u32, Vec<u32>>,
) -> usize {
    let mut total = 0;
    for vert in left.iter() {
        for other in edges.get(vert).unwrap().iter() {
            if right.contains(other) {
                total += 1;
            }
        }
    }
    total
}

fn solution(input: &str) -> usize {
    let mut m: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
    m.reserve(1300);
    for line in input.trim().split('\n') {
        let (a, bs) = line.split_once(": ").unwrap();
        let a = u32::from_str_radix(a, 36).unwrap();
        for b in bs.split_ascii_whitespace() {
            let b = u32::from_str_radix(b, 36).unwrap();
            m.entry(a).or_insert_with(||Vec::with_capacity(9)).push(b);
            m.entry(b).or_insert_with(||Vec::with_capacity(9)).push(a);
        }
    }
    let mut left = m.keys().copied().collect::<FxHashSet<_>>();
    let arbitrary_vert = m.keys().copied().next().unwrap();
    let mut right = FxHashSet::default();
    right.reserve(left.len());
    right.insert(arbitrary_vert);
    left.remove(&arbitrary_vert);
    while count_cuts(&left, &right, &m) != 3 {
        let v = most_tightly_connected_vert(&mut left, &mut right, &m);
        debug_assert!(0 != v);
        left.remove(&v);
        right.insert(v);
    }

    left.len() * right.len()
}

fn main() {
    let input = puzzle_input!();

    const N_TIMES: u32 = 100;
    use std::time::Instant;
    let now = Instant::now();
    let mut ans = 0;
    for _ in 0..N_TIMES {
        ans = solution(&input);
    }

    println!("{ans}\n"); // 592171

    let elapsed = now.elapsed();
    eprintln!("Completed in average of {:.2?}", elapsed / N_TIMES);
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
    assert_eq!(solution(EXAMPLE), 54);
}
