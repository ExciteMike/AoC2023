use std::collections::{HashSet, VecDeque, HashMap};

use itertools::Itertools;
use shared::puzzle_input;

enum Module<'a> {
    Broa(&'a str, Box<[&'a str]>),
    Flip(&'a str, Box<[&'a str]>, bool),
    Conj(&'a str, Box<[&'a str]>, HashSet<&'a str>),
}
impl<'a> Module<'a> {
    pub fn rcv(&mut self, pulse: bool, from: &'a str, mut q: &mut VecDeque<(&'a str, bool, &'a str)>) {
        match self {
            Module::Broa(name, dsts) => {
                send(q, dsts, pulse, name);
            },
            Module::Flip(name, dsts, on) => {
                if pulse {
                    //eprintln!("{name} ignore high pulse");
                } else {
                    if *on {
                        send(&mut q, dsts, false, name);
                    } else {
                        send(&mut q, dsts, true, name);
                    }
                    *on = !*on;
                }
            },
            Module::Conj(name, dsts, memory) => {
                if pulse {
                    memory.remove(from);
                } else {
                    memory.insert(from);
                }
                send(&mut q, dsts, !memory.is_empty(), name);
            },
        }
    }
}

fn parse_line(s: &str) -> (&str, Module) {
    let (name, dsts) = s.split_once(" -> ").unwrap();
    let dsts = dsts.split(", ").collect_vec().into_boxed_slice();
    let m = match name.chars().next().unwrap() {
        'b' => Module::Broa(name, dsts),
        '%' => Module::Flip(&name[1..], dsts, false),
        '&' => Module::Conj(&name[1..], dsts, HashSet::new()),
        _ => unreachable!()
    };
    (&name[1..], m)
}

fn parse<'a>(s: &'a str) -> (HashMap<&'a str, Module<'a>>, Box<[&'a str]>) {
    let mut m: HashMap<&'a str, Module<'a>> = s.trim().split('\n').map(parse_line).collect();
    let mut r: HashMap<&str, Vec<&str>> = HashMap::new();
    for (name, module) in &m {
        match module {
            Module::Broa(_, dsts) |
            Module::Flip(_, dsts, _) |
            Module::Conj(_, dsts, _) => {
                for &dst in &**dsts {
                    r.entry(dst).or_default().push(name);
                }
            },
        }
    }
    for (name, module) in &mut m {
        match module {
            Module::Conj(_, _, memory) => {
                memory.extend(r.get(name).unwrap().iter());
            },
            _ => ()
        }
    }
    let srcs = r.get("rx").unwrap().iter().flat_map(|s|r.get(s).unwrap().iter().copied()).collect_vec().into_boxed_slice();
    (m, srcs)
}

fn send<'a>(q: &mut VecDeque<(&'a str, bool, &'a str)>, dsts: &[&'a str], pulse: bool, from: &'a str) {
    q.extend(dsts.iter().copied().map(|dst|(dst, pulse, from)))
}

fn p1<'a>(m : &mut HashMap<&'a str, Module<'a>>, times: usize) -> usize {
    let mut lows = 0;
    let mut highs = 0;
    for i in 0..times {
        let mut q = VecDeque::from([("roadcaster", false, "button")]);
        while let Some((dst, pulse, from)) = q.pop_front() {
            if pulse {
                highs += 1;
            } else {
                lows += 1;
            }
            if let Some(module) = m.get_mut(dst) {
                if !pulse && dst == "rx" {
                    println!("part 2: {}", i+1)
                }
                module.rcv(pulse, from, &mut q);
            }
        }
    }
    lows * highs
}

fn p2<'a>(m : &mut HashMap<&'a str, Module<'a>>, targets: &[&'a str]) -> usize {
    let mut presses = vec![0usize ; targets.len()];
    for i in 1001..10_000 {
        let mut q = VecDeque::from([("roadcaster", false, "button")]);
        while let Some((dst, pulse, from)) = q.pop_front() {
            if let Some(module) = m.get_mut(dst) {
                if !pulse && dst == "rx" {
                    return i;
                }
                if pulse {
                    if let Some(j) = targets.iter().position(|s|s==&from) {
                        if presses[j] == 0 {
                            presses[j] = i;
                        }
                    }
                }
                module.rcv(pulse, from, &mut q);
            }
        }
    }
    presses.iter().product()
}

fn solution() {
    let input = puzzle_input!();
    let (mut m, p2_targets) = parse(&input);
    println!("{}", p1(&mut m, 1000)); // 737679780
    println!("{}", p2(&mut m, &p2_targets)); // 227411378431763
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
const EXAMPLE_1: &str = r"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
";

#[cfg(test)]
const EXAMPLE_2: &str = r"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
";

#[test]
fn p1_example() {
    assert_eq!(p1(&mut parse(EXAMPLE_1).0, 1), 8 * 4);
    assert_eq!(p1(&mut parse(EXAMPLE_2).0, 1), 4 * 4);
    assert_eq!(p1(&mut parse(EXAMPLE_2).0, 2), (4+4) * (4+2));
    assert_eq!(p1(&mut parse(EXAMPLE_2).0, 3), (4+4+5) * (4+2+3));
    assert_eq!(p1(&mut parse(EXAMPLE_2).0, 4), (4+4+5+4) * (4+2+3+2));
    assert_eq!(p1(&mut parse(EXAMPLE_1).0, 1000), 32000000);
    assert_eq!(p1(&mut parse(EXAMPLE_2).0, 1000), 11687500);
}