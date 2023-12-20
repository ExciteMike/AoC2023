use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use itertools::Itertools;
use shared::puzzle_input;

#[derive(Debug)]
struct Rule<'a> {
    category: char,
    ordering: Ordering,
    amount: u32,
    dst_wf: &'a str,
}
type Rules<'a> = Box<[Rule<'a>]>;
type Map<'a> = HashMap<&'a str, Rules<'a>>;
type Part = (u32, u32, u32, u32);
type Parts = Box<[Part]>;

fn parse_rule(s: &str) -> Rule {
    if let Some((cond, conseq)) = s.split_once(':') {
        let category;
        let ordering;
        let amount;
        if let Some((cat, amnt)) = cond.split_once('<') {
            category = cat.chars().next().unwrap();
            ordering = Ordering::Less;
            amount = amnt.parse::<u32>().unwrap();
        } else if let Some((cat, amnt)) = cond.split_once('>') {
            category = cat.chars().next().unwrap();
            ordering = Ordering::Greater;
            amount = amnt.parse::<u32>().unwrap();
        } else {
            unreachable!();
        }
        Rule {
            category,
            ordering,
            amount,
            dst_wf: conseq,
        }
    } else {
        Rule {
            category: 'x',
            ordering: Ordering::Less,
            amount: 4001,
            dst_wf: s,
        }
    }
}

fn parse_workflow(s: &str) -> (&str, Box<[Rule]>) {
    let (name, rules_str) = s[..s.len() - 1].split_once('{').unwrap();
    let rules = rules_str
        .split(',')
        .map(parse_rule)
        .collect_vec()
        .into_boxed_slice();
    (name, rules)
}

fn replace<'a, 'b>(m: &'a mut Map<'b>, old_name: &'b str, new_name: &'b str) {
    for rules in m.values_mut() {
        for rule in &mut **rules {
            if rule.dst_wf == old_name {
                rule.dst_wf = new_name;
            }
        }
    }
}

fn parse_workflows(s: &str) -> Map {
    let mut m: Map = s.split('\n').map(parse_workflow).collect();

    // simplify workflows
    let mut did_anything = true;
    while did_anything {
       did_anything = false;
       let to_remove = m
           .iter()
           .filter_map(|(name, rules)| {
               rules
                   .iter()
                   .map(|rule| rule.dst_wf)
                   .all_equal()
                   .then_some((*name, rules[0].dst_wf))
           })
           .collect_vec();
       for (discard, keep) in to_remove {
           did_anything = true;
           replace(&mut m, discard, keep);
           m.remove(discard);
       }
    }
    m
}

fn parse_part(s: &str) -> Part {
    let mut ratings = s[1..s.len() - 1].split(',');
    (
        ratings.next().unwrap()[2..].parse().unwrap(),
        ratings.next().unwrap()[2..].parse().unwrap(),
        ratings.next().unwrap()[2..].parse().unwrap(),
        ratings.next().unwrap()[2..].parse().unwrap(),
    )
}
fn parse_parts(s: &str) -> Parts {
    s.split_whitespace()
        .map(parse_part)
        .collect_vec()
        .into_boxed_slice()
}

fn parse(s: &str) -> (Map, Parts) {
    let (workflows, ratings) = s.split_once("\n\n").unwrap();
    (parse_workflows(workflows), parse_parts(ratings))
}

fn test_part(m: &Map, p: &Part) -> u32 {
    let mut rules = m.get("in").unwrap().iter();
    while let Some(rule) = rules.next() {
        let value = match &rule.category {
            'x' => p.0,
            'm' => p.1,
            'a' => p.2,
            's' => p.3,
            _ => unreachable!(),
        };
        if value.cmp(&rule.amount) == rule.ordering {
            match rule.dst_wf {
                "A" => {
                    return p.0 + p.1 + p.2 + p.3;
                }
                "R" => {
                    return 0;
                }
                _ => {
                    rules = m.get(rule.dst_wf).unwrap().iter();
                }
            }
        }
    }
    return 0;
}

fn p2<'a>(m: &'a Map) -> usize {
    let mut xv = HashSet::from([1, 4001]);
    let mut mv = HashSet::from([1, 4001]);
    let mut av = HashSet::from([1, 4001]);
    let mut sv = HashSet::from([1, 4001]);
    for rules in m.values() {
        for rule in &**rules {
            let amnt = match rule.ordering {
                Ordering::Less => rule.amount,
                _ => rule.amount + 1
            };
            match rule.category {
                'x' => {xv.insert(amnt);},
                'm' => {mv.insert(amnt);},
                'a' => {av.insert(amnt);},
                's' => {sv.insert(amnt);},
                _ => unreachable!(),
            }
        }
    }
    let xv = xv.into_iter().sorted().collect_vec();
    let mv = mv.into_iter().sorted().collect_vec();
    let av = av.into_iter().sorted().collect_vec();
    let sv = sv.into_iter().sorted().collect_vec();
    let mut total = 0;
    for (i, (x1, x2)) in xv.iter().tuple_windows().enumerate() {
        for (m1, m2) in mv.iter().tuple_windows() {
            for (a1, a2) in av.iter().tuple_windows() {
                for (s1, s2) in sv.iter().tuple_windows() {
                    if 0 < test_part(m, &(*x1, *m1, *a1, *s1)) {
                        total += (x2-x1) as usize * (m2-m1) as usize * (a2-a1) as usize * (s2-s1) as usize;
                    }
                }
            }
        }
        eprintln!("{:5.1}%", ((i+1) as f64) / (xv.len() as f64) * 100f64)
    }
    total
}

fn solution() {
    let input = puzzle_input!();
    let (m, parts) = parse(&input);
    let p1 = parts.iter().map(|part| test_part(&m, &part)).sum::<u32>();
    println!("{p1}\n{}", p2(&m)); // 353046 125355665599537
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
const EXAMPLE: &str = r"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
";

#[test]
fn p1_example() {
    let (m, parts) = parse(EXAMPLE);
    assert_eq!(
        parts.iter().map(|part| test_part(&m, &part)).sum::<u32>(),
        19114
    );
}
#[test]
fn p1_test() {
    let input = puzzle_input!();
    let (m, parts) = parse(&input);
    assert_eq!(
        parts.iter().map(|part| test_part(&m, &part)).sum::<u32>(),
        353046
    );
}


#[test]
fn p2_example() {
    assert_eq!(p2(&parse("in{x<101:foo,R}\nfoo{m>3001:R,A}\n\n{x=1,m=1,a=1,s=1}").0), 100usize * 4000 * 4000 * 3001);

    const TEST0: &str = r"in{a<2000:foo,A}\n\
    foo{x>1111:bar,s<99:baz,R}\n\
    bar{s>2222:R,R}\n\
    baz{x>3333:A,m>3333:R,R}\n\
    \n\
    {x=787,m=2655,a=1222,s=2876}
    ";
    let m = parse(TEST0).0;
    assert!(test_part(&m, &(1, 3334, 2000, 1)) > 0);
    assert!(test_part(&m, &(1, 3334, 2000, 99)) > 0);

    assert_eq!(p2(&m), 128_064_000_000_000usize);

    let (m, _) = parse(EXAMPLE);
    assert_eq!(p2(&m), 167409079868000usize);
}
#[test]
fn p2_test() {
    let input = puzzle_input!();
    let m = parse(&input).0;
    assert_eq!(p2(&m), 0usize);
}
