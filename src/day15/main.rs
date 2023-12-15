use itertools::Itertools;
use shared::puzzle_input;

fn hash_step(acc: usize, c: char) -> usize {
    ((acc + (c as usize)) * 17) % 256
}

fn p1_hash(s: &str) -> usize {
    s.chars().fold(0usize, hash_step)
}

fn handle_step<'a>(m: &mut Vec<Vec<(&'a str, usize)>>, step: &'a str) {
    if step.is_empty() { return; }
    if step.ends_with('-') {
        remove(m, &step[..step.len()-1])
    } else {
        let i = step.find('=').unwrap();
        let (key, rest) = step.split_at(i);
        let focal_length = rest.chars().nth(1).unwrap().to_digit(10).unwrap() as usize;
        add(m, key, focal_length)
    }
}

fn add<'a>(m: &mut Vec<Vec<(&'a str, usize)>>, key: &'a str, focal_length: usize) {
    let box_key = p1_hash(&key) as usize;
    let box_value = m.get_mut(box_key).unwrap();
    match box_value.iter().position(|x|x.0==key) {
        None => {box_value.push((key, focal_length));},
        Some(i) => {box_value[i] = (key, focal_length);},
    }
}

fn remove(m: &mut Vec<Vec<(&str, usize)>>, key: &str) {
    let box_key = p1_hash(&key) as usize;
    let box_value = m.get_mut(box_key).unwrap();
    match box_value.iter().position(|x|x.0==key) {
        None => {},
        Some(i) => {box_value.remove(i);},
    }
}

fn score_hashmap(m: &Vec<Vec<(&str, usize)>>) -> usize {
    m.iter().enumerate().map(|(i, v)|(i+1) * score_box(v)).sum()
}

fn score_box(v: &[(&str, usize)]) -> usize {
    v.iter().enumerate().map(|(i, x)|(i+1) * x.1).sum()
}

fn to_hashmap<'a>(steps: &[&'a str]) -> Vec<Vec<(&'a str, usize)>> {
    let mut m: Vec<Vec<(&str, usize)>> = vec![Vec::new(); 256];
    for step in steps {
        handle_step(&mut m, step);
    }
    m
}

fn p2(steps: &[&str]) -> usize {
    score_hashmap(&to_hashmap(steps))
}

fn main() {    
    let input = puzzle_input!();
    let steps = input.split(',').collect_vec();
    println!("{}", steps.iter().map(|s|p1_hash(s) as usize).sum::<usize>()); // 506869
    println!("{}", p2(&steps)); // 271384
}

#[test]
fn p1test() {
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
        .split(',')
        .map(p1_hash)
        .zip([30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231])
        .for_each(|(a, b)|assert_eq!(a, b, "got {a}. expected {b}."));
}

#[test]
fn p2test() {
    let steps = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".split(',').collect_vec();
    let m = to_hashmap(&steps);
    // only have things in 0 and 3
    assert_eq!(m.iter().enumerate().filter(|(_,v)|!v.is_empty()).count(), 2);
    // box 0: rn 1, cm 2
    assert_eq!(m[0][0], ("rn", 1));
    assert_eq!(m[0][1], ("cm", 2));
    // box 3: ot 7, ab 5, pc 6
    assert_eq!(m[3][0], ("ot", 7));
    assert_eq!(m[3][1], ("ab", 5));
    assert_eq!(m[3][2], ("pc", 6));
    
    assert_eq!(p2(&steps), 145);
}