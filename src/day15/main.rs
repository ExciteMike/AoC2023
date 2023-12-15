use shared::puzzle_input;

trait EnuMapSum<T> {
    fn enumapsum<S, F>(&self, f: F) -> S
    where
        Self: Sized,
        S: std::iter::Sum<S>,
        F: FnMut((usize, &T)) -> S;
}

impl<T> EnuMapSum<T> for &[T] {
    fn enumapsum<S, F>(&self, f: F) -> S
    where
        Self: Sized,
        S: std::iter::Sum<S>,
        F: FnMut((usize, &T)) -> S,
    {
        self.iter().enumerate().map(f).sum()
    }
}

fn hash_step(acc: usize, c: char) -> usize {
    ((acc + (c as usize)) * 17) % 256
}

fn p1_hash(s: &str) -> usize {
    s.chars().fold(0usize, hash_step)
}

fn handle_step<'a>(mut m: Vec<Vec<(&'a str, usize)>>, step: &'a str) -> Vec<Vec<(&'a str, usize)>> {
    if step.is_empty() {
        m
    } else if let Some(key) = step.strip_suffix('-') {
        remove(&mut m, key);
        m
    } else {
        let i = step.find('=').unwrap();
        let focal_length = step[i + 1..].chars().next().unwrap().to_digit(10).unwrap() as usize;
        add(&mut m, &step[..i], focal_length);
        m
    }
}

fn add<'a>(m: &mut [Vec<(&'a str, usize)>], key: &'a str, focal_length: usize) {
    let box_key = p1_hash(key);
    let box_value = m.get_mut(box_key).unwrap();
    match box_value.iter().position(|x| x.0 == key) {
        None => {
            box_value.push((key, focal_length));
        }
        Some(i) => {
            box_value[i] = (key, focal_length);
        }
    }
}

fn remove(m: &mut [Vec<(&str, usize)>], key: &str) {
    let box_key = p1_hash(key);
    let box_value = m.get_mut(box_key).unwrap();
    match box_value.iter().position(|x| x.0 == key) {
        None => {}
        Some(i) => {
            box_value.remove(i);
        }
    }
}

fn score_hashmap(m: &[Vec<(&str, usize)>]) -> usize {
    m.enumapsum(|(i, v)| (i + 1) * score_box(v))
}

fn score_box(v: &[(&str, usize)]) -> usize {
    v.enumapsum(|(i, x)| (i + 1) * x.1)
}

fn to_hashmap(s: &str) -> Vec<Vec<(&str, usize)>> {
    s.split(',').fold(vec![Vec::new(); 256], handle_step)
}

fn p1(s: &str) -> usize {
    s.split(',').map(p1_hash).sum::<usize>()
}

fn p2(s: &str) -> usize {
    score_hashmap(&to_hashmap(s))
}

fn main() {
    let input = puzzle_input!();
    println!("{}", p1(&input)); // 506869
    println!("{}", p2(&input)); // 271384
}

#[test]
fn p1test() {
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
        .split(',')
        .map(p1_hash)
        .zip([30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231])
        .for_each(|(a, b)| assert_eq!(a, b, "got {a}. expected {b}."));
}

#[test]
fn p2test() {
    use itertools::Itertools;
    let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
    let m = to_hashmap(input);
    // only have things in 0 and 3
    assert_eq!(
        m.iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i) })
            .collect_tuple(),
        Some((0, 3))
    );
    // box 0: rn 1, cm 2
    assert_eq!(m[0][0], ("rn", 1));
    assert_eq!(m[0][1], ("cm", 2));
    // box 3: ot 7, ab 5, pc 6
    assert_eq!(m[3][0], ("ot", 7));
    assert_eq!(m[3][1], ("ab", 5));
    assert_eq!(m[3][2], ("pc", 6));

    assert_eq!(p2(input), 145);
}
