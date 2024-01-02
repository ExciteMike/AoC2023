use std::ops::Range;

use itertools::Itertools;
use num::{
    bigint::{BigInt, ToBigInt},
    rational::BigRational,
    traits::{ToPrimitive, Zero},
    Signed,
};
use shared::puzzle_input;

type Triple = (BigRational, BigRational, BigRational);
type HailStone = (Triple, Triple);

fn parse_line(s: &str) -> HailStone {
    let (p, v) = s.split_once(" @ ").unwrap();
    (parse_triple(p), parse_triple(v))
}

fn parse_triple(s: &str) -> Triple {
    s.split(", ")
        .map(|s| s.trim().parse::<BigRational>().unwrap())
        .collect_tuple()
        .unwrap()
}

fn intersection(stone_a: &HailStone, stone_b: &HailStone) -> Option<Triple> {
    assert!(!stone_a.1 .0.is_zero());
    let m1 = stone_a.1 .1.to_f64().unwrap() / stone_a.1 .0.to_f64().unwrap();
    let b1 = stone_a.0 .1.to_f64().unwrap() - m1 * stone_a.0 .0.to_f64().unwrap();
    assert!(!stone_b.1 .0.is_zero());
    let m2 = stone_b.1 .1.to_f64().unwrap() / stone_b.1 .0.to_f64().unwrap();
    let b2 = stone_b.0 .1.to_f64().unwrap() - m2 * stone_b.0 .0.to_f64().unwrap();
    let b = b2 - b1;
    let m = m2 - m1;
    if m.abs() <= f64::EPSILON {
        // parallel
        return None;
    }
    let x = -b / m;
    let y1 = m1 * x + b1;
    let t1 = (x - stone_a.0 .0.to_f64().unwrap()) / stone_a.1 .0.to_f64().unwrap();
    if t1 < 0.0 {
        // paths crossed in the past for A
        return None;
    }
    let t2 = (x - stone_b.0 .0.to_f64().unwrap()) / stone_b.1 .0.to_f64().unwrap();
    if t2 < 0.0 {
        // paths crossed in the past for B
        return None;
    }
    let z1 = stone_a.0 .2.to_f64().unwrap() + t1 * stone_a.1 .2.to_f64().unwrap();
    Some((
        x.to_bigint().unwrap().into(),
        y1.to_bigint().unwrap().into(),
        z1.to_bigint().unwrap().into(),
    ))
}

fn intersections_within(
    data: &[HailStone],
    xrange: Range<BigRational>,
    yrange: Range<BigRational>,
) -> usize {
    let mut total = 0;
    for (i, a) in data.iter().enumerate() {
        for b in data[i + 1..].iter() {
            if let Some((x, y, _)) = intersection(a, b) {
                if xrange.contains(&x) && yrange.contains(&y) {
                    total += 1;
                }
            }
        }
    }
    total
}

fn add(a: &Triple, b: &Triple) -> Triple {
    (&a.0 + &b.0, &a.1 + &b.1, &a.2 + &b.2)
}
fn sub(a: &Triple, b: &Triple) -> Triple {
    (&a.0 - &b.0, &a.1 - &b.1, &a.2 - &b.2)
}
fn cross(a: &Triple, b: &Triple) -> Triple {
    (
        &a.1 * &b.2 - &a.2 * &b.1,
        &a.2 * &b.0 - &a.0 * &b.2,
        &a.0 * &b.1 - &a.1 * &b.0,
    )
}
fn dot(a: &Triple, b: &Triple) -> BigRational {
    &a.0 * &b.0 + &a.1 * &b.1 + &a.2 * &b.2
}
/// = a + t * b
fn scale_and_add(a: &Triple, t: &BigRational, b: &Triple) -> Triple {
    (&a.0 + t * &b.0, &a.1 + t * &b.1, &a.2 + t * &b.2)
}

fn p2(data: &[HailStone]) -> BigRational {
    let a = &data[0];
    let b = &data[1];
    let c = &data[2];

    // transform to a space relative to a
    let b2 = (sub(&b.0, &a.0), sub(&b.1, &a.1));
    let c2 = (sub(&c.0, &a.0), sub(&c.1, &a.1));
    let future_b = add(&b2.0, &b2.1);

    // rock line must lie on plane determined by origin and two points on b's line
    let plane_b = cross(&b2.0, &future_b);

    // find where c crosses that plane
    let num = dot(&plane_b, &c2.0);
    let denom = dot(&plane_b, &c2.1);
    let tc = (num / denom).abs();
    let collision_point_c2 = scale_and_add(&c2.0, &tc, &c2.1);

    // rock must go through origin and that point. where does that meet b?
    let rock_direction = collision_point_c2.clone();
    let num = &rock_direction.0 * &b2.0 .1 - &rock_direction.1 * &b2.0 .0;
    let denom = &rock_direction.1 * &b2.1 .0 - &rock_direction.0 * &b2.1 .1;
    let tb = (&num / &denom).abs();
    let collision_point_b2 = scale_and_add(&b2.0, &tb, &b2.1);
    let dt = &tc - &tb;
    let rock_vel = (
        (&collision_point_c2.0 - &collision_point_b2.0) / &dt,
        (&collision_point_c2.1 - &collision_point_b2.1) / &dt,
        (&collision_point_c2.2 - &collision_point_b2.2) / &dt,
    );
    let rock2 = scale_and_add(&collision_point_b2, &(-tb), &rock_vel);
    let rock = add(&rock2, &a.0);
    rock.0 + rock.1 + rock.2
}

fn frac<T>(n: T) -> BigRational
where
    num::BigInt: From<T>,
{
    BigRational::from(BigInt::from(n))
}

fn main() {
    let input = puzzle_input!();
    let data = input.trim().split('\n').map(parse_line).collect_vec();
    let range = frac(200000000000000i64)..frac(400000000000001i64);
    let p1 = intersections_within(&data, range.clone(), range);
    println!("{p1}"); // 21785
    println!("{}", p2(&data)); // 554668916217145
}

#[cfg(test)]
const EXAMPLE: &str = r"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
";

#[test]
fn p1_example() {
    let data = EXAMPLE.trim().split('\n').map(parse_line).collect_vec();
    let n = intersections_within(&data, frac(7)..frac(27), frac(7)..frac(27));
    assert_eq!(n, 2);
}

#[test]
fn p2_example() {
    let data = EXAMPLE.trim().split('\n').map(parse_line).collect_vec();
    assert_eq!(p2(&data), frac(47));
}
