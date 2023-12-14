use itertools::Itertools;
use shared::puzzle_input;
use std::collections::HashSet;

const EXAMPLE: &str = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";

#[derive(Clone, Default, PartialEq, Eq)]
struct State {
    round: HashSet<(u8, u8)>,
    cube: HashSet<(u8, u8)>
}

fn tilt_north(instate: &State, width:u8, height:u8) -> State {
    let mut outstate = State { round: HashSet::default(), cube: instate.cube.clone() };
    for x in 0..width {
        for y in 0..height {
            let p0 = (x, y);
            if instate.round.contains(&p0) {
                outstate.round.insert(slide_north(p0, &outstate));
            }
        }
    }
    outstate
}

fn tilt_west(instate: &State, width:u8, height:u8) -> State {
    let mut outstate = State { round: HashSet::default(), cube: instate.cube.clone() };
    for y in 0..height {
        for x in 0..width {
            let p0 = (x, y);
            if instate.round.contains(&p0) {
                outstate.round.insert(slide_west(p0, &outstate));
            }
        }
    }
    outstate
}

fn tilt_south(instate: &State, width:u8, height:u8) -> State {
    let mut outstate = State { round: HashSet::default(), cube: instate.cube.clone() };
    for x in 0..width {
        for y in (0..height).rev() {
            let p0 = (x, y);
            if instate.round.contains(&p0) {
                outstate.round.insert(slide_south(p0, &outstate, height));
            }
        }
    }
    outstate
}

fn tilt_east(instate: &State, width:u8, height:u8) -> State {
    let mut outstate = State { round: HashSet::default(), cube: instate.cube.clone() };
    for y in 0..height {
        for x in (0..width).rev() {
            let p0 = (x, y);
            if instate.round.contains(&p0) {
                outstate.round.insert(slide_east(p0, &outstate, width));
            }
        }
    }
    outstate
}

fn slide_north(p0: (u8, u8), obstacles: &State) -> (u8, u8) {
    let (x0, y0) = p0;
    let mut y1 = y0;
    while (y1 > 0) && !obstacles.round.contains(&(x0, y1-1)) && !obstacles.cube.contains(&(x0, y1-1)) {
        y1 -= 1;
    }
    (x0, y1)
}

fn slide_west(p0: (u8, u8), obstacles: &State) -> (u8, u8) {
    let (x0, y0) = p0;
    let mut x1 = x0;
    while (x1 > 0) && !obstacles.round.contains(&(x1-1, y0)) && !obstacles.cube.contains(&(x1-1, y0)) {
        x1 -= 1;
    }
    (x1, y0)
}

fn slide_south(p0: (u8, u8), obstacles: &State, height: u8) -> (u8, u8) {
    let (x0, y0) = p0;
    let mut y1 = y0;
    while (y1 < height-1) && !obstacles.round.contains(&(x0, y1+1)) && !obstacles.cube.contains(&(x0, y1+1)) {
        y1 += 1;
    }
    (x0, y1)
}

fn slide_east(p0: (u8, u8), obstacles: &State, width: u8) -> (u8, u8) {
    let (x0, y0) = p0;
    let mut x1 = x0;
    while (x1 < width-1) && !obstacles.round.contains(&(x1+1, y0)) && !obstacles.cube.contains(&(x1+1, y0)) {
        x1 += 1;
    }
    (x1, y0)
}

fn score(state: &State, height: u8) -> usize {
    state.round.iter().map(|(_, y)|height as usize - *y as usize).sum()
}

fn compress(s:&State) -> Box<[(u8,u8)]> {
    s.round.iter().cloned().sorted().collect_vec().into_boxed_slice()
}

fn cycle(s: &State, width:u8, height:u8) -> State {
    let mut s = s.clone();
    s = tilt_north(&s, width, height);
    s = tilt_west(&s, width, height);
    s = tilt_south(&s, width, height);
    tilt_east(&s, width, height)
}

fn find_loop(s:&State, width:u8, height:u8) -> (usize, usize) {
    const CYCLES:usize = 1000000000usize;
    let mut s = s.clone();
    let mut histories = vec![compress(&s)];
    for n in 0..CYCLES {
        s = cycle(&s, width, height);
        let history_value = compress(&s);

        match histories.iter().position(|s|s==&history_value) {
            None => histories.push(history_value),
            Some(prev) => {
                let remainder = (CYCLES - prev) % (n - prev + 1);
                println!("loop detected @ {} cycles. offset={prev}. remainder = {remainder}", n+1);
                return (prev, remainder);
            }
        }
    }
    unreachable!();
}

fn main() {
    let input =
        puzzle_input!();
        //EXAMPLE;
    let width = input.find('\n').unwrap() as u8;
    let height = input.chars().filter(|c|*c=='\n').count() as u8 + 1;
    let mut state = State::default();
    for (y, line) in input.split_whitespace().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let p = (x as u8, y as u8);
            match c {
                '#' => { state.cube.insert(p); },
                'O' => { state.round.insert(p); },
                _ => (),
            }
        }
    }
    let p1_state = tilt_north(&state, width, height);
    let mut p2_state = state.clone();
    let (offset, remainder) = find_loop(&p2_state, width, height);
    for _ in 0..(offset + remainder) {
        p2_state = cycle(&p2_state, width, height);
    }

    println!("{}", score(&p1_state, height)); // 109385
    println!("{}", score(&p2_state, height)); // 93102
}
