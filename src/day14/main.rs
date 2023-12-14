use itertools::Itertools;
use shared::puzzle_input;
use std::{collections::HashSet, rc::Rc};

#[derive(PartialEq, Eq)]
struct State {
    round: HashSet<(i8, i8)>,
    cube: Rc<HashSet<(i8, i8)>>,
    width: i8,
    height: i8,
}

impl State {
    fn cycle(&mut self, n: usize) {
        let (loop_start, loop_len) = self.find_loop(n);
        let remaining = (n - loop_start) % loop_len;
        for _ in 0..remaining {
            self.one_cycle()
        }
    }

    fn compress(&self) -> Box<[(i8, i8)]> {
        self.round
            .iter()
            .cloned()
            .sorted()
            .collect_vec()
            .into_boxed_slice()
    }

    fn find_loop(&mut self, up_to: usize) -> (usize, usize) {
        let mut histories = vec![self.compress()];
        for n in 0..up_to {
            self.one_cycle();
            let history_value = self.compress();

            match histories.iter().position(|s| s == &history_value) {
                None => histories.push(history_value),
                Some(prev) => {
                    return (prev, (n - prev + 1));
                }
            }
        }
        unreachable!();
    }

    fn one_cycle(&mut self) {
        self.tilt_n();
        self.tilt_w();
        self.tilt_s();
        self.tilt_e();
    }

    pub fn score(&self) -> usize {
        self.round
            .iter()
            .map(|(_, y)| self.height as usize - *y as usize)
            .sum()
    }

    fn slide_n(&self, p0: (i8, i8)) -> (i8, i8) {
        let (x, mut y) = p0;
        while (0 < y) && !self.round.contains(&(x, y - 1)) && !self.cube.contains(&(x, y - 1)) {
            y -= 1;
        }
        (x, y)
    }
    fn slide_e(&self, p0: (i8, i8)) -> (i8, i8) {
        let (mut x, y) = p0;
        while (x < self.width - 1)
            && !self.round.contains(&(x + 1, y))
            && !self.cube.contains(&(x + 1, y))
        {
            x += 1;
        }
        (x, y)
    }
    fn slide_s(&self, p0: (i8, i8)) -> (i8, i8) {
        let (x, mut y) = p0;
        while (y < self.height - 1)
            && !self.round.contains(&(x, y + 1))
            && !self.cube.contains(&(x, y + 1))
        {
            y += 1;
        }
        (x, y)
    }
    fn slide_w(&self, p0: (i8, i8)) -> (i8, i8) {
        let (mut x, y) = p0;
        while (0 < x) && !self.round.contains(&(x - 1, y)) && !self.cube.contains(&(x - 1, y)) {
            x -= 1;
        }
        (x, y)
    }
    fn tilt<F>(&mut self, order: impl Iterator<Item = (i8, i8)>, slide: F)
    where
        F: Fn(&Self, (i8, i8)) -> (i8, i8),
    {
        let mut old_round = HashSet::with_capacity(self.round.len());
        std::mem::swap(&mut old_round, &mut self.round);
        order.for_each(|p| {
            if old_round.contains(&p) {
                self.round.remove(&p);
                self.round.insert(slide(self, p));
            }
        });
    }
    pub fn tilt_e(&mut self) {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..h).flat_map(move |y| (0..w).rev().map(move |x| (x, y))),
            Self::slide_e,
        );
    }
    pub fn tilt_n(&mut self) {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..w).flat_map(move |x| (0..h).map(move |y| (x, y))),
            Self::slide_n,
        )
    }
    pub fn tilt_s(&mut self) {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..w).flat_map(move |x| (0..h).rev().map(move |y| (x, y))),
            Self::slide_s,
        )
    }
    pub fn tilt_w(&mut self) {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..h).flat_map(move |y| (0..w).map(move |x| (x, y))),
            Self::slide_w,
        )
    }
}

fn main() {
    let input = puzzle_input!(); // EXAMPLE;
    let chars = input.split_whitespace().enumerate().flat_map(|(y, line)| {
        line.chars()
            .enumerate()
            .map(move |(x, c)| (x as i8, y as i8, c))
    });
    let round: HashSet<(i8, i8)> = chars
        .clone()
        .filter_map(|(x, y, c)| (c == 'O').then_some((x, y)))
        .collect();
    let cube = Rc::new(
        chars
            .filter_map(|(x, y, c)| (c == '#').then_some((x, y)))
            .collect(),
    );
    let init = State {
        round,
        cube,
        width: input.find('\n').unwrap() as i8,
        height: input.chars().filter(|c| *c == '\n').count() as i8 + 1,
    };
    let mut p1_state = State {
        round: init.round.clone(),
        cube: Rc::clone(&init.cube),
        ..init
    };
    p1_state.tilt_n();

    let mut p2_state = init;
    p2_state.cycle(1000000000usize);

    println!("{}", p1_state.score()); // 109385
    println!("{}", p2_state.score()); // 93102
}
