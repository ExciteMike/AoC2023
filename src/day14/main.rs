use itertools::Itertools;
use shared::puzzle_input;
use std::{collections::HashSet, rc::Rc};

#[derive(Clone, Default, PartialEq, Eq)]
struct State {
    round: HashSet<(i8, i8)>,
    cube: Rc<HashSet<(i8, i8)>>,
    width: i8,
    height: i8,
}

impl State {
    fn cycle(&self) -> State {
        self.clone().tilt_n().tilt_w().tilt_s().tilt_e()
    }

    fn compress(&self) -> Box<[(i8, i8)]> {
        self.round
            .iter()
            .cloned()
            .sorted()
            .collect_vec()
            .into_boxed_slice()
    }

    fn find_loop(&self) -> (usize, usize) {
        const CYCLES: usize = 1000000000usize;
        let mut s = self.clone();
        let mut histories = vec![s.compress()];
        for n in 0..CYCLES {
            s = s.cycle();
            let history_value = s.compress();

            match histories.iter().position(|s| s == &history_value) {
                None => histories.push(history_value),
                Some(prev) => {
                    let remainder = (CYCLES - prev) % (n - prev + 1);
                    return (prev, remainder);
                }
            }
        }
        unreachable!();
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
    fn tilt<F>(mut self, order: impl Iterator<Item = (i8, i8)>, slide: F) -> State
    where
        F: Fn(&Self, (i8, i8)) -> (i8, i8),
    {
        let mut old_round = HashSet::with_capacity(self.round.len());
        std::mem::swap(&mut old_round, &mut self.round);
        order.fold(self, |mut s, p| {
            if old_round.contains(&p) {
                s.round.insert(slide(&s, p));
            }
            s
        })
    }
    pub fn tilt_e(self) -> State {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..h).flat_map(move |y| (0..w).rev().map(move |x| (x, y))),
            Self::slide_e,
        )
    }
    pub fn tilt_n(self) -> State {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..w).flat_map(move |x| (0..h).map(move |y| (x, y))),
            Self::slide_n,
        )
    }
    pub fn tilt_s(self) -> State {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..w).flat_map(move |x| (0..h).rev().map(move |y| (x, y))),
            Self::slide_s,
        )
    }
    pub fn tilt_w(self) -> State {
        let (w, h) = (self.width, self.height);
        self.tilt(
            (0..h).flat_map(move |y| (0..w).map(move |x| (x, y))),
            Self::slide_w,
        )
    }
}

fn main() {
    let input = puzzle_input!(); // EXAMPLE
    let chars = input.split_whitespace().enumerate().flat_map(|(y, line)| {
        line.chars()
            .enumerate()
            .map(move |(x, c)| (x as i8, y as i8, c))
    });
    let round = chars
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

    let (offset, remainder) = init.find_loop();
    println!(
        "{}\n{}",
        init.clone().tilt_n().score(),
        itertools::iterate(init, State::cycle)
            .nth(offset + remainder)
            .unwrap()
            .score()
    ); // 109385 93102
}
