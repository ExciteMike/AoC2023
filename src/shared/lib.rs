#[macro_export]
macro_rules! tokenize {
    ($input:expr, $($i:expr),+) => {{
        use unicode_segmentation::UnicodeSegmentation;
        let words: Vec<_> = $input.split_word_bounds().filter(|s| !s.trim().is_empty()).collect();
        (
            $(words[$i]),+
        )
    }}
}

#[macro_export]
macro_rules! words {
    ($input:expr, $($i:expr),+) => {{
        use unicode_segmentation::UnicodeSegmentation;
        let words: Vec<_> = $input.unicode_words().filter(|s| !s.trim().is_empty()).collect();
        (
            $(words[$i]),+
        )
    }}
}

#[macro_export]
macro_rules! map_words {
    ($input:expr, $f:expr, $($i:expr),+) => {{
        use unicode_segmentation::UnicodeSegmentation;
        let words: Vec<_> = $input.unicode_words().filter(|s| !s.trim().is_empty()).collect();
        (
            $($f(words[$i])),+
        )
    }}
}

#[macro_export]
macro_rules! puzzle_input {
    () => {{
        const YEAR: i32 = 2023;
        let mod_name = module_path!().rsplit("::").next().unwrap();
        let day_num = (&mod_name[3..5])
            .parse::<u32>()
            .expect(&format!("something went wrong parsing \"{}\"", mod_name));
        let x = shared::puzzle_input(YEAR, day_num).replace("\r\n", "\n");
        x.trim_end().to_string()
    }};
}

pub fn puzzle_input(year: i32, day_num: u32) -> String {
    use std::io::Write;
    let input_path = format!("puzzle_input/day{:02}", day_num);
    if std::fs::metadata(&input_path).is_ok() {
        eprintln!("puzzle input already downloaded for day {}", day_num);
    } else {
        use chrono::prelude::{TimeZone, Utc};
        if let std::cmp::Ordering::Less =
            Utc::now().cmp(&Utc.with_ymd_and_hms(year, 12, day_num, 0, 0, 0).unwrap())
        {
            panic!("too early for fetching puzzle input!");
        }
        let url = format!("https://adventofcode.com/{}/day/{}/input", year, day_num);
        eprintln!("downloading {:?}", url);
        let session_cookie =
            std::fs::read_to_string("session_cookie").expect("error obtaining session cookie");
        let contents = reqwest::blocking::Client::new()
            .get(&url)
            .header("Cookie", format!("session={}", session_cookie))
            .send()
            .expect("error sending puzzle input request")
            .text()
            .expect("error converting response to text");
        eprintln!("done");
        write!(
            std::fs::File::create(&input_path).expect("error creating puzzle input file"),
            "{}",
            contents
        )
        .expect("error writing puzzle input file");
    }
    std::fs::read_to_string(&input_path).expect("error opening puzzle input")
}

pub mod string_windows {
    use std::num::NonZeroUsize;

    #[inline]
    fn ceil_char_boundary(s: &str, index: usize) -> usize {
        assert!(index <= s.len());

        let upper_bound = Ord::min(index + 4, s.len());
        s.as_bytes()[index..upper_bound]
            .iter()
            .position(|b| (*b as i8) >= -0x40)
            .map_or(upper_bound, |pos| pos + index)
    }

    pub trait Windows<'a> {
        fn windows(&'a self, size: usize) -> StringWindows<'a>;
    }

    pub enum StringWindows<'a> {
        Done,
        InProgress {
            begin: usize,
            end: usize,
            src: &'a str,
        },
    }

    impl<'a> StringWindows<'a> {
        #[inline]
        pub fn new(src: &'a str, size: NonZeroUsize) -> Self {
            if src.len() < size.into() {
                return Self::Done;
            }
            let end = src
                .char_indices()
                .nth(size.into())
                .unwrap_or((src.len(), ' '))
                .0;
            Self::InProgress { begin: 0, end, src }
        }
    }

    impl<'a> Iterator for StringWindows<'a> {
        type Item = &'a str;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            match self {
                Self::Done => None,
                Self::InProgress { begin, end, src } => {
                    let s = &src[*begin..*end];
                    if *end < src.len() {
                        *begin = ceil_char_boundary(src, *begin + 1);
                        *end = ceil_char_boundary(src, *end + 1);
                    } else {
                        *self = Self::Done;
                    }
                    Some(s)
                }
            }
        }

        #[inline]
        fn size_hint(&self) -> (usize, Option<usize>) {
            match self {
                Self::Done => (0, Some(0)),
                Self::InProgress { end, src, .. } => {
                    let n = src.len() - end + 1;
                    (n, Some(n))
                }
            }
        }

        #[inline]
        fn count(self) -> usize {
            match self {
                Self::Done => 0,
                Self::InProgress { end, src, .. } => src.len() - end + 1,
            }
        }
    }

    impl<'a> Windows<'a> for &'a str {
        fn windows(&'a self, size: usize) -> StringWindows {
            let size = NonZeroUsize::new(size).expect("Attempted to take windows of size zero");
            StringWindows::<'a>::new(self, size)
        }
    }
}
