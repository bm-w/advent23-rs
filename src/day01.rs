// Copyright (c) 2023 Bastiaan Marinus van de Weerd


fn input() -> &'static str {
	include_str!("day01.txt")
}


fn part1_impl(input: &str) -> Result<u64, (usize, &str)> {
	input
		.lines()
		.enumerate()
		.try_fold(0, |acc, (l, line)| {
			let s = line.trim_matches(|c: char| !c.is_ascii_digit());
			if s.is_empty() { return Err((l, line)) }
			let mut bytes = s.bytes();
			let first = bytes.next().unwrap();
			let last = bytes.last().unwrap_or(first);
			Ok(acc + (first - b'0') as u64 * 10 + (last - b'0') as u64)
		})
}

pub(crate) fn part1() -> u64 {
	part1_impl(input()).unwrap()
}


const DIGIT_STRS: &[&str]
	= &["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

fn part2_impl(input: &str) -> Result<u64, (usize, &str)> {
	input
		.lines()
		.enumerate()
		.try_fold(0, |acc, (l, line)| {
			let mut first = line
				.find(|c: char| c.is_ascii_digit())
				.map(|c| (c, 1, (line.as_bytes()[c] - b'0') as u64));
			for (i, digit_str) in DIGIT_STRS.iter().enumerate() {
				let (c, n) = first.map(|(c, n, _)| (c, n)).unwrap_or((line.len(), 0));
				if let Some(ci) = line[..c + n].find(digit_str) {
					if ci < c { first = Some((ci, digit_str.len(), (i + 1) as u64)); }
				}
			}
			let Some((c, n, first)) = first else { return Err((l, line)) };

			let mut last = line[c..]
				.rfind(|c: char| c.is_ascii_digit())
				.map(|ci| (c + ci, 1, (line.as_bytes()[c + ci] - b'0') as u64));
			for (i, digit_str) in DIGIT_STRS.iter().enumerate() {
				let (c, n) = last.map(|(c, n, _)| (c, n)).unwrap_or((c, n));
				if let Some(ci) = line[c..].rfind(digit_str) {
					let ci = c + ci;
					let ni = digit_str.len();
					if ci + ni > c + n { last = Some((ci, ni, (i + 1) as u64)); }
				}
			}
			let Some((_, _, last)) = last else { return Err((l, line)) };

			Ok(acc + first * 10 + last)
		})
}

pub(crate) fn part2() -> u64 {
	part2_impl(input()).unwrap()
}


#[test]
fn tests() {
	const INPUT_PART1: &str = indoc::indoc! {"
		1abc2
		pqr3stu8vwx
		a1b2c3d4e5f
		treb7uchet
	"};

	const INPUT_PART2: &str = indoc::indoc! {"
		two1nine
		eightwothree
		abcone2threexyz
		xtwone3four
		4nineeightseven2
		zoneight234
		7pqrstsixteen
	"};

	assert_eq!(part1_impl(INPUT_PART1), Ok(142));
	assert_eq!(part1(), 54601);
	assert_eq!(part2_impl("xtwone3four"), Ok(24));
	assert_eq!(part2_impl(INPUT_PART2), Ok(281));
	assert_eq!(part2(), 54078);
}
