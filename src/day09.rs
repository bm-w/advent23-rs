// Copyright (c) 2023 Bastiaan Marinus van de Weerd


fn input_histories() -> impl Iterator<Item = impl Iterator<Item = i64>> {
	parsing::try_histories_from_str(include_str!("day09.txt")).map(|h| h.map(|r| r.unwrap()))
}


fn part1and2_impl(
	input_histories: impl Iterator<Item = impl Iterator<Item = i64>>,
	extrapolate: fn(usize, &[i64], usize, usize, i64) -> i64,
) -> i64 {
	input_histories.map(|history| {
		let mut history = history.collect::<Vec<_>>();
		let history_len = history.len();
		let mut levels = 1;
		loop {
			let level_len = history_len - levels;
			let level_offset = history.len();
			history.extend(std::iter::repeat(0).take(level_len));
			levels += 1;

			let mut all_zeroes = true;
			let (history, level) = history.split_at_mut(level_offset);
			let history = &mut history[level_offset - level_len - 1..];
			for i in 0..level_len {
				level[i] = history[i + 1] - history[i];
				if level[i] != 0 { all_zeroes = false }
			}
			if all_zeroes { break }
		}
		let mut extrapolations = vec![0; levels];
		for level in (1..levels).rev() {
			let level_offset = level * history_len - (level * level - level) / 2;
			extrapolations[level - 1] = extrapolate(history_len, &history,
				level, level_offset, extrapolations[level]);
		}
		extrapolations[0]
	}).sum()
}


fn part1_impl(input_histories: impl Iterator<Item = impl Iterator<Item = i64>>) -> i64 {
	part1and2_impl(input_histories, |_, history, _, level_offset, prev_extrapolation|
		history[level_offset - 1] + prev_extrapolation)
}

pub(crate) fn part1() -> i64 {
	part1_impl(input_histories())
}


fn part2_impl(input_histories: impl Iterator<Item = impl Iterator<Item = i64>>) -> i64 {
	part1and2_impl(input_histories, |history_len, history, level, level_offset, prev_extrapolation| {
		let level_len = history_len - level;
		history[level_offset - level_len - 1] - prev_extrapolation
	})
}

pub(crate) fn part2() -> i64 {
	part2_impl(input_histories())
}


mod parsing {
	use std::num::ParseIntError;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct HistoriesError {
		line: usize,
		offset: usize,
		source: ParseIntError,
	}

	pub(super) fn try_histories_from_str(s: &str)
	-> impl Iterator<Item = impl Iterator<Item = Result<i64, HistoriesError>> + '_> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.split(' ')
				.enumerate()
				.map(move |(i, val)| val.parse()
					.map_err(|e| HistoriesError { line: l + 1, offset: i, source: e })))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		0 3 6 9 12 15
		1 3 6 10 15 21
		10 13 16 21 30 45
	"};
	assert_eq!(part1_impl(parsing::try_histories_from_str(INPUT).map(|h| h.map(|r| r.unwrap()))), 114);
	assert_eq!(part1(), 1798691765);
	assert_eq!(part2_impl(parsing::try_histories_from_str(INPUT).map(|h| h.map(|r| r.unwrap()))), 2);
	assert_eq!(part2(), 1104);
}
