// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy, PartialEq, Eq)]
enum Space { Ash, Rock }

struct Pattern {
	spaces: Vec<Space>,
	stride: usize,
}


fn input_patterns() -> impl Iterator<Item = Pattern> {
	parsing::try_patterns_from_str(include_str!("day13.txt")).map(|r| r.unwrap())
}

fn part1and2_impl<const SMUDGED: bool>(input_patterns: impl Iterator<Item = Pattern>) -> u64 {
	input_patterns.map(|pattern| {
		let height = pattern.spaces.len() / pattern.stride;
		if let Some(column) = (1..pattern.stride).find(|&x| {
			let d = x.min(pattern.stride - x);
			let mut found_smudge = false;
			for y in 0..height {
				let min = y * pattern.stride;
				for d in 0..d {
					let [x_left, x_right] = [x - d - 1, x + d];
					let [left, right] = [min + x_left, min + x_right];
					if pattern.spaces[left] != pattern.spaces[right] {
						if SMUDGED && !found_smudge { found_smudge = true }
						else { return false }
					}
				}
			}
			!SMUDGED || found_smudge
		}) {
			return column as u64
		}

		if let Some(row) = (1..height).find(|&y| {
			let d = y.min(height - y);
			let mut found_smudge = false;
			for x in 0..pattern.stride {
				for d in 0..d {
					let [y_above, y_below] = [y - d - 1, y + d];
					let [above, below]
						= [y_above * pattern.stride + x, y_below * pattern.stride + x];
					if pattern.spaces[above] != pattern.spaces[below] {
						if SMUDGED && !found_smudge { found_smudge = true }
						else { return false }
					}
				}
			}
			!SMUDGED || found_smudge
		}) {
			return row as u64 * 100
		}

		0
	}).sum()
}

pub(crate) fn part1() -> u64 {
	part1and2_impl::<false>(input_patterns())
}

pub(crate) fn part2() -> u64 {
	part1and2_impl::<true>(input_patterns())
}


mod parsing {
	use std::str::FromStr;
	use super::{Pattern, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Space::Ash),
				b'#' => Ok(Space::Rock),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum PatternError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
	}

	impl FromStr for Pattern {
		type Err = PatternError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			s.lines()
				.enumerate()
				.try_fold(Pattern { spaces: Vec::default(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 {
						return Err(PatternError::Format { line: l + 1 }) }

					if acc.stride == 0 { acc.stride = stride }
					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						acc.spaces.push(Space::try_from(byte).map_err(|_|
							PatternError::Space { line: l + 1, column: c + 1, invalid: byte })?);
					}
					Ok(acc)
				})
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct PatternsError { offset: usize, source: PatternError }

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_patterns_from_str(s: &str)
	-> impl Iterator<Item = Result<Pattern, PatternsError>> + '_ {
		s.split("\n\n")
			.enumerate()
			.map(|(i, s)| s.parse()
				.map_err(|e| PatternsError { offset: i, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		#.##..##.
		..#.##.#.
		##......#
		##......#
		..#.##.#.
		..##..##.
		#.#.##.#.
		
		#...##..#
		#....#..#
		..##..###
		#####.##.
		#####.##.
		..##..###
		#....#..#
	"};
	assert_eq!(part1and2_impl::<false>(
		parsing::try_patterns_from_str(INPUT).map(|r| r.unwrap())), 405);
	assert_eq!(part1(), 36015);
	assert_eq!(part1and2_impl::<true>(
		parsing::try_patterns_from_str(INPUT).map(|r| r.unwrap())), 400);
	assert_eq!(part2(), 35335);
}
