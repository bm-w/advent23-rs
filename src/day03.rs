// Copyright (c) 2023 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
enum Space {
	Open,
	Digit { ascii: u8 },
	Symbol { ascii: u8 },
}

struct Grid {
	spaces: Vec<Space>,
	stride: usize,
}

impl Grid {
	fn adjacent_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Space)> {
		let above = pos >= self.stride;
		let left = pos % self.stride > 0;
		let right = pos % self.stride < self.stride - 1;
		let below = pos < self.spaces.len() - self.stride;

		macro_rules! adj_space { ( $dx:literal, $dy:literal ) => { {
			#[allow(clippy::neg_multiply)]
			let pos = (pos as isize + $dx + $dy * (self.stride as isize)) as usize;
			(pos, self.spaces[pos])
		} }; }

		let mut spaces = [(usize::MAX, Space::Open); 8];
		if above {
			if left { spaces[0] = adj_space!(-1, -1); }
			spaces[1] = adj_space!(0, -1);
			if right { spaces[2] = adj_space!(1, -1); }
		}
		if left { spaces[3] = adj_space!(-1, 0); }
		if right { spaces[4] = adj_space!(1, 0); }
		if below {
			if left { spaces[5] = adj_space!(-1, 1); }
			spaces[6] = adj_space!(0, 1);
			if right { spaces[7] = adj_space!(1, 1); }
		}
		spaces.into_iter().filter(|(pos, _)| *pos < usize::MAX)
	}

	fn part_number(&self, mut pos: usize) -> Option<(usize, u64)> {
		if !matches!(self.spaces[pos], Space::Digit { .. }) { return None }
		while pos % self.stride > 0 && matches!(self.spaces[pos - 1], Space::Digit { .. }) {
			pos -= 1;
		}
		let left_pos = pos;
		let mut number = 0;
		while let Space::Digit { ascii } = self.spaces[pos] {
			number = 10 * number + (ascii - b'0') as u64;
			pos += 1;
		}
		Some((left_pos, number))
	}
}


fn input_grid() -> Grid {
	include_str!("day03.txt").parse::<Grid>().unwrap()

}

fn part1_impl(input_grid: Grid) -> u64 {
	let mut part_numbers = std::collections::HashMap::<usize, u64>::new();

	let height = input_grid.spaces.len() / input_grid.stride;
	for y in 0..height {
		let iy0 = input_grid.stride * y;
		for x in 0..input_grid.stride {
			let pos = iy0 + x;
			if matches!(input_grid.spaces[pos], Space::Symbol { .. }) {
				for (adj_pos, _) in input_grid.adjacent_spaces(pos) {
					if let Some((pos, part_number)) = input_grid.part_number(adj_pos) {
						part_numbers.insert(pos, part_number);
					}
				}
			}
		}
	}

	part_numbers.values().copied().sum()
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_grid())
}


fn part2_impl(input_grid: Grid) -> u64 {
	use std::collections::HashMap;

	let mut acc = 0;
	let mut part_numbers = HashMap::<usize, u64>::with_capacity(8);

	let height = input_grid.spaces.len() / input_grid.stride;
	for y in 0..height {
		let iy0 = input_grid.stride * y;
		for x in 0..input_grid.stride {
			let pos = iy0 + x;
			if matches!(input_grid.spaces[pos], Space::Symbol { ascii: b'*' }) {
				for (adj_pos, _) in input_grid.adjacent_spaces(pos) {
					if let Some((pos, part_number)) = input_grid.part_number(adj_pos) {
						part_numbers.insert(pos, part_number);
					}
				}
				let mut drain = part_numbers.drain();
				if let (Some((_, first)), Some((_, second)), None) 
					= (drain.next(), drain.next(), drain.next()) {
					acc += first * second;
				} else {
					drain.for_each(|_| {})
				}
			}
		}
	}

	acc
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_grid())
}


mod parsing {
	use std::str::FromStr;
	use super::{Grid, Space};

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum GridError {
		Format { line: usize },
		Space { line: usize, column: usize, ascii: u8 },
	}

	impl FromStr for Grid {
		type Err = GridError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use GridError as E;
			s.lines()
				.enumerate()
				.try_fold(Grid { spaces: Vec::new(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 {
						return Err(E::Format { line: l + 1 })}

					if acc.stride == 0 { acc.stride = stride; }

					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						acc.spaces.push(match byte {
							b'.' => Space::Open,
							b'0'..=b'9' => Space::Digit { ascii: byte },
							symbol if !symbol.is_ascii_control()
								&& !symbol.is_ascii_alphanumeric()
								=> Space::Symbol { ascii: symbol },
							invalid => return Err(
								E::Space { line: l + 1, column: c + 1, ascii: invalid })
						})
					}

					Ok(acc)
				})
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		467..114..
		...*......
		..35..633.
		......#...
		617*......
		.....+.58.
		..592.....
		......755.
		...$.*....
		.664.598..
	"};
	assert_eq!(part1_impl(INPUT.parse::<Grid>().unwrap()), 4361);
	assert_eq!(part1(), 509115);
	assert_eq!(part2_impl(INPUT.parse::<Grid>().unwrap()), 467835);
	assert_eq!(part2(), 75220503);
}
