// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[cfg_attr(test, derive(Debug))]
enum Space { Empty, Mirror { forward: bool }, Splitter { hor: bool } }

struct Cavern {
	spaces: Vec<Space>,
	stride: usize,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy)]
enum Dir { Up = 0, Down = 1, Left = 2, Right = 3 }

impl Cavern {
	fn trace_beam(&self, mut pos: usize, mut dir: Dir, energized_spaces: &mut [[bool; 4]]) {
		let s = self.stride;
		macro_rules! can_move_up { () => { pos >= s } }
		macro_rules! can_move_down { () => { pos < self.spaces.len() - s } }
		macro_rules! can_move_left { () => { pos % s > 0 } }
		macro_rules! can_move_right { () => { (pos + 1) % s != 0 } }
		macro_rules! if_or_else_return { ($check:ident, $block:tt) => {
			{ if !$check!() { return }; $block } } } 
		macro_rules! maybe_move_up { () => { if_or_else_return!(can_move_up,
			{ pos -= s; dir = Dir::Up }) } }
		macro_rules! maybe_move_down { () => { if_or_else_return!(can_move_down,
			{ pos += s; dir = Dir::Down }) } }
		macro_rules! maybe_move_left { () => { if_or_else_return!(can_move_left,
			{ pos -= 1; dir = Dir::Left }) } }
		macro_rules! maybe_move_right { () => { if_or_else_return!(can_move_right,
			{ pos += 1; dir = Dir::Right }) } }

		loop {
			if std::mem::replace(&mut energized_spaces[pos][dir as usize], true) { return }
			match (dir, &self.spaces[pos]) {
				(Dir::Up, Space::Empty | Space::Splitter { hor: false }) => maybe_move_up!(),
				(Dir::Down, Space::Empty | Space::Splitter { hor: false }) => maybe_move_down!(),
				(Dir::Left, Space::Empty | Space::Splitter { hor: true }) => maybe_move_left!(),
				(Dir::Right, Space::Empty | Space::Splitter { hor: true }) => maybe_move_right!(),
				(Dir::Up, Space::Mirror { forward: true }) => maybe_move_right!(),
				(Dir::Up, Space::Mirror { forward: false }) => maybe_move_left!(),
				(Dir::Down, Space::Mirror { forward: true }) => maybe_move_left!(),
				(Dir::Down, Space::Mirror { forward: false }) => maybe_move_right!(),
				(Dir::Left, Space::Mirror { forward: true }) => maybe_move_down!(),
				(Dir::Left, Space::Mirror { forward: false }) => maybe_move_up!(),
				(Dir::Right, Space::Mirror { forward: true }) => maybe_move_up!(),
				(Dir::Right, Space::Mirror { forward: false }) => maybe_move_down!(),
				(Dir::Up, Space::Splitter { .. }) => {
					if can_move_left!() { self.trace_beam(pos - 1, Dir::Left, energized_spaces); }
					maybe_move_right!()
				}
				(Dir::Down, Space::Splitter { .. }) => {
					if can_move_right!() { self.trace_beam(pos + 1, Dir::Right, energized_spaces); }
					maybe_move_left!()
				}
				(Dir::Left, Space::Splitter { .. }) => {
					if can_move_down!() { self.trace_beam(pos + s, Dir::Down, energized_spaces); }
					maybe_move_up!()
				}
				(Dir::Right, Space::Splitter { .. }) => {
					if can_move_up!() { self.trace_beam(pos - s, Dir::Up, energized_spaces); }
					maybe_move_down!()
				}
			}
		}
	}

	fn count_energized_spaces(&self, start: usize, dir: Dir) -> usize {
		let mut energized_spaces = vec![[false; 4]; self.spaces.len()];
		self.trace_beam(start, dir, &mut energized_spaces);
		energized_spaces.into_iter().filter(|&[u, d, l, r]| u || d || l || r).count()
	}
}


fn input_cavern() -> Cavern {
	include_str!("day16.txt").parse().unwrap()
}


fn part1_impl(input_cavern: Cavern) -> usize {
	input_cavern.count_energized_spaces(0, Dir::Right)
}

pub(crate) fn part1() -> usize {
	part1_impl(input_cavern())
}


fn part2_impl(input_cavern: Cavern) -> usize {
	let [n, s] = [input_cavern.spaces.len(), input_cavern.stride];
	let h = n / s;

	let top_down = (0..input_cavern.stride).map(|pos| (pos, Dir::Down));
	let bottom_up = (0..input_cavern.stride).map(|d| (n - s + d, Dir::Up));
	let left_right = (0..h).map(|y| (y * s, Dir::Right));
	let right_left = (0..h).map(|y| ((y + 1) * s - 1, Dir::Left));

	top_down.chain(bottom_up).chain(left_right).chain(right_left)
		.map(|(pos, dir)| input_cavern.count_energized_spaces(pos, dir))
		.max().unwrap()
}

pub(crate) fn part2() -> usize {
	part2_impl(input_cavern())
}


mod parsing {
	use std::str::FromStr;
	use super::{Cavern, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Space::Empty),
				b'/' => Ok(Space::Mirror { forward: true }),
				b'\\' => Ok(Space::Mirror { forward: false }),
				b'-' => Ok(Space::Splitter { hor: true }),
				b'|' => Ok(Space::Splitter { hor: false }),
				_ => Err(())
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum CavernError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
	}

	impl FromStr for Cavern {
		type Err = CavernError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			s.lines()
				.enumerate()
				.try_fold(Cavern { spaces: Vec::default(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 {
						return Err(CavernError::Format { line: l + 1 }) }

					if acc.stride == 0 { acc.stride = stride }
					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						acc.spaces.push(Space::try_from(byte).map_err(|_|
							CavernError::Space { line: l + 1, column: c + 1, invalid: byte })?);
					}
					Ok(acc)
				})
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {r#"
		.|...\....
		|.-.\.....
		.....|-...
		........|.
		..........
		.........\
		..../.\\..
		.-.-/..|..
		.|....-|.\
		..//.|....
	"#};
	assert_eq!(part1_impl(INPUT.parse::<Cavern>().unwrap()), 46);
	assert_eq!(part1(), 7434);
	assert_eq!(part2_impl(INPUT.parse::<Cavern>().unwrap()), 51);
	assert_eq!(part2(), 8183);
}
