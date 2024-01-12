// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Dir { Up, Down, Left, Right }

struct Step {
	dir: Dir,
	dist: usize,
	color: [u8; 3],
}


fn input_steps() -> impl Iterator<Item = Step> {
	parsing::try_steps_from_str(include_str!("day18.txt")).map(|r| r.unwrap())
}


fn part1_impl(input_steps: impl Iterator<Item = Step>) -> usize {
	use std::collections::{HashSet, VecDeque};

	let mut path = HashSet::from([[0_isize, 0]]);
	let [mut hor_range, mut vert_range] = [0..1, 0..1];
	let mut pos = [0, 0];

	for step in input_steps {
		macro_rules! r#move { ( pos[$xy:literal] $op:tt $d:expr ) => { {
			for _ in 1..=$d {
				pos[$xy] $op 1;
				path.insert(pos);
			}
		} } }
		macro_rules! update_range {
			( $range:ident.$bound:ident, $minmax:ident, $xy:literal $(, +$add:literal )?) => {
				$range.$bound = $range.$bound.$minmax(pos[$xy] $( + $add )?)
			}
		}
		match (step.dir, step.dist as isize) {
			(Dir::Up, d) => { r#move!(pos[1] -= d); update_range!(vert_range.start, min, 1) }
			(Dir::Down, d) => { r#move!(pos[1] += d); update_range!(vert_range.end, max, 1, +1) }
			(Dir::Left, d) => { r#move!(pos[0] -= d); update_range!(hor_range.start, min, 0) }
			(Dir::Right, d) => { r#move!(pos[0] += d); update_range!(hor_range.end, max, 0, +1) }
		}
	}

	let ext_hor_range = hor_range.start - 1..hor_range.end + 1;
	let ext_vert_range = vert_range.start - 1..vert_range.end + 1;

	let mut queue = VecDeque::from([[ext_hor_range.start, ext_vert_range.start]]);
	let mut outside = HashSet::new();
	while let Some(pos) = queue.pop_front() {
		if !outside.insert(pos) { continue }
		for (idx, step) in [(1_usize, -1), (1, 1), (0, -1), (0, 1)] {
			let mut adj_pos = pos;
			adj_pos[idx] += step;
			if [&ext_hor_range, &ext_vert_range][idx].contains(&adj_pos[idx])
				&& !path.contains(&adj_pos) {
				queue.push_back(adj_pos)
			}
		}
	}

	#[cfg(never)]
	{
		for y in ext_vert_range.clone() {
			if y > ext_vert_range.start { println!() }
			for x in ext_hor_range.clone() {
				print!("{}", if outside.contains(&[x, y]) { '.' }
					else if path.contains(&[x, y]) { '#'}
					else { 'o' })
			}
		}
		println!();
	}

	ext_hor_range.len() * ext_vert_range.len() - outside.len()
}

pub(crate) fn part1() -> usize {
	part1_impl(input_steps())
}


fn part2_impl<const DECODE_COLOR: bool>(input_steps: impl Iterator<Item = Step>) -> usize {
	use std::collections::{HashSet, VecDeque};
	use itertools::Itertools as _;

	let [mut x_stops, mut y_stops] = std::array::from_fn(|_| HashSet::from([0_isize, 1]));

	let mut path = Vec::from([[0_isize, 0]]);
	let mut pos = [0, 0];

	for step in input_steps {
		macro_rules! r#move { ( pos[$xy:literal] $op:tt $d:expr ) => { {
			let d = { let mut d = 0; d $op $d; d };
			let stops = match $xy { 0 => &mut x_stops, _ => &mut y_stops };
			stops.insert(pos[$xy] + d);
			stops.insert(pos[$xy] + d + 1);
			pos[$xy] $op d.abs();
			path.push(pos);
		} } }
		let (dir, dist) = if DECODE_COLOR {
			let dir = match step.color[2] % 16 {
				0 => Dir::Right,
				1 => Dir::Down,
				2 => Dir::Left,
				3 => Dir::Up,
				_ => panic!("Invalid color")
			};
			let dist = ((step.color[0] as usize) << 12)
				+ ((step.color[1] as usize) << 4)
				+ ((step.color[2] as usize) >> 4);
			(dir, dist)
		} else {
			(step.dir, step.dist)
		};
		match (dir, dist as isize) {
			(Dir::Up, d) => r#move!(pos[1] -= d),
			(Dir::Down, d) => r#move!(pos[1] += d),
			(Dir::Left, d) => r#move!(pos[0] -= d),
			(Dir::Right, d) => r#move!(pos[0] += d),
		}
	}

	let [x_stops, y_stops] = {
		let mut x = x_stops.into_iter().sorted().collect::<Vec<_>>();
		x.insert(0, x[0] - 1);
		x.push(*x.last().unwrap() + 1);
		let mut y = y_stops.into_iter().sorted().collect::<Vec<_>>();
		y.insert(0, y[0] - 1);
		y.push(*y.last().unwrap() + 1);
		[x, y]
	};
	let [mut i, mut j] = [
		x_stops.iter().position(|&x| x == 0).unwrap(),
		y_stops.iter().position(|&y| y == 0).unwrap(),
	];

	let path = path.into_iter().tuple_windows().fold(
		HashSet::from([[i, j]]),
		|mut path, (prev_pos, pos)| {
			if prev_pos[1] == pos[1] {
				while x_stops[i] != pos[0] {
					i = if pos[0] > prev_pos[0] { i + 1 } else { i - 1 };
					path.insert([i, j]);
				}
			} else {
				assert!(prev_pos[0] == pos[0]);
				while y_stops[j] != pos[1] {
					j = if pos[1] > prev_pos[1] { j + 1 } else { j - 1 };
					path.insert([i, j]);
				}
			}
			path
		}
	);

	let mut queue = VecDeque::from([[0, 0]]);
	let mut outside = HashSet::new();
	while let Some(pos) = queue.pop_front() {
		if !outside.insert(pos) { continue }
		for (idx, add) in [(1_usize, false), (1, true), (0, false), (0, true)] {
			let Some(adj_pos_idx) = (if add {
				let adj_pos_idx = pos[idx] + 1;
				if adj_pos_idx == match idx { 0 => &x_stops, _ => &y_stops }.len() - 1 { continue }
				Some(adj_pos_idx)
			} else {
				pos[idx].checked_sub(1)
			}) else { continue };
			let mut adj_pos = pos;
			adj_pos[idx] = adj_pos_idx;
			if !path.contains(&adj_pos) {
				queue.push_back(adj_pos)
			}
		}
	}

	#[cfg(never)]
	{
		for j in 0..y_stops.len() - 1 {
			if j > 0 { println!() }
			for i in 0..x_stops.len() - 1 {
				print!("{}", if outside.contains(&[i, j]) { '.' }
					else if path.contains(&[i, j]) { '#'}
					else { 'o' })
			}
		}
		println!();
	}

	let dx = x_stops.last().unwrap().abs_diff(*x_stops.first().unwrap());
	let dy = y_stops.last().unwrap().abs_diff(*y_stops.first().unwrap());

	dx * dy - outside.into_iter()
		.map(|[i, j]| x_stops[i + 1].abs_diff(x_stops[i]) * y_stops[j + 1].abs_diff(y_stops[j]))
		.sum::<usize>()
}

pub(crate) fn part2() -> usize {
	part2_impl::<true>(input_steps())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Dir, Step};

	impl TryFrom<u8> for Dir {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'U' => Ok(Dir::Up),
				b'D' => Ok(Dir::Down),
				b'L' => Ok(Dir::Left),
				b'R' => Ok(Dir::Right),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum StepError {
		Format,
		Dir { invalid: u8 },
		Dist(ParseIntError),
		Color { offset: usize, source: ParseIntError },
	}

	impl FromStr for Step {
		type Err = StepError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use StepError as E;

			let (dir, s) = s.split_once(' ').ok_or(E::Format)?;
			if dir.len() != 1 { return Err(E::Format) }
			let dir = dir.as_bytes()[0];
			let dir = Dir::try_from(dir).map_err(|_| E::Dir { invalid: dir })?;

			let (dist, s) = s.split_once(' ').ok_or(E::Format)?;
			let dist = dist.parse().map_err(E::Dist)?;

			let s = s.strip_prefix("(#").ok_or(E::Format)?.strip_suffix(')').ok_or(E::Format)?;
			if s.len() != 6 { return Err(E::Format) }
			let color = [
				u8::from_str_radix(&s[..2], 16).map_err(|e| E::Color { offset: 0, source: e })?,
				u8::from_str_radix(&s[2..4], 16).map_err(|e| E::Color { offset: 1, source: e })?,
				u8::from_str_radix(&s[4..], 16).map_err(|e| E::Color { offset: 2, source: e })?,
			];

			Ok(Step { dir, dist, color })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct StepsError { line: usize, source: StepError }

	pub(super) fn try_steps_from_str(s: &str)
	-> impl Iterator<Item = Result<Step, StepsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| StepsError { line: l + 1, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		R 6 (#70c710)
		D 5 (#0dc571)
		L 2 (#5713f0)
		D 2 (#d2c081)
		R 2 (#59c680)
		D 2 (#411b91)
		L 5 (#8ceee2)
		U 2 (#caa173)
		L 1 (#1b58a2)
		U 2 (#caa171)
		R 2 (#7807d2)
		U 3 (#a77fa3)
		L 2 (#015232)
		U 2 (#7a21e3)
	"};
	assert_eq!(part1_impl(parsing::try_steps_from_str(INPUT).map(|r| r.unwrap())), 62);
	assert_eq!(part1(), 48400);
	assert_eq!(part2_impl::<false>(parsing::try_steps_from_str(INPUT).map(|r| r.unwrap())), 62);
	assert_eq!(part2_impl::<false>(input_steps()), 48400);
	assert_eq!(part2_impl::<true>(parsing::try_steps_from_str(INPUT).map(|r| r.unwrap())),
		952408144115);
	assert_eq!(part2(), 72811019847283);
}
