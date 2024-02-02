// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Brick {
	start: [usize; 3],
	end: [usize; 3],
}

trait Stack: AsMut<[Brick]> {
	/// Drop the bricks until settled, returning a square “support” matrix of
	/// size `l * l`, where `l` is `self.as_mut().len()` and each element
	/// indicates whether the brick corresponding to that row (assuming
	/// row-major) ended up supporting the brick corresponding to that column.
	fn settle(mut self) -> Vec<bool> where Self: Sized {
		let bricks = self.as_mut();
		let l = bricks.len();
		let mut supports = vec![false; l * l];
		let mut settled = false;

		// TODO: Make way faster!
		loop {
			let mut any_moved = false;

			for i in 0..l {
				let z_down = bricks[i].start[2].checked_sub(1)
					.expect("floor is at z = 0, so bricks should be on or above it at z >= 1");
				if z_down == 0 { continue }

				let mut any_supports = false;
				for j in 0..l {
					if i == j { continue }

					let z_top = bricks[j].end[2];
					if z_top == z_down
						&& bricks[i].start[0] <= bricks[j].end[0]
						&& bricks[i].end[0] >= bricks[j].start[0]
						&& bricks[i].start[1] <= bricks[j].end[1]
						&& bricks[i].end[1] >= bricks[j].start[1] {
						supports[j * l + i] = true;
						any_supports = true;
					}
				}

				if !any_supports {
					debug_assert!(!settled);
					bricks[i].start[2] -= 1;
					bricks[i].end[2] -= 1;
					any_moved = true;
				}
			}

			if settled { break }
			if !any_moved { settled = true }
			supports.fill(false);
		}

		supports
	}
}

impl Stack for &mut [Brick] {}


fn input_bricks() -> Vec<Brick> {
	parsing::try_bricks_from_str(include_str!("day22.txt")).map(|r| r.unwrap()).collect()
}


fn part1_impl(input_bricks: &mut [Brick]) -> usize {
	let supports = input_bricks.settle();

	let l = input_bricks.len();
	(0..l).filter(|&j| !(0..l).any(|i| supports[j * l + i]
		&& !(0..l).any(|other_j| other_j != j && supports[other_j * l + i]))).count()
}

pub(crate) fn part1() -> usize {
	part1_impl(&mut input_bricks())
}


fn part2_impl(input_bricks: &mut [Brick]) -> usize {
	use std::collections::{VecDeque, HashSet};

	let supports = input_bricks.settle();
	println!("Ding!");

	// TODO: Make way faster!
	let l = input_bricks.len();
	(0..l).map(|j| {
		let mut queue = VecDeque::from_iter([j]);
		let mut fallen = HashSet::new();
		let mut count = 0;
		while let Some(j) = queue.pop_front() {
			let ok = fallen.insert(j);
			debug_assert!(ok);
			for i in 0..l {
				if !supports[j * l + i]
					|| (0..l).any(|other_j| other_j != j
						&& !fallen.contains(&other_j)
						&& supports[other_j * l + i]) { continue }
				queue.push_back(i);
				count += 1;
			}
		}
		count
	}).sum()
}

pub(crate) fn part2() -> usize {
	part2_impl(&mut input_bricks())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Brick;

	#[derive(Debug)]
	pub(crate) enum Coord { X, Y, Z }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(crate) enum BrickError {
		Format,
		Start { coord: Coord, source: ParseIntError },
		End { coord: Coord, source: ParseIntError },
	}

	impl FromStr for Brick {
		type Err = BrickError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use {Coord::*, BrickError as E};

			let (start, end) = s.split_once('~').ok_or(E::Format)?;

			fn coords(s: &str, coord_err: impl Fn(Coord, ParseIntError) -> E)
			-> Result<[usize; 3], E> {
				let (x, s) = s.split_once(',').ok_or(E::Format)?;
				let (y, z) = s.split_once(',').ok_or(E::Format)?;
				Ok([
					x.parse().map_err(|e| coord_err(X, e))?,
					y.parse().map_err(|e| coord_err(Y, e))?,
					z.parse().map_err(|e| coord_err(Z, e))?,
				])
			}

			Ok(Brick {
				start: coords(start, |coord, source| E::Start { coord, source })?,
				end: coords(end, |coord, source| E::End { coord, source })?,
			})
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct BricksError { line: usize, source: BrickError }

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_bricks_from_str(s: &str)
	-> impl Iterator<Item = Result<Brick, BricksError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| BricksError { line: l + 1, source: e }))
	}
}


#[cfg(test)]
#[allow(dead_code)]
fn print_bricks(bricks: &[Brick]) {
	use std::ops::RangeInclusive;

	const XY_TICKS: &str = "0123456789abcdef";
	const LABEL_ALPHABET: &[u8] = b"\
		ABCDEFGHIJKLMNOPQRSTUVWXYZ\
		abcdefghijklmnopqrstuvwxyz\
		0123456789";

	fn rlen(r: &RangeInclusive<usize>) -> usize { r.end() - r.start() + 1 }
	let mut extents = [[usize::MAX, usize::MIN]; 3];
	for brick in bricks {
		extents[0][0] = extents[0][0].min(brick.start[0]);
		extents[0][1] = extents[0][1].max(brick.end[0]);
		extents[1][0] = extents[1][0].min(brick.start[1]);
		extents[1][1] = extents[1][1].max(brick.end[1]);
		extents[2][0] = extents[2][0].min(brick.start[2]);
		extents[2][1] = extents[2][1].max(brick.end[2]);
	}
	let extents = std::array::from_fn::<_, 3, _>(|i| extents[i][0]..=extents[i][1]);
	let [dx, dy] = std::array::from_fn(|i| rlen(&extents[i]));
	let dz = extents[2].end() + 1;

	let mut gap = 1;
	while 10_usize.pow(gap as u32) - 1 < dz { gap += 1; }
	let gap = gap + 2;

	println!("{0:^1$} {2:gap$}  {3:^4$}", 'x', dx, ' ', 'y', dy);
	println!("{} {:gap$}  {}", &XY_TICKS[0..dx], ' ', &XY_TICKS[0..dy]);
	let mut elevs = [vec![b' '; dx * dz], vec![b' '; dy * dz]];
	for (i, brick) in bricks.iter().enumerate() {
		for z in brick.start[2]..=brick.end[2] {
			for x in brick.start[0]..=brick.end[0] {
				let p = z * dx + x;
				if elevs[0][p] == b' ' { elevs[0][p] = LABEL_ALPHABET[i % LABEL_ALPHABET.len()] }
				else { elevs[0][p] = b'?' }
			}
			for y in brick.start[1]..=brick.end[1] {
				let p = z * dy + y;
				if elevs[1][p] == b' ' { elevs[1][p] = LABEL_ALPHABET[i % LABEL_ALPHABET.len()] }
				else { elevs[1][p] = b'?' }
			}
		}
	}
	for z in (1..dz).rev() {
		println!("{} {:<gap$}  {} {z}",
			std::str::from_utf8(&elevs[0][z * dx..(z + 1) * dx]).unwrap(),
			format!("{z} {}", if z == dz / 2 { 'z' } else { ' ' }),
			std::str::from_utf8(&elevs[1][z * dy..(z + 1) * dy]).unwrap());
	}
	println!("{0:-^1$} {2:<gap$}  {3:-^4$} 0", '-', dx, '0', '-', dy);
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		1,0,1~1,2,1
		0,0,2~2,0,2
		0,2,3~2,2,3
		0,0,4~0,2,4
		2,0,5~2,2,5
		0,1,6~2,1,6
		1,1,8~1,1,9
	"};
	assert_eq!(part1_impl(&mut parsing::try_bricks_from_str(INPUT)
		.map(|r| r.unwrap()).collect::<Vec<_>>()), 5);
	assert_eq!(part1(), 463);
	assert_eq!(part2_impl(&mut parsing::try_bricks_from_str(INPUT)
		.map(|r| r.unwrap()).collect::<Vec<_>>()), 7);
	assert_eq!(part2(), 89727);
}
