// Copyright (c) 2024 Bastiaan Marinus van de Weerd


struct Map {
	heat_losses: Vec<u8>,
	stride: usize,
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Dir { Up = 0, Down = 1, Left = 2, Right = 3 }

impl Dir {
	fn inverse(&self) -> Self {
		use Dir::*;
		match self { Up => Down, Down => Up, Left => Right, Right => Left }
	}
}

impl Map {
	fn adjacent_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Dir, u8)> {
		let up = pos >= self.stride;
		let left = pos % self.stride > 0;
		let right = (pos + 1) % self.stride > 0;
		let down = pos < self.heat_losses.len() - self.stride;

		macro_rules! adj_space { ( $dx:literal, $dy:literal, $dir:ident ) => { {
			#[allow(clippy::neg_multiply)]
			let pos = (pos as isize + $dx + $dy * (self.stride as isize)) as usize;
			(pos, Dir::$dir, self.heat_losses[pos])
		} }; }

		let mut spaces = [(usize::MAX, Dir::Up, 0u8); 4];
		if up { spaces[0] = adj_space!(0, -1, Up) }
		if down { spaces[1] = adj_space!(0, 1, Down) }
		if left { spaces[2] = adj_space!(-1, 0, Left) }
		if right { spaces[3] = adj_space!(1, 0, Right) }
		spaces.into_iter().filter(|(pos, _, _)| *pos < usize::MAX)
	}
}


fn input_map() -> Map {
	include_str!("day17.txt").parse().unwrap()
}


fn part1_impl(input_map: Map) -> u64 {
	use std::{cmp::Reverse, collections::{BinaryHeap, HashMap, hash_map::Entry}};

	#[derive(PartialEq, Eq)]
	struct State {
		pos: usize,
		dir: (Dir, usize),
		loss: u64,
	}

	impl Ord for State {
		fn cmp(&self, other: &Self) -> std::cmp::Ordering {
			self.loss.cmp(&other.loss)
				.then(self.pos.cmp(&other.pos))
				.then(self.dir.cmp(&other.dir))
		}
	}

	impl PartialOrd for State {
		fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
			Some(self.cmp(other))
		}
	}

	let mut heap = BinaryHeap::from([Reverse(State { pos: 0, dir: (Dir::Up, 0), loss: 0 })]);
	let mut losses = HashMap::with_capacity(input_map.heat_losses.len() * 4);

	while let Some(Reverse(State { pos, dir: (dir, dir_steps), loss })) = heap.pop() {
		if pos == input_map.heat_losses.len() - 1 { return loss }

		match losses.entry((pos, dir, dir_steps)) {
			Entry::Vacant(entry) => { entry.insert(loss); },
			Entry::Occupied(mut entry) => {
				if *entry.get() <= loss { continue }
				entry.insert(loss);
			}
		}

		#[cfg(never)]
		println!("{pos} ({},{}; {dir:?} x {dir_steps}): {loss}",
			pos % input_map.stride, pos / input_map.stride);

		for (adj_pos, adj_dir, add_loss) in input_map.adjacent_spaces(pos) {
			let dir_steps = if pos != 0 {
				if adj_dir == dir.inverse() { continue }
				let dir_steps = if adj_dir == dir { dir_steps + 1 } else { 1 };
				if dir_steps > 3 { continue }
				dir_steps
			} else {
				1
			};
			let loss = loss + add_loss as u64;
			if losses.get(&(adj_pos, adj_dir, dir_steps)).map(|&l| loss < l).unwrap_or(true) {
				#[cfg(never)]
				println!(" └-> {adj_pos} ({},{}; {adj_dir:?} x {dir_steps}): {loss}",
					adj_pos % input_map.stride, adj_pos / input_map.stride);
				heap.push(Reverse(State { pos: adj_pos, dir: (adj_dir, dir_steps), loss }))
			}
		}
	}

	panic!("No path!");
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_map())
}


fn part2_impl(input_map: Map) -> u64 {
	use std::{cmp::Reverse, collections::{BinaryHeap, HashMap, hash_map::Entry}};

	#[derive(PartialEq, Eq)]
	struct State {
		pos: usize,
		dir: (Dir, usize),
		loss: u64,
		#[cfg(never)]
		path: Vec<usize>,
	}

	impl Ord for State {
		fn cmp(&self, other: &Self) -> std::cmp::Ordering {
			self.loss.cmp(&other.loss)
				.then(self.pos.cmp(&other.pos))
				.then(self.dir.cmp(&other.dir))
		}
	}

	impl PartialOrd for State {
		fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
			Some(self.cmp(other))
		}
	}

	let mut heap = BinaryHeap::from(
		[Reverse(State { pos: 0, dir: (Dir::Up, 0), loss: 0, #[cfg(never)] path: vec![0] })]);
	let mut losses = HashMap::with_capacity(input_map.heat_losses.len() * 4);

	while let Some(Reverse(state)) = heap.pop() {
		#[cfg(never)]
		let State { pos, dir: (dir, dir_steps), loss, path } = state;
		#[cfg(not(never))]
		let State { pos, dir: (dir, dir_steps), loss } = state;

		if pos == input_map.heat_losses.len() - 1 {
			if dir_steps < 4 { continue }
			#[cfg(never)]
			{
				use itertools::Itertools as _;
				let mut map = format!("{input_map}");
				unsafe { map.as_bytes_mut()[0] = b'.' }
				for (prev_pos, pos) in path.into_iter().tuple_windows() {
					let [x, y] = [pos % input_map.stride, pos / input_map.stride];
					let str_pos = y * (input_map.stride + 1) + x;
					let byte = if pos > prev_pos { if pos - prev_pos == 1 { b'>'} else { b'v' } }
						else if prev_pos - pos == 1 { b'<' } else { b'^' };
					unsafe { map.as_bytes_mut()[str_pos] = byte; }
				}
				println!("Ding:\n{map}");
			}
			return loss
		}

		match losses.entry((pos, dir, dir_steps)) {
			Entry::Vacant(entry) => { entry.insert(loss); },
			Entry::Occupied(mut entry) => {
				if *entry.get() <= loss { continue }
				entry.insert(loss);
			}
		}

		#[cfg(never)]
		println!("{pos} ({},{}; {dir:?} x {dir_steps}): {loss}",
			pos % input_map.stride, pos / input_map.stride);

		for (adj_pos, adj_dir, add_loss) in input_map.adjacent_spaces(pos) {
			let dir_steps = if pos != 0 {
				if adj_dir == dir.inverse() { continue }
				let dir_steps = if adj_dir == dir {
					dir_steps + 1
				} else if dir_steps >= 4 {
					// TODO: Skip ahead 4 steps to reduce search space
					1
				} else {
					continue
				};
				if dir_steps > 10 { continue }
				dir_steps
			} else {
				1
			};
			let loss = loss + add_loss as u64;
			if losses.get(&(adj_pos, adj_dir, dir_steps)).map(|&l| loss < l).unwrap_or(true) {
				#[cfg(never)]
				{
					let mut path = path.clone();
					path.push(adj_pos);
					println!(" └-> {adj_pos} ({},{}; {adj_dir:?} x {dir_steps}): {loss}",a
						adj_pos % input_map.stride, adj_pos / input_map.stride);
					heap.push(Reverse(State { pos: adj_pos, dir: (adj_dir, dir_steps), loss, path }))
				};
				#[cfg(not(never))]
				heap.push(Reverse(State { pos: adj_pos, dir: (adj_dir, dir_steps), loss }))
			}
		}
	}

	panic!("No path!");
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_map())
}


mod parsing {
	use std::str::FromStr;
	use super::Map;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Format { line: usize },
		HeatLoss { line: usize, column: usize, invalid: u8 },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			s.lines()
				.enumerate()
				.try_fold(Map { heat_losses: Vec::default(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.heat_losses.len() % stride != 0 { return Err(
						MapError::Format { line: l + 1 }) }

					if acc.stride == 0 { acc.stride = stride }
					acc.heat_losses.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						if !byte.is_ascii_digit() { return Err(
							MapError::HeatLoss { line: l + 1, column: c + 1, invalid: byte }) }
						acc.heat_losses.push(byte - b'0');
					}
					Ok(acc)
				})
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for Map {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;
			for y in 0..self.heat_losses.len() / self.stride {
				if y > 0 { f.write_char('\n')? }
				for pos in y * self.stride..(y + 1) * self.stride {
					f.write_char((b'0' + self.heat_losses[pos]) as char)?
				}
			}
			Ok(())
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		2413432311323
		3215453535623
		3255245654254
		3446585845452
		4546657867536
		1438598798454
		4457876987766
		3637877979653
		4654967986887
		4564679986453
		1224686865563
		2546548887735
		4322674655533
	"};
	assert_eq!(part1_impl(INPUT.parse().unwrap()), 102);
	assert_eq!(part1(), 916);
	assert_eq!(part2_impl(INPUT.parse().unwrap()), 94);
	const UNFORTUNATE_INPUT: &str = indoc::indoc! {"
		111111111111
		999999999991
		999999999991
		999999999991
		999999999991
	"};
	assert_eq!(part2_impl(UNFORTUNATE_INPUT.parse().unwrap()), 71);
	assert_eq!(part2(), 1067);
}
