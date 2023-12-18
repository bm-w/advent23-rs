// Copyright (c) 2023 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
enum Space {
	Vertical,
	Horizontal,
	NorthEast,
	NorthWest,
	SouthEast,
	SouthWest,
	Ground,
	Animal,
}

struct Grid {
	spaces: Vec<Space>,
	stride: usize,
}

#[derive(Clone, Copy)]
enum Dir { North, South, East, West }

impl Space {
	// `west` or else east, `north` or else south.
	fn connects(&self, to: Dir) -> bool {
		use {Space::*, Dir::*};
		matches!((self, to),
			| (Vertical, North | South)
			| (Horizontal, East | West)
			| (NorthEast, North | East)
			| (NorthWest, North | West)
			| (SouthEast, South | East)
			| (SouthWest, South | West)
		)
	}
}

impl Grid {
	fn adjacent_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Dir, Space)> {
		let north = pos >= self.stride;
		let west = pos % self.stride > 0;
		let east = pos % self.stride < self.stride - 1;
		let south = pos < self.spaces.len() - self.stride;

		macro_rules! adj_space { ( $dx:literal, $dy:literal, $dir:ident ) => { {
			#[allow(clippy::neg_multiply)]
			let pos = (pos as isize + $dx + $dy * (self.stride as isize)) as usize;
			(pos, Dir::$dir, self.spaces[pos])
		} }; }

		let mut spaces = [(usize::MAX, Dir::North, Space::Ground); 4];
		if north { spaces[0] = adj_space!(0, -1, North); }
		if south { spaces[1] = adj_space!(0, 1, South); }
		if east { spaces[2] = adj_space!(1, 0, East); }
		if west { spaces[3] = adj_space!(-1, 0, West); }

		spaces.into_iter().filter(|(pos, _, _)| *pos < usize::MAX)
	}

	fn reachable_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Dir, Space)> {
		let from_space = self.spaces[pos];
		self.adjacent_spaces(pos).filter(move |(_, dir, to_space)|
			(matches!(from_space, Space::Animal) || from_space.connects(*dir))
				&& to_space.connects(dir.inverse()))
	}

	/// Returns loop spaces and max steps.
	fn find_loop(&self) -> (std::collections::HashSet<usize>, usize) {
		use std::collections::{HashSet, VecDeque};
	
		let pos = self.spaces.iter().position(|s| matches!(s, Space::Animal)).unwrap();
		let mut queue = VecDeque::from([(pos, 0)]);
		let mut seen = HashSet::new();
		let mut max_steps = 0;
	
		while let Some((pos, steps)) = queue.pop_front() {
			if !seen.insert(pos) { continue }
			max_steps = max_steps.max(steps);
	
			for (pos, _, _) in self.reachable_spaces(pos) {
				queue.push_back((pos, steps + 1));
			}
		}
	
		(seen, max_steps)
	}

	// North-west, north-east, south-west, & south-east.
	fn pos_vertices(&self, pos: usize) -> [usize; 4] {
		let (x, y) = (pos / self.stride, pos % self.stride);
		let northwest = y * (self.stride + 1) + x;
		[northwest, northwest + 1, northwest + self.stride + 1, northwest + self.stride + 2]
	}

	fn reachable_vertices(&self,
		vertex: usize,
		is_loop: impl Fn(usize) -> bool
	) -> impl Iterator<Item = (usize, Dir)> {
		let stride = self.stride + 1;
		let spaces_height = self.spaces.len() / self.stride;
		let vertices_len = self.spaces.len() + spaces_height + stride;
		let north = vertex >= stride;
		let west = vertex % stride > 0;
		let east = vertex % stride < stride - 1;
		let south = vertex < vertices_len - stride;

		let (x, y) = (vertex % stride, vertex / stride);
		let southeast_space = y * self.stride + x; // This space may not actually exist

		macro_rules! adj_vertex { (
			$dx:literal, $dy:literal, $dir:ident,
			$pass:expr, $ldx:literal, $ldy:literal, $rdx:literal, $rdy:literal
		) => { {
			let pass = $pass || {
				let southeast_space = southeast_space as isize;
				let spaces_stride = self.stride as isize;
				#[allow(clippy::neg_multiply)]
				let lpos = (southeast_space + $ldx + ($ldy * spaces_stride)) as usize;
				#[allow(clippy::neg_multiply)]
				let rpos = (southeast_space + $rdx + ($rdy * spaces_stride)) as usize;
				!is_loop(lpos) || !is_loop(rpos) || {
					let (l, r) = (self.spaces[lpos], self.spaces[rpos]);
					let lconnects = matches!(l, Space::Animal) || l.connects(Dir::$dir.ortho());
					let rconnects = matches!(r, Space::Animal)
						|| r.connects(Dir::$dir.ortho().inverse());
					!lconnects || !rconnects
				} 
			};
			#[allow(clippy::neg_multiply)]
			if !pass { (usize::MAX, Dir::North) }
			else { ((vertex as isize + $dx + $dy * (stride as isize)) as usize, Dir::$dir) }
		} }; }

		let mut vertices = [(usize::MAX, Dir::North); 4];
		if north { vertices[0] = adj_vertex!(0, -1, North, !west || !east, -1, -1, 0, -1); }
		if south { vertices[1] = adj_vertex!(0, 1, South, !east || !west, 0, 0, -1, 0); }
		if east { vertices[2] = adj_vertex!(1, 0, East, !north || !south, 0, -1, 0, 0); }
		if west { vertices[3] = adj_vertex!(-1, 0, West, !south || !north, -1, 0, -1, -1); }

		vertices.into_iter().filter(|(pos, _)| *pos < usize::MAX)
	}
}

impl Dir {
	fn inverse(&self) -> Self {
		use Dir::*;
		match self { North => South, South => North, East => West, West => East }
	}

	fn ortho(&self) -> Self {
		use Dir::*;
		match self { North => East, South => West, East => South, West => North }
	}
}


fn input_grid() -> Grid {
	include_str!("day10.txt").parse::<Grid>().unwrap()
}


fn part1_impl(input_grid: Grid) -> usize {
	Grid::find_loop(&input_grid).1
}

pub(crate) fn part1() -> usize {
	part1_impl(input_grid())
}


fn part2_impl(input_grid: Grid) -> usize {
	use std::collections::{HashSet, VecDeque};

	let r#loop = input_grid.find_loop().0;
	let vertices_stride = input_grid.stride + 1;
	let vertices_len = input_grid.spaces.len() + 2 * vertices_stride - 1;

	for &pos in &r#loop {
		'vertices: for vertex in input_grid.pos_vertices(pos) {
			let mut queue = VecDeque::from([(vertex, 0)]);
			let mut seen = HashSet::new();

			while let Some((vertex, steps)) = queue.pop_front() {
				if !seen.insert(vertex) { continue }

				if vertex < vertices_stride
					|| vertex % vertices_stride == 0
					|| vertex % vertices_stride == (vertices_stride - 1)
					|| vertex >= vertices_len - vertices_stride { continue 'vertices }

				for (vertex, _) in input_grid.reachable_vertices(vertex,
					|pos| r#loop.contains(&pos)
				) {
					queue.push_back((vertex, steps + 1))
				}
			}

			return seen.into_iter()
				.map(|vertex| {
					let (x, y) = (vertex % vertices_stride, vertex / vertices_stride);
					y * input_grid.stride + x
				})
				.filter(|vertex| !r#loop.contains(vertex))
				.count()
		}
	}

	panic!("Inner area not found")
}

pub(crate) fn part2() -> usize {
	part2_impl(input_grid())
}


mod parsing {
	use std::str::FromStr;
	use super::{Grid, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			use Space::*;
			match value {
				b'|' => Ok(Vertical),
				b'-' => Ok(Horizontal),
				b'L' => Ok(NorthEast),
				b'J' => Ok(NorthWest),
				b'F' => Ok(SouthEast),
				b'7' => Ok(SouthWest),
				b'.' => Ok(Ground),
				b'S' => Ok(Animal),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum GridError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
		MissingAnimal,
	}

	impl FromStr for Grid {
		type Err = GridError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use GridError as E;

			let (spaces, stride, animal) = s.lines()
				.enumerate()
				.try_fold((Vec::new(), 0, false), |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.0.len() % stride != 0 {
						return Err(E::Format { line: l + 1 }) }

					if acc.1 == 0 { acc.1 = stride; }
					acc.0.reserve(stride);

					for (c, byte) in line.bytes().enumerate() {
						let space = Space::try_from(byte)
							.map_err(|_| E::Space { line: l + 1, column: c + 1, invalid: byte })?;
						acc.0.push(space);
						if matches!(space, Space::Animal) {
							acc.2 = true
						}
					}

					Ok(acc)
				})?;

			if !animal { return Err(E::MissingAnimal) }

			Ok(Grid { spaces, stride })
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn part1() {
		const TINY_INPUT: &str = indoc::indoc! {"
			-L|F7
			7S-7|
			L|7||
			-L-J|
			L|-JF
		"};
		const SLIGHTLY_MORE_COMPLEX_INPUT: &str = indoc::indoc! { "
			7-F7-
			.FJ|7
			SJLL7
			|F--J
			LJ.LJ
		"};
		assert_eq!(part1_impl(TINY_INPUT.parse::<Grid>().unwrap()), 4);
		assert_eq!(part1_impl(SLIGHTLY_MORE_COMPLEX_INPUT.parse::<Grid>().unwrap()), 8);
		assert_eq!(super::part1(), 6923);
	}

	#[test]
	fn part2() {
		const INPUTS: [&str; 3] = [
			indoc::indoc! {"
				...........
				.S-------7.
				.|F-----7|.
				.||.....||.
				.||.....||.
				.|L-7.F-J|.
				.|..|.|..|.
				.L--J.L--J.
				...........
			"},
			indoc::indoc! {"
				.F----7F7F7F7F-7....
				.|F--7||||||||FJ....
				.||.FJ||||||||L7....
				FJL7L7LJLJ||LJ.L-7..
				L--J.L7...LJS7F-7L7.
				....F-J..F7FJ|L7L7L7
				....L7.F7||L7|.L7L7|
				.....|FJLJ|FJ|F7|.LJ
				....FJL-7.||.||||...
				....L---J.LJ.LJLJ...
			"},
			indoc::indoc! {"
				FF7FSF7F7F7F7F7F---7
				L|LJ||||||||||||F--J
				FL-7LJLJ||||||LJL-77
				F--JF--7||LJLJ7F7FJ-
				L---JF-JLJ.||-FJLJJ7
				|F|F-JF---7F7-L7L|7|
				|FFJF7L7F-JF7|JL---7
				7-L-JL7||F7|L7F-7F7|
				L.L7LFJ|||||FJL7||LJ
				L7JLJL-JLJLJL--JLJ.L
			"}
		];
		assert_eq!(part2_impl(INPUTS[0].parse::<Grid>().unwrap()), 4);
		assert_eq!(part2_impl(INPUTS[1].parse::<Grid>().unwrap()), 8);
		assert_eq!(super::part2(), 529);
	}
}
