// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
enum Dir { Up, Down, Left, Right }

#[derive(Clone, Copy)]
enum Space { Path, Forest, Slope(Dir) }

struct Map {
	spaces: Vec<Space>,
	stride: usize,
}

impl Map {
	fn adjacent_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Space)> {
		let up = pos >= self.stride;
		let left = pos % self.stride > 0;
		let right = pos % self.stride < self.stride - 1;
		let down = pos < self.spaces.len() - self.stride;

		macro_rules! adj_space { ( $dx:literal, $dy:literal ) => { {
			#[allow(clippy::neg_multiply)]
			let pos = (pos as isize + $dx + $dy * (self.stride as isize)) as usize;
			(pos, self.spaces[pos])
		} }; }

		let mut spaces = [(usize::MAX, Space::Path); 8];
		if up { spaces[0] = adj_space!(0, -1) }
		if down { spaces[1] = adj_space!(0, 1) }
		if left { spaces[2] = adj_space!(-1, 0) }
		if right { spaces[3] = adj_space!(1, 0) }
		spaces.into_iter().filter(|(pos, _)| *pos < usize::MAX)
	}

	fn reachable_spaces<const SLIPPERY: bool>(&self, pos: usize)
	-> impl Iterator<Item = (usize, Space)> + '_ {
		use {Space::*, Dir::*};
		self.adjacent_spaces(pos).filter(move |&(adj_pos, adj_space)| !matches!(adj_space, Forest)
			&& (!SLIPPERY || match self.spaces[pos] {
				Slope(Up) => adj_pos + self.stride == pos,
				Slope(Down) => adj_pos == pos + self.stride,
				Slope(Left) => adj_pos + 1 == pos,
				Slope(Right) => adj_pos == pos + 1,
				_ => true,
			}))
	}

	fn start(&self) -> usize {
		self.spaces[0..self.stride].iter()
			.position(|space| matches!(space, Space::Path))
			.expect("start tile")
	}

	fn end(&self) -> usize {
		let from = self.spaces.len() - self.stride;
		from + self.spaces[from..].iter()
			.position(|space| matches!(space, Space::Path))
			.expect("end tile")
	}

	/// Returns the position of the start & end nodes, and a connectivity map
	/// of all nodes to closest other nodes with corresponding step counts.
	#[allow(clippy::type_complexity)]
	fn paths<const SLIPPERY: bool>(&self)
	-> (usize, usize, std::collections::HashMap<usize, Vec<(usize, usize)>>) {
		use std::collections::{HashMap, HashSet, VecDeque};

		let [start, end] = [self.start(), self.end()];

		let mut nodes = HashMap::with_capacity(self.stride);

		nodes.insert(start, Vec::with_capacity(1));
		nodes.insert(end, Vec::with_capacity(1));
		for (pos, &space) in self.spaces.iter().enumerate() {
			if !matches!(space, Space::Path) { continue }
			let reachable_count = self.reachable_spaces::<SLIPPERY>(pos).count();
			if reachable_count <= 2 { continue }
			nodes.insert(pos, Vec::with_capacity(reachable_count));
		}

		let is_node = {
			let nodes: *const HashMap<_, _> = &nodes as _;
			// SAFETY: `iter_mut` does not modify the map’s structure.
			move |pos| unsafe { &*nodes }.contains_key(&pos)
		};

		for (&pos, paths) in nodes.iter_mut() {
			let mut queue = VecDeque::from_iter([(pos, 0)]);
			let mut visited = HashSet::new();
			while let Some((pos, steps)) = queue.pop_front() {
				if !visited.insert(pos) { continue }
				for (adj_pos, _) in self.reachable_spaces::<SLIPPERY>(pos) {
					if visited.contains(&adj_pos) { continue }
					if is_node(adj_pos) { paths.push((adj_pos, steps + 1)) }
					else { queue.push_back((adj_pos, steps + 1)) }
				}
			}
		}

		(start, end, nodes)
	}
}


fn input_map() -> Map {
	include_str!("day23.txt").parse().unwrap()
}

fn part1and2_impl<const SLIPPERY: bool>(input_map: Map) -> usize {
	use std::{collections::VecDeque, rc::Rc};

	let (start, end, paths) = input_map.paths::<SLIPPERY>();

	struct Node {
		pos: usize,
		steps: usize,
		prev: Option<Rc<Node>>,
	}

	impl Node {
		fn path(&self) -> impl Iterator<Item = usize> + '_ {
			let mut pos = Some(self.pos);
			let mut next = self.prev.as_deref();
			std::iter::from_fn(move || {
				let cur_pos = pos.take()?;
				if let Some(cur_next) = next.take() {
					pos = Some(cur_next.pos);
					next = cur_next.prev.as_deref();
				}
				Some(cur_pos)
			})
		}

		fn visited(&self, pos: usize) -> bool {
			self.path().any(|p| p == pos)
		}

		#[cfg(never)]
		fn print(&self, map: &Map, cur_pos: Option<usize>) {
			let visited = std::collections::HashSet::<usize>::from_iter(self.path());
			for y in 0..map.spaces.len() / map.stride {
				for pos in y * map.stride..(y + 1) * map.stride {
					print!("{}", if cur_pos == Some(pos) { 'X' }
						else if visited.contains(&pos) { 'O' }
						else { match map.spaces[pos] {
							Space::Path => '.',
							Space::Forest => '#',
							Space::Slope(Dir::Up) => '^',
							Space::Slope(Dir::Down) => 'v',
							Space::Slope(Dir::Left) => '<',
							Space::Slope(Dir::Right) => '>',
						} })
				}
				println!();
			}
		}
	}

	let mut max_steps = 0;
	let mut queue = VecDeque::from_iter([Node { pos: start, steps: 0, prev: None }]);
	while let Some(node) = queue.pop_front() {
		#[cfg(never)] let mut dead_end = true;
		let node = Rc::new(node);
		for &(adj_pos, adj_steps) in paths.get(&node.pos).unwrap() {
			let steps = node.steps + adj_steps;
			if adj_pos == end {
				max_steps = max_steps.max(steps);
				#[cfg(never)] { println!("Goal!"); node.print(&input_map, None) }
				continue
			}
			if node.visited(adj_pos) { continue }
			#[cfg(never)] { dead_end = false }
			queue.push_back(Node { pos: adj_pos, steps, prev: Some(node.clone()) })
		}
		#[cfg(never)] if dead_end { println!("Dead end!"); node.print(&input_map, Some(node.pos)) }
	}

	max_steps
}

pub(crate) fn part1() -> usize {
	part1and2_impl::<true>(input_map())
}

pub(crate) fn part2() -> usize {
	part1and2_impl::<false>(input_map())
}


mod parsing {
	use std::str::FromStr;
	use super::{Dir, Map, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Space::Path),
				b'#' => Ok(Space::Forest),
				b'^' => Ok(Space::Slope(Dir::Up)),
				b'v' => Ok(Space::Slope(Dir::Down)),
				b'<' => Ok(Space::Slope(Dir::Left)),
				b'>' => Ok(Space::Slope(Dir::Right)),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use MapError as E;
			s.lines()
				.enumerate()
				.try_fold(Map { spaces: Vec::new(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 {
						return Err(E::Format { line: l + 1 })}

					if acc.stride == 0 { acc.stride = stride; }

					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						acc.spaces.push(Space::try_from(byte)
							.map_err(|_| E::Space { line: l + 1, column: c + 1, invalid: byte })?)
					}

					Ok(acc)
				})
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		#.#####################
		#.......#########...###
		#######.#########.#.###
		###.....#.>.>.###.#.###
		###v#####.#v#.###.#.###
		###.>...#.#.#.....#...#
		###v###.#.#.#########.#
		###...#.#.#.......#...#
		#####.#.#.#######.#.###
		#.....#.#.#.......#...#
		#.#####.#.#.#########v#
		#.#...#...#...###...>.#
		#.#.#v#######v###.###v#
		#...#.>.#...>.>.#.###.#
		#####v#.#.###v#.#.###.#
		#.....#...#...#.#.#...#
		#.#########.###.#.#.###
		#...###...#...#...#.###
		###.###.#.###v#####v###
		#...#...#.#.>.>.#.>.###
		#.###.###.#.###.#.#v###
		#.....###...###...#...#
		#####################.#
	"};
	assert_eq!(part1and2_impl::<true>(INPUT.parse().unwrap()), 94);
	assert_eq!(part1(), 2034);
	assert_eq!(part1and2_impl::<false>(INPUT.parse().unwrap()), 154);
	assert_eq!(part2(), 6302);
}
