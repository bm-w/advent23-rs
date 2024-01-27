// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
enum Space { Garden, Rock, Start }

struct Map {
	spaces: Vec<Space>,
	stride: usize,
}

impl Map {
	fn adjacent_spaces(&self, pos: usize) -> impl Iterator<Item = (usize, Space)> {
		let up = pos >= self.stride;
		let left = pos % self.stride > 0;
		let right = (pos + 1) % self.stride > 0;
		let down = pos < self.spaces.len() - self.stride;

		macro_rules! adj_space { ( $dx:literal, $dy:literal ) => { {
			#[allow(clippy::neg_multiply)]
			let pos = (pos as isize + $dx + $dy * (self.stride as isize)) as usize;
			(pos, self.spaces[pos])
		} }; }

		let mut spaces = [(usize::MAX, Space::Rock); 4];
		if up { spaces[0] = adj_space!(0, -1) }
		if down { spaces[1] = adj_space!(0, 1) }
		if left { spaces[2] = adj_space!(-1, 0) }
		if right { spaces[3] = adj_space!(1, 0) }
		spaces.into_iter().filter(|(pos, _)| *pos < usize::MAX)
	}

	fn count_steps<const MAX_STEPS: usize>(&self, start: usize) -> Vec<usize> {
		use std::collections::VecDeque;

		let mut queue = VecDeque::from_iter([(start, 0)]);
		let mut seen = vec![usize::MAX; self.spaces.len()];

		while let Some((pos, steps)) = queue.pop_front() {
			if seen[pos] <= steps { continue }
			seen[pos] = steps;
			if steps == MAX_STEPS { continue }
			for (adj_pos, space) in self.adjacent_spaces(pos) {
				if matches!(space, Space::Rock) || seen[adj_pos] <= steps + 1 { continue }
				queue.push_back((adj_pos, steps + 1))
			}
		}

		seen
	}
}


fn input_map() -> Map {
	include_str!("day21.txt").parse().unwrap()
}


fn part1_impl<const STEPS: usize>(input_map: Map) -> usize {
	assert!(STEPS % 2 == 0);
	
	let start = input_map.spaces.iter().position(|s| matches!(s, Space::Start)).unwrap();
	let seen = input_map.count_steps::<STEPS>(start);
	seen.into_iter().filter(|s| *s < usize::MAX && s % 2 == 0).count()
}

pub(crate) fn part1() -> usize {
	part1_impl::<64>(include_str!("day21.txt").parse().unwrap())
}


/// _NB. This solutions Makes assumptions that only work for the true input
/// map (expressed below in `debug_assert`s); not the example one._
fn part2_impl<const STEPS: usize>(input_map: Map) -> usize {

	// `STEPS` is odd
	debug_assert_eq!(STEPS % 2, 1);

	// Map is square
	debug_assert_eq!(input_map.stride * input_map.stride, input_map.spaces.len());

	let [d, r] = [input_map.stride, input_map.stride / 2];
	debug_assert_eq!(d, 131);

	// Start is at the center
	debug_assert!(matches!(input_map.spaces[8580], Space::Start));

	// Center row contains no rocks
	debug_assert!(!input_map.spaces[8515..8646].iter().any(|s| matches!(s, Space::Rock)));
	// Center column contains no rocks
	debug_assert!(!input_map.spaces[65..].iter().step_by(131).any(|s| matches!(s, Space::Rock)));

	// Edge rows & columns contain no rocks
	debug_assert!(!input_map.spaces[0..131].iter().any(|s| matches!(s, Space::Rock)));
	debug_assert!(!input_map.spaces[17030..].iter().any(|s| matches!(s, Space::Rock)));
	debug_assert!(!input_map.spaces.iter().step_by(131).any(|s| matches!(s, Space::Rock)));
	debug_assert!(!input_map.spaces[130..].iter().step_by(131).any(|s| matches!(s, Space::Rock)));

	let start_steps = input_map.count_steps::<{usize::MAX}>(d * d / 2);

	// The elf can reach any plots that are an odd number of steps away from
	// the starting point on the starting map.
	let start_map_count = start_steps.iter()
		.filter(|&&s| s != usize::MAX && s % 2 == STEPS % 2)
		.count();

	// The minimal no. steps from the starting point to the 1st-ring maps, i.e.
	// to the middle plots on the inner edges of those maps, is even. As such,
	// the elf can reach plots that are an odd number of steps away from those
	// middle edge plots (even + odd = odd). Those middle edge plots are an odd
	// number away from the starting point on the starting map, and so the
	// reachable plots an even number.
	debug_assert_eq!(start_steps[65] % 2, 1); // Middle plot along top edge
	let count_per_odd_ring_map = start_steps.iter()
		.filter(|&&s| s != usize::MAX && s % 2 == 0)
		.count();

	// The minimal no. of steps from the starting point to the 2nd-ring maps,
	// i.e. to the inner corner plots on those maps, is also even. As such, the
	// elf can reach plots that are an odd number of steps away from those
	// corner plots as well. But those corner plots are an even number away
	// from the starting point on the starting map, and so the reachable plots
	// plots an odd number. (Equivalently, the no. of steps from the starting
	// point to the middle plots on the inner edges of those 2nd-ring maps is
	// odd.)
	debug_assert_eq!(start_steps[0] % 2, 0); // Upper-left corner plot
	let count_per_even_ring_map = start_steps.iter()
		.filter(|&&s| s != usize::MAX && s % 2 != 0)
		.count();

	// First compute the extent of the region of fully-reachable maps by
	// computing the number of steps remaining after first walking to one of
	// the corners of the starting map, and then dividing by the maps’ size.
	let n = (STEPS - 2 * r) / d;

	// Compute the number of maps in odd-numbered rings, and multiply by
	// `count_per_odd_ring_map` to get the number of reachable plots in
	// odd-numbered rings.
	let odd_ring_maps = 4 * ((n + 1) / 2) * ((n + 1) / 2);
	let odd_ring_maps_count = odd_ring_maps * count_per_odd_ring_map;
	
	// Same for the number of reachable plots in even-numbered rings.
	let even_ring_maps = 4 * (n / 2) * (n / 2 + 1);
	let even_ring_maps_count = even_ring_maps * count_per_even_ring_map;

	// Sum the previous counts and start adding the partial maps’ counts.
	let mut count = start_map_count + odd_ring_maps_count + even_ring_maps_count;

	// TODO: Compute the remaining partial maps algebraically

	// The first 4 partial maps are the ”points” of the reachable area, and the
	// elf first reaches those maps’ inner edge middle plots.
	for mid_edge_start in [r, d * d - r - 1, d * r, d * r + d - 1] { // Up, down, left right
		let map_steps = input_map.count_steps::<{usize::MAX}>(mid_edge_start);

		// Add one more step if any are left to reach the partial map.
		let mut remaining_steps_opt = (STEPS - r - n * d).checked_sub(1);

		// If the partial map is on an odd-numbered ring then the plots that
		// are an odd number of steps away from the inner edge middle plot are
		// reachable.
		let mut odd_ring = n % 2 == 0;
		while let Some(remaining_steps) = remaining_steps_opt {
			count += map_steps.iter()
				.filter(|&&s| s <= remaining_steps && (s % 2 != 0) == odd_ring)
				.count();
			remaining_steps_opt = remaining_steps.checked_sub(d);
			odd_ring = !odd_ring;
		}
	}

	// The final partial maps are on the diagonal edges of the reachable area
	// and the elf first reaches those maps’ inner corner plots.
	for corner_start in [0, d - 1, d * d - d, d * d - 1] { // Upper-left, up.-right, lower-l., low.-r.
		let map_steps = input_map.count_steps::<{usize::MAX}>(corner_start);

		// Add two more step, if any are left, to reach the partial map.
		let mut remaining_steps_opt = (STEPS - 2 * r - (n - 1) * d).checked_sub(2);

		// The number of partial maps grows every ring.
		let mut n = n;

		// If the partial map is on an odd-numbered ring then the plots that
		// are an even number of steps away from the corner plot are reachable.
		let mut odd_ring = n % 2 == 0;
		while let Some(remaining_steps) = remaining_steps_opt {
			count += n * map_steps.iter()
				.filter(|&&s| s <= remaining_steps && (s % 2 == 0) == odd_ring)
				.count();
			remaining_steps_opt = remaining_steps.checked_sub(d);
			n += 1;
			odd_ring = !odd_ring;
		}
	}

	count
}

pub(crate) fn part2() -> usize {
	part2_impl::<26501365>(input_map())
}


mod parsing {
	use std::str::FromStr;
	use super::{Map, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Space::Garden),
				b'#' => Ok(Space::Rock),
				b'S' => Ok(Space::Start),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
		MissingStart,
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use MapError as E;
			let map = s.lines()
				.enumerate()
				.try_fold(Map { spaces: Vec::default(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 { return Err(
						MapError::Format { line: l + 1 }) }

					if acc.stride == 0 { acc.stride = stride }
					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						let space = Space::try_from(byte)
							.map_err(|_| E::Space { line: l + 1, column: c + 1, invalid: byte })?;
						acc.spaces.push(space);
					}
					Ok(acc)
				})?;

			if !map.spaces.iter().any(|s| matches!(s, Space::Start)) {
				return Err(E::MissingStart)
			}

			Ok(map)
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		...........
		.....###.#.
		.###.##..#.
		..#.#...#..
		....#.#....
		.##..S####.
		.##..#...#.
		.......##..
		.##.#.####.
		.##..##.##.
		...........
	"};
	assert_eq!(part1_impl::<6>(INPUT.parse().unwrap()), 16);
	assert_eq!(part1(), 3615);
	assert_eq!(part2(), 602259568764234);
}
