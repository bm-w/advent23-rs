// Copyright (c) 2023 Bastiaan Marinus van de Weerd


struct Grid {
	width: usize,
	height: usize,
	galaxies: Vec<usize>,
}


fn input_grid() -> Grid {
	include_str!("day11.txt").parse::<Grid>().unwrap()
}

fn part1and2_impl<const EXPANDED: usize>(input_grid: Grid) -> usize {
	use std::collections::HashSet;

	let [exp_x, exp_y] = {
		let [mut xx, mut yy] = std::array::from_fn(|_| HashSet::new());
		for &galaxy in &input_grid.galaxies {
			xx.insert(galaxy % input_grid.width);
			yy.insert(galaxy / input_grid.width);
		}
		[(0..input_grid.width).filter(|x| !xx.contains(x)).collect::<HashSet<_>>(),
			(0..input_grid.height).filter(|y| !yy.contains(y)).collect::<HashSet<_>>()]
	};

	let mut dists = 0;
	for from in 0..input_grid.galaxies.len() {
		let from_galaxy = input_grid.galaxies[from];
		let [from_x, from_y] = [from_galaxy % input_grid.width, from_galaxy / input_grid.width];
		for &to_galaxy in &input_grid.galaxies[from + 1..] {
			let [to_x, to_y] = [to_galaxy % input_grid.width, to_galaxy / input_grid.width];
			let [from_min_x, from_max_x] = [from_x.min(to_x), from_x.max(to_x)];
			let [from_min_y, from_max_y] = [from_y.min(to_y), from_y.max(to_y)];
			let dx: usize = (from_min_x..from_max_x)
				.map(|x| if exp_x.contains(&x) { EXPANDED } else { 1 }).sum();
			let dy: usize = (from_min_y..from_max_y)
				.map(|y| if exp_y.contains(&y) { EXPANDED } else { 1 }).sum();
			dists += dx + dy;
		}
	}

	dists
}

pub(crate) fn part1() -> usize {
	part1and2_impl::<2>(input_grid())
}

pub(crate) fn part2() -> usize {
	part1and2_impl::<1_000_000>(input_grid())
}


mod parsing {
	use std::str::FromStr;
	use super::Grid;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum GridError {
		Format { line: usize },
		Space { line: usize, column: usize, source: u8}
	}

	impl FromStr for Grid {
		type Err = GridError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use GridError as E;

			fn isqrt(n: usize) -> usize {
				(1..).find(|i| i * i >= n).unwrap() - 1
			}

			let (galaxies, width, _, height) = s.lines()
				.enumerate()
				.try_fold((Vec::new(), 0, 0, 0), |mut acc, (l, line)| {
					let width = line.len();
					if width == 0 || acc.1 != 0 && width != acc.1 {
						return Err(E::Format { line: l + 1 }) }

					acc.3 += 1;
					if acc.1 == 0 { acc.1 = width; acc.2 = isqrt(width) }
					acc.0.reserve(acc.2);

					for (c, byte) in line.bytes().enumerate() {
						match byte {
							b'.' => (),
							b'#' => acc.0.push(l * width + c),
							_ => return Err(E::Space { line: l + 1, column: c + 1, source: byte }),
						}
					}

					Ok(acc)
				})?;

			Ok(Grid { width, height, galaxies })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		...#......
		.......#..
		#.........
		..........
		......#...
		.#........
		.........#
		..........
		.......#..
		#...#.....
	"};
	assert_eq!(part1and2_impl::<2>(INPUT.parse::<Grid>().unwrap()), 374);
	assert_eq!(part1(), 9947476);
	assert_eq!(part1and2_impl::<10>(INPUT.parse::<Grid>().unwrap()), 1030);
	assert_eq!(part1and2_impl::<100>(INPUT.parse::<Grid>().unwrap()), 8410);
	assert_eq!(part2(), 519939907614);
}
