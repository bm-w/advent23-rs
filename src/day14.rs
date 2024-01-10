// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Space { Empty, Cube, Round }

#[derive(Clone, PartialEq, Eq, Hash)]
struct Platform {
	spaces: Vec<Space>,
	stride: usize,
}

enum TiltDir { North, West, South, East }

impl TiltDir {
	fn iter_poss<'p>(&self, platform: &'p Platform)
	-> Box<dyn Iterator<Item = usize> + '_> {
		macro_rules! east_west { ($x:expr, $y:expr) => { {
			let stride = platform.stride;
			Box::new(itertools::iproduct!($y, $x).map(move |(y, x)| y * stride + x))
		} }; }
		match self {
			Self::North => Box::new(0..platform.spaces.len()),
			Self::West => east_west!(0..platform.stride,
				(0..platform.spaces.len() / platform.stride).rev()),
			Self::South => Box::new((0..platform.spaces.len()).rev()),
			Self::East => east_west!((0..platform.stride).rev(),
				(0..platform.spaces.len() / platform.stride)),
		}
	}

	fn xy_hat(&self, platform: &Platform, pos: usize) -> [usize; 2] {
		let [x, y] = [pos % platform.stride, pos / platform.stride];
		if matches!(self, Self::North) { return [x, y] }
		else if matches!(self, Self::East) { return [y, platform.stride - x - 1] }
		let north_depth = platform.spaces.len() / platform.stride;
		if matches!(self, Self::South) { [platform.stride - x - 1, north_depth - y - 1] }
		else { [north_depth - y - 1, x] }
	}

	fn pos(&self, platform: &Platform, [x_hat, y_hat]: [usize; 2]) -> usize {
		if matches!(self, Self::North) { return y_hat * platform.stride + x_hat }
		else if matches!(self, Self::East) { return (x_hat + 1) * platform.stride - y_hat - 1 }
		let north_depth = platform.spaces.len() / platform.stride;
		if matches!(self, Self::South) { (north_depth - y_hat) * platform.stride - x_hat - 1 }
		else { (north_depth - x_hat - 1) * platform.stride + y_hat }
	}
}

impl Platform {
	fn tilt(&mut self, dir: TiltDir) -> u64 {
		let north_depth = self.spaces.len() / self.stride;
		let mut north_beams_load = 0;
		let width = if matches!(dir, TiltDir::North | TiltDir::South) { self.stride }
			else { north_depth };
		let mut depths = vec![0; width];

		for pos in dir.iter_poss(self) {
			match self.spaces[pos] {
				Space::Empty => {}
				Space::Cube => {
					let [x_hat, y_hat] = dir.xy_hat(self, pos);
					depths[x_hat] = y_hat + 1;
				}
				Space::Round => {
					let [x_hat, _] = dir.xy_hat(self, pos);
					depths[x_hat] += 1;
					let dest = dir.pos(self, [x_hat, depths[x_hat] - 1]);
					self.spaces.swap(pos, dest);
					north_beams_load += (north_depth - dest / self.stride) as u64;
				}
			}
		}

		north_beams_load
	}

	fn cycle(&mut self) -> u64 {
		_ = self.tilt(TiltDir::North);
		_ = self.tilt(TiltDir::West);
		_ = self.tilt(TiltDir::South);
		self.tilt(TiltDir::East)
	}
}


fn input_platform() -> Platform {
	include_str!("day14.txt").parse().unwrap()
}


fn part1_impl(mut input_platform: Platform) -> u64 {
	input_platform.tilt(TiltDir::North)
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_platform())
}


fn part2_impl(mut input_platform: Platform) -> u64 {
	let mut prevs = Some(std::collections::HashMap::new());
	let mut i = 0;
	let mut north_beams_load = 0;
	while i < 1_000_000_000 {
		north_beams_load = input_platform.cycle();
		i += 1;
		if let Some(prev_i) = prevs.as_mut().and_then(|p| p.insert(input_platform.clone(), i)) {
			let d = i - prev_i;
			i = prev_i + ((1_000_000_000 - prev_i) / d) * d;
			prevs = None;
			continue
		}
	}
	north_beams_load
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_platform())
}


mod parsing {
	use std::str::FromStr;
	use super::{Platform, Space};

	impl TryFrom<u8> for Space {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value {
				b'.' => Ok(Space::Empty),
				b'#' => Ok(Space::Cube),
				b'O' => Ok(Space::Round),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum PlatformError {
		Format { line: usize },
		Space { line: usize, column: usize, invalid: u8 },
	}

	impl FromStr for Platform {
		type Err = PlatformError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			s.lines()
				.enumerate()
				.try_fold(Platform { spaces: Vec::default(), stride: 0 }, |mut acc, (l, line)| {
					let stride = line.len();
					if stride == 0 || acc.spaces.len() % stride != 0 {
						return Err(PlatformError::Format { line: l + 1 }) }

					if acc.stride == 0 { acc.stride = stride }
					acc.spaces.reserve(stride);
					for (c, byte) in line.bytes().enumerate() {
						acc.spaces.push(Space::try_from(byte).map_err(|_|
							PlatformError::Space { line: l + 1, column: c + 1, invalid: byte })?);
					}
					Ok(acc)
				})
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for Platform {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;

			for y in 0..self.spaces.len() / self.stride {
				if y > 0 { f.write_char('\n')? }
				for space in &self.spaces[y * self.stride..(y + 1) * self.stride] {
					match space {
						Space::Empty => f.write_char('.')?,
						Space::Cube => f.write_char('#')?,
						Space::Round => f.write_char('O')?,
					}
				}
			}
			Ok(())
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		O....#....
		O.OO#....#
		.....##...
		OO.#O....O
		.O.....O#.
		O.#..O.#.#
		..O..#O..O
		.......O..
		#....###..
		#OO..#....
	"};
	assert_eq!(part1_impl(INPUT.parse::<Platform>().unwrap()), 136);
	assert_eq!(part1(), 105784);
	assert_eq!(part2_impl(INPUT.parse::<Platform>().unwrap()), 64);
	assert_eq!(part2(), 91286);
}
