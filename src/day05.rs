// Copyright (c) 2023 Bastiaan Marinus van de Weerd

#[cfg_attr(test, derive(Debug))]
struct Range {
	src: u64,
	dest: u64,
	len: usize,
}

impl Range {
	fn map(&self, src: u64) -> Option<u64> {
		if src < self.src || src >= self.src + self.len as u64 { None }
		else { Some(self.dest + (src - self.src)) }
	}
}
struct Map {
	src: String,
	dest: String,
	ranges: Vec<Range>,
}

impl Map {
	fn map(&self, src: u64) -> Option<u64> {
		self.ranges.iter().find_map(|r| r.map(src))
	}
}

struct Almanac {
	seeds: Vec<u64>,
	maps: Vec<Map>
}

impl Almanac {
	fn location(&self, seed: u64) -> u64 {
		let dest = self.maps.iter().fold((seed, "seed"), |mut acc, map| {
			assert_eq!(map.src, acc.1);
			acc.0 = map.map(acc.0).unwrap_or(acc.0);
			acc.1 = &map.dest;
			acc
		});
		assert_eq!(dest.1, "location");
		dest.0
	}
}


fn input_almanac() -> Almanac {
	include_str!("day05.txt").parse::<Almanac>().unwrap()
}


fn part1_impl(input_almanac: Almanac) -> u64 {
	input_almanac.seeds.iter()
		.map(|&seed| input_almanac.location(seed))
		.min().unwrap()
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_almanac())
}


#[allow(dead_code)]
fn part2_brute(input_almanac: Almanac) -> u64 {
	use itertools::Itertools as _;
	input_almanac.seeds.iter().tuples().map(|(&start, &len)| {
		(start..start + len)
			.map(|seed| input_almanac.location(seed))
			.min().unwrap()
	}).min().unwrap()
}

fn part2_impl(mut input_almanac: Almanac) -> u64 {
	use itertools::Itertools as _;

	for map in &mut input_almanac.maps { map.ranges.sort_by_key(|r| r.src) }
	let input_almanac = input_almanac;

	input_almanac.seeds.iter().tuples().fold(u64::MAX, |mut loc, (&start, &len)| {
		let mut rem = Vec::from([(start, len)]);
		while !rem.is_empty() {
			for (i, map) in input_almanac.maps.iter().enumerate().skip(rem.len() - 1) {
				let (src, src_len) = &mut rem[i];

				let (range, next_range) = {
					let next_r = map.ranges.iter().position(|r| r.src > *src);
					let r = if let Some(next_r) = next_r { next_r.checked_sub(1) }
						else { Some(map.ranges.len() - 1) };
					(r.map(|r| &map.ranges[r]), next_r.map(|r| &map.ranges[r]))
				};

				let dest = range
					.and_then(|r|
						if *src >= r.src + r.len as u64 { None }
						else { Some(r.dest + (*src - r.src)) })
					.unwrap_or(*src);
				let dest_len = (*src_len)
					.min(next_range.map(|r| r.src - *src).unwrap_or(u64::MAX));
				*src += dest_len;
				*src_len -= dest_len;

				rem.push((dest, dest_len))
			}

			loc = loc.min(rem.pop().unwrap().0);

			while rem.last().map(|(_, len)| *len == 0).unwrap_or(false) {
				_ = rem.pop();
			}
		}
		loc
	})
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_almanac())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Almanac, Map, Range};

	#[derive(Debug)]
	pub(super) enum RangeError {
		Format,
		Src(ParseIntError),
		Dest(ParseIntError),
		Len(ParseIntError),
	}

	impl FromStr for Range {
		type Err = RangeError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use RangeError as E;

			let (dest, s) = s.split_once(' ').ok_or(E::Format)?;
			let dest = dest.parse().map_err(E::Src)?;
			let (src, len) = s.split_once(' ').ok_or(E::Format)?;
			let src = src.parse().map_err(E::Dest)?;
			let len = len.parse().map_err(E::Len)?;

			Ok(Range { src, dest, len })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError {
		Format,
		Range { offset: usize, source: RangeError },
	}

	impl FromStr for Map {
		type Err = MapError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use MapError as E;

			let (src, s) = s.split_once("-to-").ok_or(E::Format)?;
			let (dest, s) = s.split_once(" map:\n").ok_or(E::Format)?;
			let ranges = s.lines()
				.enumerate()
				.map(|(l, line)| line.parse::<Range>()
					.map_err(|e| E::Range { offset: l, source: e }))
				.collect::<Result<_, _>>()?;

			Ok(Map { src: src.to_owned(), dest: dest.to_owned(), ranges })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum AlmanacError {
		Format,
		Seed { offset: usize, source: ParseIntError },
		Map { offset: usize, source: MapError },
	}

	impl FromStr for Almanac {
		type Err = AlmanacError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use AlmanacError as E;

			let s = s.strip_prefix("seeds: ").ok_or(E::Format)?;
			let (seeds, s) = s.split_once("\n\n").ok_or(E::Format)?;
			let seeds = seeds.split(' ')
				.enumerate()
				.map(|(i, seed)| seed.parse::<u64>()
					.map_err(|e| E::Seed { offset: i, source: e }))
				.collect::<Result<_, _>>()?;
			let maps = s.split("\n\n")
				.enumerate()
				.map(|(i, map)| map.parse::<Map>()
					.map_err(|e| E::Map { offset: i, source: e }))
				.collect::<Result<_, _>>()?;

			Ok(Almanac { seeds, maps })
		}
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		seeds: 79 14 55 13

		seed-to-soil map:
		50 98 2
		52 50 48

		soil-to-fertilizer map:
		0 15 37
		37 52 2
		39 0 15

		fertilizer-to-water map:
		49 53 8
		0 11 42
		42 0 7
		57 7 4

		water-to-light map:
		88 18 7
		18 25 70

		light-to-temperature map:
		45 77 23
		81 45 19
		68 64 13

		temperature-to-humidity map:
		0 69 1
		1 0 69

		humidity-to-location map:
		60 56 37
		56 93 4
	"};
	assert_eq!(part1_impl(INPUT.parse::<Almanac>().unwrap()), 35);
	assert_eq!(part1(), 240320250);
	assert_eq!(part2_brute(INPUT.parse::<Almanac>().unwrap()), 46);
	assert_eq!(part2_impl(INPUT.parse::<Almanac>().unwrap()), 46);
	assert_eq!(part2(), 28580589);
}
