// Copyright (c) 2023 Bastiaan Marinus van de Weerd


#[cfg_attr(test, derive(Debug))]
struct Race {
	time: u64,
	dist: u64,
}

fn next_pow10(val: u64) -> u64 {
	let mut next = 1;
	while next < val { next *= 10 }
	next
}


fn input_races() -> impl Iterator<Item = Race> {
	parsing::try_races_from_str(include_str!("day06.txt")).map(|r| r.unwrap())
}


fn part1_impl(input_races: impl Iterator<Item = Race>) -> u64 {
	input_races
		.map(|race| {
			fn find(race: &Race, mut helds: impl Iterator<Item = u64>) -> u64 {
				helds.find(|held| held * (race.time - held) > race.dist).unwrap()
			}
			let min = find(&race, 1..race.time);
			let max = find(&race, (1..race.time).rev());
			max - min + 1
		})
		.product()
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_races())
}


fn part2_impl(input_races: impl Iterator<Item = Race>) -> u64 {

	let race = input_races.fold(Race { time: 0, dist: 0 }, |acc, race| Race {
		time: acc.time * next_pow10(race.time) + race.time,
		dist: acc.dist * next_pow10(race.dist) + race.dist,
	});

	fn find(race: &Race, mut helds: impl Iterator<Item = u64>) -> u64 {
		helds.find(|held| held.checked_mul(race.time - held)
			.map(|dist| dist > race.dist)
			.unwrap_or(true))
			.unwrap()
	}
	let min = find(&race, 1..race.time);
	let max = find(&race, (1..race.time).rev());

	max - min + 1
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_races())
}


mod parsing {
	use std::num::ParseIntError;
	use super::Race;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RaceError {
		Time(ParseIntError),
		Dist(ParseIntError),
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RacesError {
		Format,
		Race { offset: usize, source: RaceError },
	}

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_races_from_str(s: &str) -> impl Iterator<Item = Result<Race, RacesError>> + '_ {
		use RacesError as EE;
		let Some(s) = s.strip_prefix("Time:") else {
			return itertools::Either::Left([Err(EE::Format)].into_iter())
		};
		let s = s.trim_start_matches(' ');
		let Some((times, dists)) = s.split_once("\nDistance:") else {
			return itertools::Either::Left([Err(EE::Format)].into_iter())
		};
		let dists = dists.trim_start_matches(' ').trim_end_matches('\n');

		itertools::Either::Right(
			std::iter::zip(
				times.split_ascii_whitespace(),
				dists.split_ascii_whitespace())
			.enumerate()
			.map(|(i, (time, dist))| {
				use RaceError as E;

				let time = time.parse()
					.map_err(|e| EE::Race { offset: i, source: E::Time(e) })?;
				let dist = dist.parse()
					.map_err(|e| EE::Race { offset: i, source: E::Time(e) })?;
				Ok(Race { time, dist })
			}))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		Time:      7  15   30
		Distance:  9  40  200
	"};
	assert_eq!(part1_impl(parsing::try_races_from_str(INPUT).map(|r| r.unwrap())), 288);
	assert_eq!(part1(), 1108800);
	assert_eq!(part2_impl(parsing::try_races_from_str(INPUT).map(|r| r.unwrap())), 71503);
	assert_eq!(part2(), 36919753);
}
