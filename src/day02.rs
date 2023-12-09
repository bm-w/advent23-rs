// Copyright (c) 2023 Bastiaan Marinus van de Weerd

use std::num::NonZeroUsize;


#[derive(Default)]
#[cfg_attr(test, derive(Debug))]
struct Set {
	red: Option<NonZeroUsize>,
	green: Option<NonZeroUsize>,
	blue: Option<NonZeroUsize>,
}

#[cfg_attr(test, derive(Debug))]
struct Game {
	id: u16,
	sets: Vec<Set>,
}


fn input_games() -> impl Iterator<Item = Game> {
	parsing::try_games_from_str(include_str!("day02.txt")).map(|g| g.unwrap())
}


fn part1_impl(input_games: impl Iterator<Item = Game>) -> u64 {
	input_games
		.filter(|game| game.sets.iter()
			.all(|set| {
				macro_rules! all_lte { ( $( $color:ident <= $rhs:literal ),+ ) => {
					true $( && set.$color.map(|n| n.get()).unwrap_or(0) <= $rhs )+ }; }
				all_lte!(red <= 12, green <= 13, blue <= 14)
			}))
		.map(|g| g.id as u64)
		.sum()
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_games())
}


fn part2_impl(input_games: impl Iterator<Item = Game>) -> u64 {
	input_games
		.map(|game| {
			let min_set = game.sets.iter().fold(Set::default(), |mut acc, set| {
				macro_rules! set_max { ( $( $color:ident ),+ ) => { $(
					if let Some(count) = set.$color {
						if count.get() > acc.$color.map(|n| n.get()).unwrap_or(0) {
							acc.$color = Some(count)
						}
					}
				)+ }; }
				set_max!(red, green, blue);
				acc
			});
			macro_rules! power { ( $( $color:ident ),+ ) => {
				1 $( * min_set.$color.map(|n| n.get() as u64).unwrap_or(0) )+ }; }
			power!(red, green, blue)
		})
		.sum()
}

pub(crate) fn part2() -> u64 {
	part2_impl(input_games())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Game, Set};

	#[derive(Debug)]
	pub(super) enum SetError {
		Format,
		Count(ParseIntError),
		Color(String),
	}

	impl FromStr for Set {
		type Err = SetError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use SetError as E;

			let mut set = Set::default();
			for s in s.split(", ") {
				let (count, color) = s.split_once(' ').ok_or(E::Format)?;
				let count = count.parse().map_err(E::Count)?;
				match color {
					"red" => { set.red = Some(count); }
					"green" => { set.green = Some(count); }
					"blue" => { set.blue = Some(count); }
					invalid => return Err(E::Color(invalid.to_owned()))
				}
			}
			Ok(set)
		}
	}

	#[derive(Debug)]
	pub(super) enum GameError {
		Format,
		Id(ParseIntError),
		Set(SetError),
	}

	impl FromStr for Game {
		type Err = GameError;
		fn from_str(mut s: &str) -> Result<Self, Self::Err> {
			use GameError as E;

			s = s.strip_prefix("Game ").ok_or(E::Format)?;
			let (id, s) = s.split_once(": ").ok_or(E::Format)?;
			let id = id.parse().map_err(E::Id)?;
			let mut sets = Vec::new();
			for s in s.split("; ") {
				sets.push(s.parse().map_err(E::Set)?);
			}
			Ok(Game { id, sets })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct GamesError {
		line: usize,
		source: GameError
	}

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_games_from_str(s: &str) -> impl Iterator<Item = Result<Game, GamesError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|source| GamesError { line: l + 1, source }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
		Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
		Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
		Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
		Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
	"};
	assert_eq!(part1_impl(parsing::try_games_from_str(INPUT).map(|g| g.unwrap())), 8);
	assert_eq!(part1(), 2406);
	assert_eq!(part2_impl(parsing::try_games_from_str(INPUT).map(|g| g.unwrap())), 2286);
	assert_eq!(part2(), 78375);
}
