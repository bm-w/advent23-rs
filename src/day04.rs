// Copyright (c) 2023 Bastiaan Marinus van de Weerd


struct Card {
	#[allow(dead_code)]
	id: u8,
	winning: Vec<u8>,
	owned: Vec<u8>,
}

impl Card {
	fn matches(&self) -> usize {
		self.owned.iter().filter(|&n| self.winning.contains(n)).count()
	}
}


fn input_cards() -> impl Iterator<Item = Card> {
	parsing::try_cards_from_str(include_str!("day04.txt")).map(|r| r.unwrap())
}


fn part1_impl(input_cards: impl Iterator<Item = Card>) -> u64 {
	let mut acc = 0;

	for card in input_cards {
		let matches = card.matches();
		if matches == 0 { continue }
		acc += 1 << (matches - 1);
	}

	acc
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_cards())
}


fn part2_impl(input_cards: impl Iterator<Item = Card>) -> usize {
	let mut input_cards_counts = input_cards.map(|card| (card, 1)).collect::<Vec<_>>();

	for i in 0..input_cards_counts.len() {
		let &(ref card, count) = &input_cards_counts[i];
		println!("{} ({count}): {}", card.id, card.matches());
		for k in i + 1..(i + card.matches() + 1).min(input_cards_counts.len()) {
			dbg!(k);
			input_cards_counts[k].1 += count;
		}
	}

	input_cards_counts.iter().map(|(_, count)| *count).sum()
}

pub(crate) fn part2() -> usize {
	part2_impl(input_cards())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Card;

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum CardError {
		Format,
		Id(ParseIntError),
		Number { winning: bool, offset: usize, source: ParseIntError },
	}

	impl FromStr for Card {
		type Err = CardError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use CardError as E;

			let s = s.strip_prefix("Card ").ok_or(E::Format)?;
			let (id, s) = s.split_once(':').ok_or(E::Format)?;
			let id = id.trim_start().parse().map_err(E::Id)?;
			let (winning, owned) = s.split_once(" | ").ok_or(E::Format)?;
			fn numbers(s: &str, winning: bool) -> Result<Vec<u8>, E> {
				s.trim().split_ascii_whitespace()
					.enumerate()
					.map(|(i, num)| num.parse()
						.map_err(|e| E::Number { winning, offset: i + 1, source: e }))
					.collect()
			}
			let winning = numbers(winning, true)?;
			let owned = numbers(owned, false)?;

			Ok(Card { id, winning, owned })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct CardsError {
		line: usize,
		source: CardError
	}

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_cards_from_str(s: &str) -> impl Iterator<Item = Result<Card, CardsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| CardsError { line: l + 1, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
		Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
		Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
		Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
		Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
		Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
	"};
	assert_eq!(part1_impl(parsing::try_cards_from_str(INPUT).map(|r| r.unwrap())), 13);
	assert_eq!(part1(), 15268);
	assert_eq!(part2_impl(parsing::try_cards_from_str(INPUT).map(|r| r.unwrap())), 30);
	assert_eq!(part2(), 6283755);
}
