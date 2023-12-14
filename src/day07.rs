// Copyright (c) 2023 Bastiaan Marinus van de Weerd


#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Card { Two = 0, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace }

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq)]
struct Hand<const JACKS_ARE_JOKERS: bool> {
	cards: [Card; 5],
	bid: usize,
}

impl<const JACKS_ARE_JOKERS: bool> PartialOrd for Hand<JACKS_ARE_JOKERS> {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl<const JACKS_ARE_JOKERS: bool> Ord for Hand<JACKS_ARE_JOKERS> {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let mut counts = [(0u8, 0u8); 13]; // Self’s and other’s cards; twos first, etc.
		for card in &self.cards { counts[*card as usize].0 += 1; }
		for card in &other.cards { counts[*card as usize].1 += 1; }

		let jokers_counts =
			if JACKS_ARE_JOKERS { std::mem::take(&mut counts[Card::Jack as usize]) }
			else { (0, 0) };

		let mut n_of_a_kinds = [(0u8, 0u8); 6]; // Ones, twos, threes, fours, and fives
		for (zelf, other) in counts {
			if matches!(zelf, 1..=5) { n_of_a_kinds[zelf as usize - 1].0 += 1 }
			if matches!(other, 1..=5) { n_of_a_kinds[other as usize - 1].1 += 1 }
		}

		fn second_rule<const JACKS_ARE_JOKERS: bool>(zelf: &[Card; 5], other: &[Card; 5])
		-> std::cmp::Ordering {
			for (zelf, other) in std::iter::zip(zelf, other) {
				if JACKS_ARE_JOKERS { match (zelf, other) {
					(Card::Jack, Card::Jack) => continue,
					(Card::Jack, _) => return std::cmp::Ordering::Less,
					(_, Card::Jack) => return std::cmp::Ordering::Greater,
					_ => (),
				}}
				if zelf < other { return std::cmp::Ordering::Less }
				if zelf > other { return std::cmp::Ordering::Greater }
			}
			std::cmp::Ordering::Equal
		}

		macro_rules! has_k_n_of_kind {
			( $access:tt, $k:literal, $n:expr, $jokers_counts:expr ) => { {
				let i = ($n as usize - 1).checked_sub($jokers_counts.$access as usize).unwrap_or(0);
				n_of_a_kinds[i].$access >= $k || $k == 1 && $jokers_counts.$access == $n
			} };
		}
		macro_rules! has_n_of_kind {
			( $access:tt, $n:literal, $jokers_counts:expr ) =>
				{ has_k_n_of_kind!($access, 1, $n, $jokers_counts) };
		}

		macro_rules! have_k_n_of_kind { ( $k:literal, $n:expr, $jokers_counts:expr ) => { (
			has_k_n_of_kind!(0, $k, $n, $jokers_counts),
			has_k_n_of_kind!(1, $k, $n, $jokers_counts)
		) }; }
		macro_rules! have_n_of_kind {
			( $n:literal, $jokers_counts:expr ) =>
				{ (has_n_of_kind!(0, $n, $jokers_counts), has_n_of_kind!(1, $n, $jokers_counts)) };
			( $n:literal ) => { have_n_of_kind!($n, jokers_counts) };
		}

		macro_rules! cmp { ( $checks:expr ) => { match $checks {
			(true, true) => return second_rule::<JACKS_ARE_JOKERS>(&self.cards, &other.cards),
			(_, true) => return std::cmp::Ordering::Less,
			(true, _) => return std::cmp::Ordering::Greater,
			_ => (),
		} }; }

		cmp!(have_n_of_kind!(5));
		cmp!(have_n_of_kind!(4));
		cmp!({
			let threes = have_n_of_kind!(3, (0, 0));
			let threes = if JACKS_ARE_JOKERS {
				let two_pairs = have_k_n_of_kind!(2, 2, (0, 0));
				(threes.0 || jokers_counts.0 == 1 && two_pairs.0,
					threes.1 || jokers_counts.1 == 1 && two_pairs.1)
			} else { threes };
			let twos = have_n_of_kind!(2, (0, 0));
			(threes.0 && twos.0, threes.1 && twos.1)
		});
		cmp!(have_n_of_kind!(3));
		cmp!({
			// Any jokers here would have already triggered a previous rule
			have_k_n_of_kind!(2, 2, (0, 0))
		});
		cmp!(have_n_of_kind!(2));

		assert_eq!(jokers_counts, (0, 0));

		second_rule::<false>(&self.cards, &other.cards)
	}
}


fn input_hands<const JACKS_ARE_JOKERS: bool>() -> impl Iterator<Item = Hand<JACKS_ARE_JOKERS>> {
	parsing::try_hands_from_str(include_str!("day07.txt")).map(|r| r.unwrap())
}


fn part1and2_impl<const JACKS_ARE_JOKERS: bool>(
	input_hands: impl Iterator<Item = Hand<JACKS_ARE_JOKERS>>
) -> u64 {
	let mut input_hands = input_hands.collect::<Vec<_>>();
	input_hands.sort();
	input_hands.into_iter().enumerate().map(|(i, hand)| (i as u64 + 1) * hand.bid as u64).sum()
}

pub(crate) fn part1() -> u64 {
	part1and2_impl::<false>(input_hands())
}

pub(crate) fn part2() -> u64 {
	part1and2_impl::<true>(input_hands())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Card, Hand};

	impl TryFrom<u8> for Card {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			use Card::*;
			match value {
				b'A' => Ok(Ace),
				b'K' => Ok(King),
				b'Q' => Ok(Queen),
				b'J' => Ok(Jack),
				b'T' => Ok(Ten),
				b'9' => Ok(Nine),
				b'8' => Ok(Eight),
				b'7' => Ok(Seven),
				b'6' => Ok(Six),
				b'5' => Ok(Five),
				b'4' => Ok(Four),
				b'3' => Ok(Three),
				b'2' => Ok(Two),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum HandError {
		Format,
		Card { offset: usize, source: u8 },
		CardsLen(usize),
		Bid(ParseIntError)
	}

	impl<const JACKS_ARE_JOKERS: bool> FromStr for Hand<JACKS_ARE_JOKERS> {
		type Err = HandError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use HandError as E;

			let (cards, bid) = s.split_once(' ').ok_or(E::Format)?;
			if cards.len() != 5 { return Err(E::Format) }
			let cards = cards.bytes()
				.enumerate()
				.map(|(i, byte)| Card::try_from(byte)
					.map_err(|_| E::Card { offset: i, source: byte }))
				// FIXME: Collect without allocation
				.collect::<Result<Vec<_>, _>>()?
				.try_into().map_err(|cc: Vec<_>| E::CardsLen(cc.len()))?;
			let bid = bid.parse().map_err(E::Bid)?;

			Ok(Hand { cards, bid })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct HandsError {
		line: usize,
		source: HandError
	}

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_hands_from_str<const JACKS_ARE_JOKERS: bool>(s: &str)
	-> impl Iterator<Item = Result<Hand<JACKS_ARE_JOKERS>, HandsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| HandsError { line: l + 1, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		32T3K 765
		T55J5 684
		KK677 28
		KTJJT 220
		QQQJA 483
	"};
	assert_eq!(part1and2_impl::<false>(parsing::try_hands_from_str(INPUT).map(|r| r.unwrap())), 6440);
	assert_eq!(part1(), 246163188);
	assert_eq!(part1and2_impl::<true>(parsing::try_hands_from_str(INPUT).map(|r| r.unwrap())), 5905);
	assert_eq!(part2(), 245794069);
}
