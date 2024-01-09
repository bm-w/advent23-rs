// Copyright (c) 2023 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
#[cfg_attr(test, derive(Debug))]
enum Spring { Operational, Damaged, Unknown }

#[cfg_attr(test, derive(Debug))]
struct Row {
	springs: Vec<Spring>,
	damaged_groups: Vec<usize>,
}

type Cache = std::collections::HashMap<(usize, usize, Option<usize>), usize>;

impl Row {
	fn count_arrangements_with_cache(&self,
		mut springs_offset: usize,
		mut damaged_groups_offset: usize,
		mut damaged_group_remaining: Option<usize>,
		mut cache: Option<&mut Cache>,
	) -> usize {
		let cache_key = (springs_offset, damaged_groups_offset, damaged_group_remaining);
		if let Some(&count) = cache.as_ref().and_then(|c| c.get(&cache_key)) { return count }

		macro_rules! cache_and_return {
			($result:expr) => {
				return {
					let count = $result;
					if let Some(cache) = cache.as_mut() { cache.insert(cache_key, count); }
					count
				}
			};
		}

		loop {
			if springs_offset == self.springs.len() {
				cache_and_return!(if damaged_groups_offset == self.damaged_groups.len()
					&& matches!(damaged_group_remaining, None | Some(0)) { 1 } else { 0 })
			}

			let leaving_damaged_group = if matches!(damaged_group_remaining, Some(0)) {
				damaged_group_remaining = None;
				true
			} else {
				false
			};

			match &self.springs[springs_offset] {
				Spring::Operational if damaged_group_remaining.is_some() => cache_and_return!(0),
				Spring::Operational => {}
				Spring::Damaged | Spring::Unknown if damaged_group_remaining.is_some() =>
					*damaged_group_remaining.as_mut().unwrap() -= 1,
				Spring::Damaged => {
					if leaving_damaged_group || damaged_groups_offset == self.damaged_groups.len()
						{ cache_and_return!(0) }
					damaged_group_remaining = Some(self.damaged_groups[damaged_groups_offset] - 1);
					damaged_groups_offset += 1;
				}
				Spring::Unknown if leaving_damaged_group
					|| damaged_groups_offset == self.damaged_groups.len() => {}
				Spring::Unknown => {
					let skip = self.count_arrangements_with_cache(
						springs_offset + 1, damaged_groups_offset, None, cache.as_deref_mut());
					let begin = self.count_arrangements_with_cache(
						springs_offset + 1, damaged_groups_offset + 1,
						Some(self.damaged_groups[damaged_groups_offset] - 1), cache.as_deref_mut());
					cache_and_return!(skip + begin)
				}
			}

			springs_offset += 1;
		}
	}
}


fn input_rows() -> impl Iterator<Item = Row> {
	parsing::try_rows_from_str(include_str!("day12.txt")).map(|r| r.unwrap())
}


fn part1_impl(input_rows: impl Iterator<Item = Row>) -> usize {
	input_rows.map(|input_row| input_row.count_arrangements_with_cache(0, 0, None, None)).sum()
}

pub(crate) fn part1() -> usize {
	part1_impl(input_rows())
}


fn part2_impl(input_rows: impl Iterator<Item = Row>) -> usize {
	let mut cache = std::collections::HashMap::new();
	input_rows.map(|mut input_row| {
		let springs_range = 0..input_row.springs.len();
		input_row.springs.reserve(4 * (springs_range.len() + 1));
		let damaged_groups_range = 0..input_row.damaged_groups.len();
		input_row.damaged_groups.reserve(4 * damaged_groups_range.len());
		for _ in 0..4 {
			input_row.springs.push(Spring::Unknown);
			for i in springs_range.clone() { input_row.springs.push(input_row.springs[i]) }
			for i in damaged_groups_range.clone() { input_row.damaged_groups
				.push(input_row.damaged_groups[i]) }
		}

		let count = input_row.count_arrangements_with_cache(0, 0, None, Some(&mut cache));
		cache.clear();
		count
	}).sum()
}

pub(crate) fn part2() -> usize {
	part2_impl(input_rows())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr, fmt::Write};
	use super::{Row, Spring};

	impl TryFrom<u8> for Spring {
		type Error = ();
		fn try_from(byte: u8) -> Result<Self, Self::Error> {
			match byte {
				b'.' => Ok(Spring::Operational),
				b'#' => Ok(Spring::Damaged),
				b'?' => Ok(Spring::Unknown),
				_ => Err(()),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RowError {
		Format,
		Spring { offset: usize, source: u8 },
		DamagedGroup { offset: usize, source: ParseIntError },
	}

	impl FromStr for Row {
		type Err = RowError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use RowError as E;

			let (springs, damaged_groups) = s.split_once(' ').ok_or(E::Format)?;
			let springs = springs.bytes()
				.enumerate()
				.map(|(c, byte)| Spring::try_from(byte)
					.map_err(|_| E::Spring { offset: c, source: byte }))
				.collect::<Result<Vec<_>, _>>()?;
			let damaged_groups = damaged_groups.split(',')
				.enumerate()
				.map(|(i, damaged_group)| damaged_group.parse()
					.map_err(|e| E::DamagedGroup { offset: i, source: e }))
				.collect::<Result<Vec<_>, _>>()?;

			Ok(Row { springs, damaged_groups })
		}
	}

	impl std::fmt::Display for Row {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			for spring in &self.springs { match spring {
				Spring::Operational => f.write_char('.')?,
				Spring::Damaged => f.write_char('#')?,
				Spring::Unknown => f.write_char('?')?,
			} }
			f.write_char(' ')?;
			let mut first = true;
			for damaged_group in &self.damaged_groups {
				if !first { f.write_char(',')?; } else { first = false }
				write!(f, "{damaged_group}")?;
			}
			Ok(())
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct RowsError { line: usize, source: RowError }

	// FIXME: The `+ '_` is  not technically correct (see one of Jon Gjengset’s recent videos)
	pub(super) fn try_rows_from_str(s: &str) -> impl Iterator<Item = Result<Row, RowsError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| RowsError { line: l + 1, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		???.### 1,1,3
		.??..??...?##. 1,1,3
		?#?#?#?#?#?#?#? 1,3,1,6
		????.#...#... 4,1,1
		????.######..#####. 1,6,5
		?###???????? 3,2,1
	"};
	assert_eq!(part1_impl(parsing::try_rows_from_str(INPUT).map(|r| r.unwrap())), 21);
	assert_eq!(part1(), 7344);
	assert_eq!(part2_impl(parsing::try_rows_from_str(INPUT).map(|r| r.unwrap())), 525152);
	assert_eq!(part2(), 1088006519007);
}
