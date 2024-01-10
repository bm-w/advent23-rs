// Copyright (c) 2024 Bastiaan Marinus van de Weerd


enum Op {
	Remove,
	Insert { lens_focal_length: u8 }
}

struct Step<'s> {
	label: &'s str,
	op: Op,
}


fn hash(s: &str) -> u8 {
	let mut val = 0;
	for byte in s.bytes() {
		val += byte as u16;
		val *= 17;
		val %= 256;
	}
	val as u8
}


fn input_steps_str() -> &'static str {
	include_str!("day15.txt")
}


fn part1_impl<'s>(steps: impl Iterator<Item = &'s str>) -> u64 {
	steps.map(|step| hash(step) as u64).sum()
}

pub(crate) fn part1() -> u64 {
	part1_impl(parsing::step_strs_from_str(input_steps_str()))
}


fn part2_impl<'s>(steps: impl Iterator<Item = Step<'s>>) -> u64 {
	let mut boxes: [Vec<(&'s str, u8)>; 256] = std::array::from_fn(|_| Vec::new());

	for step in steps {
		let label_hash = hash(step.label);
		let r#box = &mut boxes[label_hash as usize];
		let pos = r#box.iter().position(|&(label, _)| label == step.label);
		match (&step.op, pos) {
			(Op::Remove, None) => {}
			(Op::Remove, Some(pos)) => {
				_ = r#box.remove(pos)
			}
			(&Op::Insert { lens_focal_length }, None) => {
				r#box.push((step.label, lens_focal_length))
			}
			(&Op::Insert { lens_focal_length }, Some(pos)) => {
				r#box[pos].1 = lens_focal_length
			}
		}
		#[cfg(never)]
		println!(r#"After "{step}":{}"#, boxes.iter()
			.enumerate()
			.filter(|(_, b)| !b.is_empty())
			.fold(String::new(), |mut acc, (b, r#box)| {
				use std::fmt::Write as _;
				write!(acc, "\nBox {b}:").unwrap();
				for (label, focal_length) in r#box {
					write!(acc, " [{label} {focal_length}]").unwrap();
				}
				acc
			}))
	}

	boxes.into_iter().enumerate().flat_map(|(b, r#box)|
		r#box.into_iter().enumerate().map(move |(i, (_, lens_focal_length))|
			(1 + b as u64) * (1 + i as u64) * lens_focal_length as u64)).sum()
}


pub(crate) fn part2() -> u64 {
	part2_impl(parsing::try_steps_from_str(input_steps_str()).map(|r| r.unwrap()))
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Op, Step};

	pub(super) fn step_strs_from_str(s: &str) -> impl Iterator<Item = &str> + '_ {
		s.trim().split(',')
	}

	#[derive(Debug)]
	pub(super) enum OpError {
		Format,
		LensFocalLength(ParseIntError),
	}

	impl FromStr for Op {
		type Err = OpError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			let mut chars = s.chars();
			match chars.next() {
				Some('=') => Ok(Op::Insert { lens_focal_length: chars.as_str()
					.parse().map_err(OpError::LensFocalLength)? }),
				Some('-') if chars.next().is_none() => Ok(Op::Remove),
				_ => Err(OpError::Format)
			}
		}
	}
	#[derive(Debug)]
	pub(super) enum StepError {
		Format,
		Op(OpError),
	}

	impl<'s> TryFrom<&'s str> for Step<'s> {
		type Error = StepError;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			let non_lower = s.find(|c: char| !c.is_ascii_lowercase()).ok_or(StepError::Format)?;
			Ok(Step { label: &s[..non_lower], op: s[non_lower..].parse().map_err(StepError::Op)? })
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for Op {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			use std::fmt::Write as _;
			match self {
				Op::Remove => f.write_char('-'),
				Op::Insert { lens_focal_length } => write!(f, "={lens_focal_length}"),
			}
		}
	}

	#[cfg(test)]
	impl std::fmt::Display for Step<'_> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			f.write_str(self.label)?;
			self.op.fmt(f)
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct StepsError { offset: usize, source: StepError }

	pub(super) fn try_steps_from_str(s: &str)
	-> impl Iterator<Item = Result<Step<'_>, StepsError>> + '_ {
		step_strs_from_str(s)
			.enumerate()
			.map(|(i, step)| Step::try_from(step)
				.map_err(|e| StepsError { offset: i, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
	assert_eq!(part1_impl(parsing::step_strs_from_str(INPUT)), 1320);
	assert_eq!(part1(), 518107);
	assert_eq!(part2_impl(parsing::try_steps_from_str(INPUT).map(|r| r.unwrap())), 145);
	assert_eq!(part2(), 303404);
}
