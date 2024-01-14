// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Copy)]
enum Cat { ExCool = 0, Musical = 1, Aero = 2, Shiny = 3 }

enum Res<'w> { Send(&'w str), Accept, Reject }

enum Op { Lt, Gt }

struct Rule<'w> { cat: Cat, op: Op, val: u64, res: Res<'w> }

struct Workflow<'w> { name: &'w str, rules: Vec<Rule<'w>>, res: Res<'w> }

struct Part { ratings: [u64; 4] }

impl Rule<'_> {
	fn part_res(&self, part: &Part) -> Option<&Res<'_>> {
		let rating = part.ratings[self.cat as usize];
		(match self.op {
			Op::Lt => rating < self.val,
			Op::Gt => rating > self.val,
		}).then_some(&self.res)
	}

	fn consume_ranges_to_res(&self, ratings_ranges: &mut [std::ops::Range<u16>; 4])
	-> Option<(&Res<'_>, [std::ops::Range<u16>; 4])> {
		let mut consumed_ratings_ranges = ratings_ranges.clone();
		let remaining_rating_range = &mut ratings_ranges[self.cat as usize];
		let consumed_rating_range = &mut consumed_ratings_ranges[self.cat as usize];
		let val = self.val as u16;
		match self.op {
			_ if std::ops::Range::is_empty(remaining_rating_range) => return None,
			Op::Lt if remaining_rating_range.end <= val => {
				*remaining_rating_range = 0..0
			}
			Op::Lt if remaining_rating_range.start < val => {
				remaining_rating_range.start = val;
				consumed_rating_range.end = val;
			}
			Op::Gt if remaining_rating_range.start > val => {
				*remaining_rating_range = 0..0
			}
			Op::Gt if remaining_rating_range.end > val + 1 => {
				remaining_rating_range.end = val + 1;
				consumed_rating_range.start = val + 1;
			}
			_ => return None
		}
		Some((&self.res, consumed_ratings_ranges))
	}
}

impl Workflow<'_> {
	fn part_res(&self, part: &Part) -> &Res<'_> {
		for rule in &self.rules {
			if let Some(res) = rule.part_res(part) { return res }
		}
		&self.res
	}

	fn consume_ranges_to_res(&self, ratings_ranges: &mut [std::ops::Range<u16>; 4])
	-> (&Res<'_>, [std::ops::Range<u16>; 4]) {
		for rule in &self.rules {
			if let Some((res, consumed_ratings_ranges))
				= rule.consume_ranges_to_res(ratings_ranges)
				{ return (res, consumed_ratings_ranges) }
		}
		let res_ratings_ranges = ratings_ranges.clone();
		*ratings_ranges = std::array::from_fn(|_| 0..0);
		(&self.res, res_ratings_ranges)
	}
}

impl Part {
	fn total_rating(&self) -> u64 {
		self.ratings.into_iter().sum()
	}
}


fn input_workflows_and_parts() -> (Vec<Workflow<'static>>, Vec<Part>) {
	parsing::try_workflows_and_parts_from_str(include_str!("day19.txt")).unwrap()
}


fn part1_impl((input_workflows, input_parts): (Vec<Workflow<'_>>, Vec<Part>)) -> u64 {
	let in_workflow = input_workflows.iter().find(|w| w.name == "in").unwrap();

	let mut total_accepted_rating = 0;
	for part in input_parts {
		let mut workflow = in_workflow;
		loop {
			match workflow.part_res(&part) {
				Res::Accept => { total_accepted_rating += part.total_rating(); break }
				Res::Reject => break,
				&Res::Send(name) => {
					workflow = input_workflows.iter().find(|w| w.name == name).unwrap();
					continue
				}
			}
		}
	}
	total_accepted_rating
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_workflows_and_parts())
}


fn part2_impl(input_workflows: Vec<Workflow<'_>>) -> usize {
	let mut total_accepted_combinations = 0;

	let in_workflow = input_workflows.iter().find(|w| w.name == "in").unwrap();
	let mut stack = vec![(in_workflow, std::array::from_fn::<_, 4, _>(|_| 1..4001))];

	while let Some((workflow, ratings_ranges)) = stack.last_mut() {
		if ratings_ranges.iter().all(std::ops::Range::is_empty) { _ = stack.pop(); continue }
		match workflow.consume_ranges_to_res(ratings_ranges) {
			(Res::Accept, consumed_ratings_ranges) => {
				total_accepted_combinations
					+= consumed_ratings_ranges.iter().map(std::ops::Range::len).product::<usize>();
			}
			(Res::Reject, _) => {}
			(&Res::Send(name), consumed_ratings_ranges) => {
				let workflow = input_workflows.iter().find(|w| w.name == name).unwrap();
				stack.push((workflow, consumed_ratings_ranges));
			}
		}
	}

	total_accepted_combinations
}

pub(crate) fn part2() -> usize {
	part2_impl(input_workflows_and_parts().0)
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::{Cat, Res, Op, Rule, Workflow, Part};

	impl TryFrom<u8> for Cat {
		type Error = ();
		fn try_from(ascii: u8) -> Result<Self, Self::Error> {
			match ascii {
				b'x' => Ok(Cat::ExCool),
				b'm' => Ok(Cat::Musical),
				b'a' => Ok(Cat::Aero),
				b's' => Ok(Cat::Shiny),
				_ => Err(()),
			}
		}
	}

	impl From<Cat> for u8 {
		fn from(cat: Cat) -> Self {
			match cat {
				Cat::ExCool => b'x',
				Cat::Musical => b'm',
				Cat::Aero => b'a',
				Cat::Shiny => b's',
			}
		}
	}

	impl TryFrom<u8> for Op {
		type Error = ();
		fn try_from(ascii: u8) -> Result<Self, Self::Error> {
			match ascii {
				b'<' => Ok(Op::Lt),
				b'>' => Ok(Op::Gt),
				_ => Err(()),
			}
		}
	}

	impl<'s> From<&'s str> for Res<'s> {
		fn from(s: &'s str) -> Self {
			match s.bytes().next() {
				Some(b'A') => Res::Accept,
				Some(b'R') => Res::Reject,
				_ => Res::Send(s),
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum RuleError {
		Format,
		Cat { invalid: u8 },
		Op { invalid: u8 },
		Val(ParseIntError),
	}

	impl<'s> TryFrom<&'s str> for Rule<'s> {
		type Error = RuleError;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			use RuleError as E;

			let mut bytes = s.bytes();
			let cat = bytes.next().ok_or(E::Format)?;
			let cat = Cat::try_from(cat).map_err(|_| E::Cat { invalid: cat })?;
			let op = bytes.next().ok_or(E::Format)?;
			let op = Op::try_from(op).map_err(|_| E::Cat { invalid: op })?;
			let (val, res) = s[2..].split_once(':').ok_or(E::Format)?;
			let val = val.parse().map_err(E::Val)?;

			Ok(Rule { cat, op, val, res: Res::from(res) })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum WorkflowError {
		Format,
		Rule { offset: usize, source: RuleError }
	}

	impl<'s> TryFrom<&'s str> for Workflow<'s> {
		type Error = WorkflowError;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			use WorkflowError as E;

			let (name, s) = s.split_once('{').ok_or(E::Format)?;
			let s = s.strip_suffix('}').ok_or(E::Format)?;
			let mut split = s.split(',');
			let res = Res::from(split.next_back().ok_or(E::Format)?);
			let rules = split.enumerate()
				.map(|(i, rule)| Rule::try_from(rule)
					.map_err(|e| E::Rule { offset: i, source: e }))
				.collect::<Result<Vec<_>, _>>()?;

			Ok(Workflow { name, rules, res })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum PartError {
		Format,
		RatingCat { offset: usize, invald: Option<u8> },
		RatingVal { offset: usize, source: ParseIntError },
	}

	impl FromStr for Part {
		type Err = PartError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use PartError as E;

			let s = s.strip_prefix('{').and_then(|s| s.strip_suffix('}')).ok_or(E::Format)?;
			let mut split = s.split(',');
			let ratings = split.by_ref().take(4)
				.zip([Cat::ExCool, Cat::Musical, Cat::Aero, Cat::Shiny])
				.enumerate()
				.map(|(i, (rating, cat))| {
					let rating = rating.strip_prefix(char::from(u8::from(cat)))
						.ok_or_else(|| E::RatingCat { offset: i, invald: rating.bytes().next() })?;
					let val = rating.strip_prefix('=').ok_or(E::Format)?;
					val.parse::<u64>().map_err(|e| E::RatingVal { offset: i, source: e })
				})
				.collect::<Result<Vec<_>, _>>()?
				.try_into().map_err(|_| E::Format)?;
			if split.next().is_some() { return Err(E::Format) }

			Ok(Part { ratings })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct WorkflowsError { line: usize, source: WorkflowError }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct PartsError { line: usize, source: PartError }

	// TODO: Return iterators
	pub(super) fn try_workflows_and_parts_from_str(s: &str)
	-> Result<(Vec<Workflow<'_>>, Vec<Part>), itertools::Either<WorkflowsError, PartsError>> {
		let mut lines = s.lines().enumerate();

		let workflows = lines.by_ref()
			.take_while(|&(_, line)| !line.is_empty())
			.map(|(l, line)| Workflow::try_from(line)
				.map_err(|e| WorkflowsError { line: l + 1, source: e }))
			.collect::<Result<Vec<_>, _>>()
			.map_err(itertools::Either::Left)?;

		let parts = lines
			.map(|(l, line)| line.parse()
				.map_err(|e| {
					println!("{}: {line}", l + 1);
					PartsError { line: l + 1, source: e }
				}))
			.collect::<Result<Vec<_>, _>>()
			.map_err(itertools::Either::Right)?;

		Ok((workflows, parts))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		px{a<2006:qkq,m>2090:A,rfg}
		pv{a>1716:R,A}
		lnx{m>1548:A,A}
		rfg{s<537:gd,x>2440:R,A}
		qs{s>3448:A,lnx}
		qkq{x<1416:A,crn}
		crn{x>2662:A,R}
		in{s<1351:px,qqz}
		qqz{s>2770:qs,m<1801:hdj,R}
		gd{a>3333:R,R}
		hdj{m>838:A,pv}

		{x=787,m=2655,a=1222,s=2876}
		{x=1679,m=44,a=2067,s=496}
		{x=2036,m=264,a=79,s=2244}
		{x=2461,m=1339,a=466,s=291}
		{x=2127,m=1623,a=2188,s=1013}
	"};
	assert_eq!(part1_impl(parsing::try_workflows_and_parts_from_str(INPUT).unwrap()), 19114);
	assert_eq!(part1(), 425811);
	assert_eq!(part2_impl(parsing::try_workflows_and_parts_from_str(INPUT).unwrap().0),
		167409079868000);
	assert_eq!(part2(), 131796824371749);
}
