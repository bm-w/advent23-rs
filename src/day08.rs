// Copyright (c) 2023 Bastiaan Marinus van de Weerd

use std::collections::HashMap;


#[derive(Clone, Copy)]
enum Instr { Left, Right }

struct Node<'s>([&'s str; 2]);

struct Map<'s> {
	instrs: Vec<Instr>,
	nodes: HashMap<&'s str, Node<'s>>,
}


fn input_map() -> Map<'static> {
	include_str!("day08.txt").try_into().unwrap()
}


fn part1_impl(input_map: Map<'_>) -> usize {
	input_map.instrs.iter().cycle().try_fold(("AAA", 0), |acc, instr| {
		let dests = &input_map.nodes[acc.0].0;
		match instr {
			Instr::Left if dests[0] == "ZZZ" => Err(acc.1 + 1),
			Instr::Left => Ok((dests[0], acc.1 + 1)),
			Instr::Right if dests[1] == "ZZZ" => Err(acc.1 + 1),
			Instr::Right => Ok((dests[1], acc.1 + 1)),
		}
	}).unwrap_err()
}

pub(crate) fn part1() -> usize {
	part1_impl(input_map())
}


#[cfg(test)]
fn part2_brute(input_map: Map<'_>) -> usize {
	let ghosts = input_map.nodes.keys()
		.copied()
		.filter(|label| label.ends_with('A'))
		.collect::<Vec<_>>();

	input_map.instrs.iter().cycle().try_fold((ghosts, 0), |mut acc, instr| {
		let mut unfinished = false;
		for ghost in &mut acc.0 {
			let dests = &input_map.nodes[*ghost].0;
			*ghost = dests[match instr { Instr::Left => 0, Instr::Right => 1 }];
			if !ghost.ends_with('Z') { unfinished = true; }
		}
		acc.1 += 1;
		if !unfinished { return Err(acc.1) }
		Ok(acc)
	}).unwrap_err()
}

fn part2_impl(input_map: Map<'_>) -> usize {
	let ghosts = input_map.nodes.keys()
		.copied()
		.filter(|label| label.ends_with('A'))
		.collect::<Vec<_>>();

	let cycles = ghosts.iter().map(|&(mut ghost)| {
		let mut steps = Vec::new();
		let mut prev_step = 0;
		for (step, instr) in input_map.instrs.iter().cycle().enumerate() {
			let dests = &input_map.nodes[ghost].0;
			ghost = dests[match instr { Instr::Left => 0, Instr::Right => 1 }];
			if !ghost.ends_with('Z') { continue }
			let instr_step = step % input_map.instrs.len();
			let cycled = steps.iter().any(|&(g, is, _)| g == ghost && is == instr_step);
			steps.push((ghost, instr_step, 1 + step - prev_step));
			if cycled { break }
			prev_step = 1 + step;
		}
		// TODO: Is this property part of he puzzle’s design?
		assert_eq!(steps.first().unwrap().2, steps.last().unwrap().2);
		steps
	}).collect::<Vec<_>>();

	cycles.into_iter().fold(1, |acc, steps| { num_integer::lcm(acc, steps[0].2) })
}

pub(crate) fn part2() -> usize {
	part2_impl(input_map())
}


mod parsing {
	use super::{Instr, Map, Node};

	impl TryFrom<u8> for Instr {
		type Error = ();
		fn try_from(value: u8) -> Result<Self, Self::Error> {
			match value { b'L' => Ok(Self::Left), b'R' => Ok(Self::Right), _ => Err(()) }
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum NodeError<'s> {
		Format,
		Label(&'s str),
		LeftEdge(&'s str),
		RightEdge(&'s str),
	}

	pub(super) struct LabeledNode<'s>(&'s str, Node<'s>);

	impl<'s> TryFrom<&'s str> for LabeledNode<'s> {
		type Error = NodeError<'s>;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			use NodeError as E;
			let (label, edges) = s.split_once(" = ").ok_or(E::Format)?;
			if label.contains(|c: char| !c.is_ascii_uppercase() && !c.is_ascii_digit())
				{ return Err(E::Label(label)) }
			let edges = edges.strip_prefix('(')
				.and_then(|s| s.strip_suffix(')'))
				.and_then(|s| s.split_once(", "))
				.ok_or(E::Format)?;
			Ok(LabeledNode(label, Node(edges.into())))
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum MapError<'s> {
		Format,
		Instr { offset: usize, source: u8 },
		Node { offset: usize, source: NodeError<'s> },
	}

	impl<'s> TryFrom<&'s str> for Map<'s> {
		type Error = MapError<'s>;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			use MapError as E;

			let (instrs, nodes) = s.split_once("\n\n").ok_or(E::Format)?;
			let instrs = instrs.bytes()
				.enumerate()
				.map(|(c, byte)| Instr::try_from(byte)
					.map_err(|()| E::Instr { offset: c, source: byte }))
				.collect::<Result<_, _>>()?;
			let nodes = nodes.lines()
				.enumerate()
				.map(|(l, line)| LabeledNode::try_from(line)
					.map_err(|e| E::Node { offset: l, source: e })
					.map(|LabeledNode(label, node)| (label, node)))
				.collect::<Result<_, _>>()?;
			Ok(Map { instrs, nodes })
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn part1() {
		const TRIVIAL_INPUT: &str = indoc::indoc! {"
			RL

			AAA = (BBB, CCC)
			BBB = (DDD, EEE)
			CCC = (ZZZ, GGG)
			DDD = (DDD, DDD)
			EEE = (EEE, EEE)
			GGG = (GGG, GGG)
			ZZZ = (ZZZ, ZZZ)
		"};
		const REPEAT_INPUT: &str = indoc::indoc! {"
			LLR

			AAA = (BBB, BBB)
			BBB = (AAA, ZZZ)
			ZZZ = (ZZZ, ZZZ)
		"};
		assert_eq!(part1_impl(TRIVIAL_INPUT.try_into().unwrap()), 2);
		assert_eq!(part1_impl(REPEAT_INPUT.try_into().unwrap()), 6);
		assert_eq!(super::part1(), 17873);
	}

	#[test]
	fn part2() {
		const INPUT: &str = indoc::indoc! {"
			LR

			11A = (11B, XXX)
			11B = (XXX, 11Z)
			11Z = (11B, XXX)
			22A = (22B, XXX)
			22B = (22C, 22C)
			22C = (22Z, 22Z)
			22Z = (22B, 22B)
			XXX = (XXX, XXX)
		"};
		assert_eq!(part2_brute(INPUT.try_into().unwrap()), 6);
		assert_eq!(part2_impl(INPUT.try_into().unwrap()), 6);
		assert_eq!(super::part2(), 15746133679061);
	}
}
