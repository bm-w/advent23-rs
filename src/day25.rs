// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, Debug)]
struct Wires<'s>(Box<[&'s str]>);

type Diagram<'s> = std::collections::HashMap<&'s str, Wires<'s>>;


fn part1_impl(input_diagram: Diagram) -> u64 {
	use std::{collections::{HashMap, HashSet, VecDeque}, rc::Rc};
	use itertools::Itertools;

	let components = input_diagram.iter()
		.flat_map(|(&comp, wires)| std::iter::once(comp)
			.chain(wires.0.iter().copied()))
		.collect::<HashSet<_>>();

	// Find the “inverted” diagram, to be able to reach components in the
	// opposite direction from the one listed.

	let mut inv_diagram = HashMap::new();
	for (&comp0, wires) in &input_diagram {
		for &comp1 in wires.0.as_ref() {
			if input_diagram.get(comp1).map(|w| w.0.contains(&comp0)).unwrap_or(false) { continue }
			inv_diagram.entry(comp1)
				.or_insert(Vec::new())
				.push(comp0);
		}
	}

	// The union of the components across the listed wires & across the
	// inverted wires are reachable from each component.

	let wires = |comp: &str| input_diagram.get(comp).into_iter()
		.flat_map(|w| w.0.iter().copied())
		.chain(inv_diagram.get(comp).into_iter().flatten().copied());

	// Find the component furthest away from some arbitrary start component:
	// this will be the “start” component. Then find the component furthest
	// away from that: this will be the “end” component. Given how the input
	// diagram is generated these components will be on opposite sides of
	// the diagram, each within its own group, on either side of the three
	// wires that are to be disconnected.

	fn furthest<'s, F, I>(wires: F, start: &'s str) -> &'s str
	where F: Fn(&str) -> I, I: Iterator<Item = &'s str> {
		let mut queue = VecDeque::from_iter([(start, 0)]);
		let mut visited = HashSet::new();
		let mut result = (start, 0);
		while let Some((from_comp, steps)) = queue.pop_front() {
			if !visited.insert(from_comp) { continue }
			if steps > result.1 { result = (from_comp, steps) }
			for to_comp in wires(from_comp) {
				if visited.contains(to_comp) { continue }
				queue.push_back((to_comp, steps + 1));
			}
		}
		result.0
	}

	let start = furthest(wires, components.iter().next().copied().unwrap());
	let end = furthest(wires, start);

	// Find a path from `start` to `end` three times, each time adding the
	// path’s wires to a set of disconnected wires that can’t be used again the
	// next time. Each of these paths will contain one of the wires that are
	// to be disconnected (and various other wires, but that’s OK).
	// Then, “try” to find the path one more time (knowing that it won’t be
	// found because the groups are now disconnected) and count how many
	// components _can_ still be reached: this will be the group size.
	// _NB. This assumes the groups are sufficiently well-connected that each
	// component within a group can still be reached even after the wires in
	// the paths across the min.-cut wires are disconnected._

	#[cfg_attr(test, derive(Debug))]
	struct Node<'s> {
		comp: &'s str,
		prev: Option<Rc<Node<'s>>>,
	}

	impl<'s> Node<'s> {
		fn path(&self) -> impl Iterator<Item = &'s str> + '_ {
			let mut comp = Some(self.comp);
			let mut next = self.prev.as_deref();
			std::iter::from_fn(move || {
				let cur_comp = comp.take()?;
				if let Some(cur_next) = next.take() {
					comp = Some(cur_next.comp);
					next = cur_next.prev.as_deref();
				}
				Some(cur_comp)
			})
		}
	}

	macro_rules! ordered_comps { ($comp0:expr, $comp1:expr) => { {
		let [comp0, comp1] = [$comp0, $comp1];
		if comp0 < comp1 { [comp0, comp1] } else { [comp1, comp0] }
	} }; }

	let mut disconnected_wires = HashSet::new();
	let mut start_group_size = 0;
	for _ in 0..4 {
		let mut queue = VecDeque::from_iter([Node { comp: start, prev: None }]);
		let mut visited = HashSet::new();
		while let Some(node) = queue.pop_front() {
			if !visited.insert(node.comp) { continue }
			if node.comp == end {
				for (from_comp, to_comp) in node.path().tuple_windows() {
					disconnected_wires.insert(ordered_comps!(from_comp, to_comp));
				}
				break;
			}
			let node = Rc::new(node);
			for to_comp in wires(node.comp) {
				let edge = ordered_comps!(node.comp, to_comp);
				if disconnected_wires.contains(&edge) || visited.contains(to_comp) { continue }
				queue.push_back(Node { comp: to_comp, prev: Some(node.clone()) })
			}
		}
		start_group_size = visited.len()
	}
	
	start_group_size as u64 * (components.len() - start_group_size) as u64
}

pub(crate) fn part1() -> u64 {
	part1_impl(parsing::try_diagram_from_str(include_str!("day25.txt")).unwrap())
}


pub(crate) fn part2() -> &'static str {
	"Merry Christmas!"
}


mod parsing {
	use super::{Diagram, Wires};

	impl<'w> From<&'w str> for Wires<'w> {
		fn from(s: &'w str) -> Self {
			Wires(s.split_whitespace().collect())
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct DiagramFormatError { line: usize }

	pub(super) fn try_diagram_from_str(s: &str)
	-> Result<Diagram, DiagramFormatError> {
		s.lines()
			.enumerate()
			.map(|(l, line)| {
				let (component, wires) = line.split_once(": ")
					.ok_or(DiagramFormatError { line: l + 1 })?;
				Ok((component, Wires::from(wires)))
			})
			.collect()
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		jqt: rhn xhk nvd
		rsh: frs pzl lsr
		xhk: hfx
		cmg: qnr nvd lhk bvb
		rhn: xhk bvb hfx
		bvb: xhk hfx
		pzl: lsr hfx nvd
		qnr: nvd
		ntq: jqt hfx bvb xhk
		nvd: lhk
		lsr: lhk
		rzs: qnr cmg lsr rsh
		frs: qnr lhk lsr
	"};
	assert_eq!(part1_impl(parsing::try_diagram_from_str(INPUT).unwrap()), 54);
	assert_eq!(part1(), 547410);
}
