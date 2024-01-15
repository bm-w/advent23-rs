// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Clone, PartialEq, Eq)]
enum ModKind<'n> {
	Broadcaster,
	FlipFlop(bool),
	Conjunction(std::collections::HashMap<&'n str, bool>),
	Untyped,
}

#[derive(Clone)]
struct Mod<'n> {
	name: &'n str,
	kind: ModKind<'n>,
	dest_names: Vec<&'n str>,
}

trait ModConfig<'n>: AsRef<[Mod<'n>]> + AsMut<[Mod<'n>]> {

	fn broadcaster_mod_position(&self) -> usize {
		self.as_ref().iter().position(|m| matches!(m.kind, ModKind::Broadcaster)).unwrap()
	}

	fn press_button(
		&mut self,
		broadcaster_offset: Option<usize>,
		mut analyze: impl FnMut(&'n str, &Mod<'n>, bool)
	) {
		use std::collections::VecDeque;

		let broadcaster_offset = if let Some(b) = broadcaster_offset {
			debug_assert!(matches!(&self.as_ref()[b].kind, ModKind::Broadcaster));
			b
		} else {
			self.broadcaster_mod_position()
		};

		analyze("button", &self.as_ref()[broadcaster_offset], false);

		let mods = self.as_mut();
		let broadcaster = &mods[broadcaster_offset];
		let mut queue = VecDeque::from_iter(broadcaster.dest_names.iter()
			.copied()
			.map(|n| (broadcaster.name, n, false)));
		while let Some((src_name, recv_name, high)) = queue.pop_front() {
			let recv_mod = mods.iter_mut().find(|m| m.name == recv_name).unwrap();
			analyze(src_name, recv_mod, high);
			let high = match &mut recv_mod.kind {
				ModKind::FlipFlop(is_on) if !high => {
					*is_on = !*is_on;
					*is_on
				}
				ModKind::Conjunction(mem) => {
					mem.insert(src_name, high);
					mem.values().any(|v| !*v)
				}
				ModKind::FlipFlop(_) | ModKind::Untyped => continue,
				_ => unreachable!(),
			};

			queue.extend(recv_mod.dest_names.iter().copied().map(|n| (recv_mod.name, n, high)));
		}
	}
}

impl<'n> ModConfig<'n> for Vec<Mod<'n>> {}


fn input_mods() -> Vec<Mod<'static>> {
	parsing::try_mods_from_str(include_str!("day20.txt")).unwrap()
}


fn part1_impl(input_mods: Vec<Mod<'_>>) -> u64 {
	let mut mods = input_mods.clone();
	let b = mods.iter().position(|m| matches!(m.kind, ModKind::Broadcaster)).unwrap();

	let [mut lows, mut highs] = [0, 0];
	let mut i = 0;
	while i < 1000 {
		mods.press_button(Some(b), |_, _, high| if high { highs += 1 } else { lows += 1 });
		i += 1;

		if std::iter::zip(&mods, &input_mods).all(|(m, im)| m.kind == im.kind) {
			let n = 1000 / i;
			lows *= n;
			highs *= n;
			i *= n;
		}
	}

	lows as u64 * highs as u64
}

pub(crate) fn part1() -> u64 {
	part1_impl(input_mods())
}


fn part2_impl(mut input_mods: Vec<Mod<'_>>) -> usize {
	use itertools::Itertools as _;

	let broadcaster_offset = input_mods.broadcaster_mod_position();
	let rx_conjunction_offset = input_mods.iter()
		.enumerate()
		.filter(|(_, m)| m.dest_names == ["rx"] && matches!(m.kind, ModKind::Conjunction(_)))
		.map(|(i, _)| i)
		.exactly_one().map_err(|_| ()).unwrap();
	let rx_conjunction_input_name_ptrs = input_mods.iter()
		.filter(|m| m.dest_names.contains(&input_mods[rx_conjunction_offset].name))
		.map(|m| m.name as *const str)
		.collect::<Vec<_>>();

	let mut found = vec![usize::MAX; rx_conjunction_input_name_ptrs.len()];
	for i in 0.. {
		let rx_conjunction_ptr = &input_mods[rx_conjunction_offset] as *const Mod<'_>;
		input_mods.press_button(Some(broadcaster_offset), |src_name, recv_mod, high| {
			if !high || !std::ptr::eq(recv_mod, rx_conjunction_ptr) { return }
			let Some(f) = rx_conjunction_input_name_ptrs.iter()
				.position(|p| std::ptr::eq(src_name, *p)) else { return };
			found[f] = i + 1;
		});

		if !found.contains(&usize::MAX) {
			break
		}
	}

	found.into_iter().fold(1, num_integer::lcm)
}

pub(crate) fn part2() -> usize {
	part2_impl(input_mods())
}


mod parsing {
	use super::{ModKind, Mod};

	#[derive(Debug)]
	pub(super) enum ModError {
		Format,
		Kind,
	}

	impl<'s> TryFrom<&'s str> for Mod<'s> {
		type Error = ModError;
		fn try_from(s: &'s str) -> Result<Self, Self::Error> {
			use {ModError as E, ModKind as K};

			let (s, dest_names) = s.split_once(" -> ").ok_or(E::Format)?;
			let dest_names = dest_names.split(", ").collect();

			if s == "broadcaster" {
				Ok(Mod { name: "broadcaster", kind: K::Broadcaster, dest_names })
			} else if let Some(name) = s.strip_prefix('%') {
				Ok(Mod { name, kind: K::FlipFlop(false), dest_names })
			} else if let Some(name) = s.strip_prefix('&') {
				Ok(Mod { name, kind: K::Conjunction(Default::default()), dest_names })
			} else {
				Err(E::Kind)
			}
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum ModsError {
		Mod { line: usize, source: ModError },
		MissingBroadcaster,
	}

	pub(super) fn try_mods_from_str(s: &str) -> Result<Vec<Mod<'_>>, ModsError> {
		let mut mods = s.lines()
			.enumerate()
			.map(|(l, line)| Mod::try_from(line)
				.map_err(|e| ModsError::Mod { line: l + 1, source: e }))
			.collect::<Result<Vec<_>, _>>()?;

		if !mods.iter().any(|m| matches!(m.kind, ModKind::Broadcaster)) {
			return Err(ModsError::MissingBroadcaster)
		}

		let mods_range = 0..mods.len();
		while let Some(dest_name) = mods[mods_range.clone()].iter()
			.flat_map(|m| m.dest_names.iter().copied())
			.find(|&n| !mods.iter().any(|m| m.name == n)) {
			mods.push(Mod { name: dest_name, kind: ModKind::Untyped, dest_names: vec![] })
		}

		let mut i = 0;
		while i < mods.len() {
			let r#mod = &mods[i];
			if matches!(r#mod.kind, ModKind::Conjunction(_)) {
				let src_names = mods.iter()
					.filter_map(|m| m.dest_names.contains(&r#mod.name).then_some((m.name, false)))
					.collect();
				let ModKind::Conjunction(src_states) = &mut mods[i].kind else { unreachable!() };
				*src_states = src_names;
			}
			i += 1;
		}

		Ok(mods)
	}
}


#[test]
fn tests() {
	const INPUTS: [&str; 2] = [
		indoc::indoc! {"
			broadcaster -> a, b, c
			%a -> b
			%b -> c
			%c -> inv
			&inv -> a
		"},
		indoc::indoc! {"
			broadcaster -> a
			%a -> inv, con
			&inv -> b
			%b -> con
			&con -> output
		"},
	];
	assert_eq!(part1_impl(parsing::try_mods_from_str(INPUTS[0]).unwrap()), 32_000_000);
	assert_eq!(part1_impl(parsing::try_mods_from_str(INPUTS[1]).unwrap()), 11_687_500);
	assert_eq!(part1(), 832957356);
	assert_eq!(part2_impl(input_mods()), 240162699605221);
}
