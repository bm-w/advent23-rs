// Copyright (c) 2024 Bastiaan Marinus van de Weerd


#[derive(Debug)]
struct Hailstone {
	pos: [isize; 3],
	velo: [isize; 3],
}


fn input_hailstones() -> Box<[Hailstone]> {
	parsing::try_hailstones_from_str(include_str!("day24.txt")).map(|r| r.unwrap()).collect()
}


/// Let _p_ denote a hailstones’s initial position and _v_ its velocity. The
/// path of a hailstone `i` is given by _pi + ti * vi_. The paths of two
/// hailstones _i_ and _j_ cross if there exist two times _ti_ and _tj_ such
/// that _pi + ti * vi = pj + tj * vj_. Since we’re only interested in the
/// _x & y_ coordinates, we have two unknowns (the times) and two equations
/// (the _x & y_ components of the above equation).
fn part1_impl<const TEST_MIN: isize, const TEST_MAX: isize>(input_hailstones: &[Hailstone])
-> usize {
	use itertools::Itertools;

	input_hailstones.iter().tuple_combinations().filter(|(hailstone_i, hailstone_j)| {
		// The system of equations can be expressed as a 2x2-matrix equation:
		//
		//     | ti | =  | vi_x  -vj_x |^-1   | -pi_x + pj_x |
		//     | tj | =  | vi_y  -vj_y |    * | -pi_y + pj_y |
		let [pi, pj] = [&hailstone_i.pos[..2], &hailstone_j.pos[..2]];
		let [vi, vj] = [&hailstone_i.velo[..2], &hailstone_j.velo[..2]];

		// There will only be a solution if the determinant is non-zero
		let det = -vi[0] * vj[1] + vi[1] * vj[0];
		if det == 0 { return false }

		let [dpx, dpy] = [-pi[0] + pj[0], -pi[1] + pj[1]];

		// The intersection must not be in the “past” (there is a solution, but
		// it’s invalid if either time is negative)
		let ti = (-vj[1] * dpx + vj[0] * dpy) as f64 / det as f64;
		if ti < 0.0 { return false }
		let tj = (-vi[1] * dpx + vi[0] * dpy) as f64 / det as f64;
		if tj < 0.0 { return false }

		// The intersection must be in the test area
		let x = pi[0] as f64 + ti * vi[0] as f64;
		if x < TEST_MIN as f64 || x > TEST_MAX as f64 { return false }
		let y = pi[1] as f64 + ti * vi[1] as f64;
		if y < TEST_MIN as f64 || y > TEST_MAX as f64 { return false }

		true
	}).count()
}

pub(crate) fn part1() -> usize {
	part1_impl::<200_000_000_000_000, 400_000_000_000_000>(&input_hailstones())
}


/// Now let _p_ denote initial position and _v_ the velocity of the thrown
/// rock, and _pi_ / _vi_ those of hailstone _i_. The rock will collide with
/// each hailstone _i_ at _ti_:
///
///     p + ti * v = pi + ti * vi
///     p - pi + ti * (v - vi) = 0
///
/// We can eliminate the _ti_ term by taking the wedge product with _v - vi_,
/// given that taking a vector’s wedge product with itself results in 0:
/// 
///     p ∧ (v - vi) - pi ∧ (v - vi) + ti * (v - vi) ∧ (v - vi) = 0
///     p ∧ (v - vi) - pi ∧ (v - vi) = 0
///     p ∧ v - p ∧ vi - pi ∧ v + pi ∧ vi = 0
///
/// The _p ∧ v_ term is the same for all hailstones _i, j, …etc._, so we can
/// eliminate it by subtracting the equation of _i_ from that of _j_:
///
///     -p∧v + p∧vi + pi∧v - pi∧vi + p∧v - p∧vj - pj∧v + pj∧vj = 0
///     p∧vi + pi∧v - pi∧vi - p∧vj - pj∧v + pj∧vj = 0
///     p ∧ (vi - vj) + (pi - pj) ∧ v - pi∧vi + pj∧vj = 0
///
/// Then lastly we can eliminate the _v_ term by taking the wedge product with
/// _pi - pj_:
///
///     p ∧ (vi - vj) ∧ (pi - pj) + (pi - pj) ∧ v ∧ (pi - pj)
///         - pi∧vi ∧ (pi - pj) + pj∧vj ∧ (pi - pj) = 0
///     p ∧ (vi - vj) ∧ (pi - pj) - pi∧vi ∧ (pi - pj) + pj∧vj ∧ (pi - pj) = 0
///     p ∧ (vi - vj) ∧ (pi - pj) - pi ∧ pj ∧ (vi - vj) = 0
///
/// This equation has three unknowns: the _x, y, & z_ coordinates of _p_, which
/// are the same for every pair of hailstones _i & j_. Thus, we can take three
/// arbitrary hailstones 0, 1, & 2, form pairs 0-1, 1-2, & 2-0, and obtain a
/// solvable system of three linear equations with three unknowns.
fn part2_impl(input_hailstones: &[Hailstone]) -> isize {

	// The real input numbers get quite big after a few multiplications, and
	// using floating point numbers leads to unpredictable rounding errors.
	use num_bigint::BigInt;

	// Any three arbitrary hailstones will do, so just take the first three.
	let [p0, p1, p2] = std::array::from_fn(|i| input_hailstones[i].pos.map(BigInt::from));
	let [v0, v1, v2] = std::array::from_fn(|i| input_hailstones[i].velo.map(BigInt::from));

	fn sub(lhs: &[BigInt; 3], rhs: &[BigInt; 3]) -> [BigInt; 3] {
		std::array::from_fn(|i| &lhs[i] - &rhs[i])
	}

	fn dot(lhs: &[BigInt; 3], rhs: &[BigInt; 3]) -> BigInt {
		&lhs[0] * &rhs[0] + &lhs[1] * &rhs[1] + &lhs[2] * &rhs[2]
	}

	fn cross(lhs: &[BigInt; 3], rhs: &[BigInt; 3]) -> [BigInt; 3] {
		std::array::from_fn(|i| &lhs[(i + 1) % 3] * &rhs[(i + 2) % 3]
			- &lhs[(i + 2) % 3] * &rhs[(i + 1) % 3])
	}

	fn triple(lhs: &[BigInt; 3], mhs: &[BigInt; 3], rhs: &[BigInt; 3]) -> BigInt {
		&lhs[0] * &mhs[1] * &rhs[2] + &lhs[1] * &mhs[2] * &rhs[0] + &lhs[2] * &mhs[0] * &rhs[1]
			- &lhs[2] * &mhs[1] * &rhs[0] - &lhs[1] * &mhs[0] * &rhs[2] - &lhs[0] * &mhs[2] * &rhs[1]
	}

	fn det2(m: &[&[BigInt; 2]; 2]) -> BigInt {
		&m[0][0] * &m[1][1] - &m[0][1] * &m[1][0]
	}

	// Form pairs from the three hailstones.
	let [dp01, dp12, dp20] = [sub(&p0, &p1), sub(&p1, &p2), sub(&p2, &p0)];
	let [dv01, dv12, dv20] = [sub(&v0, &v1), sub(&v1, &v2), sub(&v2, &v0)];

	// The system of equations can be expressed as a 3x3-matrix equation,
	// _Ax - b = 0_. The first term, the multiplication of the matrix _A_ by
	// the vector _x_, can be formed from the first terms of the equation
	// _p ∧ (vi - vj) ∧ (pi - pj)_, while the _b_ vector can be formed from
	// the second terms _pi ∧ pj ∧ (vi - vj)_. (NB. Both terms are “triple”
	// wedge products, and so would have a result of the form _k * ex∧ey∧ez_,
	// where _k_ is the vectors’ triple product and the _ex∧ey∧ez_ can be
	// simplified away.)

	// Build the matrix _A_ using the pairwise cross products
	// _(vi - vj) ⨯ (pi - pj)_ as the rows.
	let a = [cross(&dv01, &dp01), cross(&dv12, &dp12), cross(&dv20, &dp20)];

	// Invert the matrix using its cofactor matrix and the determinant.
	let co_a = [
		[det2(&[&[a[1][1].clone(), a[1][2].clone()], &[a[2][1].clone(), a[2][2].clone()]]),
			det2(&[&[a[1][2].clone(), a[1][0].clone()], &[a[2][2].clone(), a[2][0].clone()]]),
				det2(&[&[a[1][0].clone(), a[1][1].clone()], &[a[2][0].clone(), a[2][1].clone()]])],
		[det2(&[&[a[2][1].clone(), a[2][2].clone()], &[a[0][1].clone(), a[0][2].clone()]]),
			det2(&[&[a[2][2].clone(), a[2][0].clone()], &[a[0][2].clone(), a[0][0].clone()]]),
				det2(&[&[a[2][0].clone(), a[2][1].clone()], &[a[0][0].clone(), a[0][1].clone()]])],
		[det2(&[&[a[0][1].clone(), a[0][2].clone()], &[a[1][1].clone(), a[1][2].clone()]]),
			det2(&[&[a[0][2].clone(), a[0][0].clone()], &[a[1][2].clone(), a[1][0].clone()]]),
				det2(&[&[a[0][0].clone(), a[0][1].clone()], &[a[1][0].clone(), a[1][1].clone()]])],
	];
	let det = dot(&a[0], &co_a[0]);

	// Form the RHS vector using the pairwise triple products as the elements.
	let b = [triple(&p0, &p1, &dv01), triple(&p1, &p2, &dv12), triple(&p2, &p0, &dv20)];

	// Compute the stone’s initial position.
	let p = [
		dot(&[co_a[0][0].clone(), co_a[1][0].clone(), co_a[2][0].clone()], &b) / &det,
		dot(&[co_a[0][1].clone(), co_a[1][1].clone(), co_a[2][1].clone()], &b) / &det,
		dot(&[co_a[0][2].clone(), co_a[1][2].clone(), co_a[2][2].clone()], &b) / &det,
	];

	(&p[0] + &p[1] + &p[2]).try_into().unwrap()
}

pub(crate) fn part2() -> isize {
	part2_impl(&input_hailstones())
}


mod parsing {
	use std::{num::ParseIntError, str::FromStr};
	use super::Hailstone;

	#[derive(Debug)]
	pub(super) enum Coord { X, Y, Z }

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) enum HailstoneError {
		Format,
		Pos { coord: Coord, source: ParseIntError },
		Velo { coord: Coord, source: ParseIntError },
	}

	impl FromStr for Hailstone {
		type Err = HailstoneError;
		fn from_str(s: &str) -> Result<Self, Self::Err> {
			use HailstoneError as E;

			fn parse_coords(s: &str, to_coord_err: impl Fn(Coord, ParseIntError) -> HailstoneError)
			-> Result<[isize; 3], HailstoneError> {
				use Coord::*;
				let (x, s) = s.split_once(',').ok_or(E::Format)?;
				let (y, z) = s.split_once(',').ok_or(E::Format)?;
				Ok([
					x.trim().parse().map_err(|e| to_coord_err(X, e))?,
					y.trim().parse().map_err(|e| to_coord_err(Y, e))?,
					z.trim().parse().map_err(|e| to_coord_err(Z, e))?,
				])
			}

			let (pos, velo) = s.split_once(" @ ").ok_or(E::Format)?;
			let pos = parse_coords(pos, |coord, source| E::Pos { coord, source })?;
			let velo = parse_coords(velo, |coord, source| E::Velo { coord, source })?;

			Ok(Hailstone { pos, velo })
		}
	}

	#[allow(dead_code)]
	#[derive(Debug)]
	pub(super) struct  HailstonesError { line: usize, source: HailstoneError }

	pub(super) fn try_hailstones_from_str(s: &str)
	-> impl Iterator<Item = Result<Hailstone, HailstonesError>> + '_ {
		s.lines()
			.enumerate()
			.map(|(l, line)| line.parse()
				.map_err(|e| HailstonesError { line: l + 1, source: e }))
	}
}


#[test]
fn tests() {
	const INPUT: &str = indoc::indoc! {"
		19, 13, 30 @ -2,  1, -2
		18, 19, 22 @ -1, -1, -2
		20, 25, 34 @ -2, -2, -4
		12, 31, 28 @ -1, -2, -1
		20, 19, 15 @  1, -5, -3
	"};
	assert_eq!(part1_impl::<7, 27>(&parsing::try_hailstones_from_str(INPUT).map(|r| r.unwrap())
		.collect::<Box<[_]>>()), 2);
	assert_eq!(part1(), 12015);
	assert_eq!(part2_impl(&parsing::try_hailstones_from_str(INPUT).map(|r| r.unwrap())
	.collect::<Box<[_]>>()), 47);
	assert_eq!(part2(), 1016365642179116);
}
