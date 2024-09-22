#![feature(impl_trait_in_assoc_type)]
#![feature(portable_simd)]

#[macro_use]
extern crate static_assertions;

mod grid;
mod rule;
mod simd_grid;

const GRID: &str =
    "000704005020010070000080002090006250600070008053200010400090000030060090200407000";

fn main() {
    let grid: grid::Grid<9> = GRID.parse().unwrap();
    let solved_grid = grid
        .try_solve(rule::standard::Standard9x9Rules::default(), 10)
        .unwrap();
    println!("{solved_grid}");

    let simd_grid: simd_grid::SimdGrid = GRID.parse().unwrap();
    let solved_simd_grid = simd_grid.try_solve(10).unwrap();
    println!("{solved_simd_grid}");
}
