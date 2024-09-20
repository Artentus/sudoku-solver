#![feature(impl_trait_in_assoc_type)]

#[macro_use]
extern crate static_assertions;

mod grid;
mod rule;

const GRID: &str =
    "000704005020010070000080002090006250600070008053200010400090000030060090200407000";

fn main() {
    let grid: grid::Grid<9> = GRID.parse().unwrap();
    match grid.try_solve(rule::standard::Standard9x9Rules::default(), 10) {
        Ok(grid) | Err(grid) => println!("{grid}"),
    }
}
