use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sudoku_solver::grid::region::*;
use sudoku_solver::grid::Grid;
use sudoku_solver::rule::standard::*;
use sudoku_solver::simd_grid::SimdGrid;

const GRID_SIZE: usize = 9;
const GRID: &str =
    "000704005020010070000080002090006250600070008053200010400090000030060090200407000";

type BoxGroup = Box3x3Group;

type Singles = (
    SinglesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    SinglesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    SinglesInRegion<BoxGroup, GRID_SIZE>,
);

type HiddenSingles = (
    HiddenSinglesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenSinglesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenSinglesInRegion<BoxGroup, GRID_SIZE>,
);

fn criterion_benchmark(c: &mut Criterion) {
    let grid: Grid<GRID_SIZE> = GRID.parse().unwrap();
    let simd_grid: SimdGrid = GRID.parse().unwrap();

    let mut group = c.benchmark_group("scalar vs simd");
    group.bench_function("scalar", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles, HiddenSingles)>::default(), 10)))
    });
    group.bench_function("simd", |b| b.iter(|| black_box(simd_grid.try_solve(10))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
