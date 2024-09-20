use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sudoku_solver::grid::region::*;
use sudoku_solver::grid::Grid;
use sudoku_solver::rule::standard::*;

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

type Tuples = (
    TuplesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    TuplesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    TuplesInRegion<BoxGroup, GRID_SIZE>,
);

type HiddenTuples = (
    HiddenTuplesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenTuplesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenTuplesInRegion<BoxGroup, GRID_SIZE>,
);

fn criterion_benchmark(c: &mut Criterion) {
    let grid: Grid<GRID_SIZE> = GRID.parse().unwrap();

    let mut group = c.benchmark_group("increasing rule sets");
    group.bench_function("singles", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles,)>::default(), 10)))
    });
    group.bench_function("singles + hidden singles", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles, HiddenSingles)>::default(), 10)))
    });
    group.bench_function("singles + hidden singles + tuples", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles, HiddenSingles, Tuples)>::default(), 10)))
    });
    group.bench_function("singles + hidden singles + tuples + hidden tuples", |b| {
        b.iter(|| {
            black_box(grid.try_solve(
                <(Singles, HiddenSingles, Tuples, HiddenTuples)>::default(),
                10,
            ))
        })
    });
    group.bench_function(
        "singles + hidden singles + tuples + hidden tuples + x-wings",
        |b| {
            b.iter(|| {
                black_box(grid.try_solve(
                    <(
                        Singles,
                        HiddenSingles,
                        Tuples,
                        HiddenTuples,
                        HyperXWings<GRID_SIZE>,
                    )>::default(),
                    10,
                ))
            })
        },
    );
    group.bench_function(
        "singles + hidden singles + tuples + hidden tuples + x-wings + y-wings",
        |b| {
            b.iter(|| {
                black_box(grid.try_solve(
                    <(
                        Singles,
                        HiddenSingles,
                        Tuples,
                        HiddenTuples,
                        HyperXWings<GRID_SIZE>,
                        YWings<
                            (ColumnGroup<GRID_SIZE>, RowGroup<GRID_SIZE>, Box3x3Group),
                            GRID_SIZE,
                        >,
                    )>::default(),
                    10,
                ))
            })
        },
    );
    drop(group);

    let mut group = c.benchmark_group("hidden singles vs tuples");
    group.bench_function("singles", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles,)>::default(), 10)))
    });
    group.bench_function("singles + hidden singles", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles, HiddenSingles)>::default(), 10)))
    });
    group.bench_function("singles + tuples", |b| {
        b.iter(|| black_box(grid.try_solve(<(Singles, Tuples)>::default(), 10)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
