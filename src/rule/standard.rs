use super::*;
use crate::grid::region::*;
use smallvec::{smallvec, SmallVec};

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct SinglesInRegion<Group, const GRID_SIZE: usize>(pub Group)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>;

impl<Group, const GRID_SIZE: usize> RuleSet<GRID_SIZE> for SinglesInRegion<Group, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        let mut result = grid.clone();

        'scan: {
            for region in self.0.regions() {
                for (src_index, src_cell) in region.cells_in(grid).enumerate() {
                    if let Some(digit) = src_cell.solved_digit() {
                        for (dst_index, dst_cell) in region.cells_mut_in(&mut result).enumerate() {
                            if dst_index != src_index {
                                dst_cell.remove_option(digit);

                                if !dst_cell.is_solvable() {
                                    break 'scan;
                                }
                            }
                        }
                    }
                }
            }

            return Ok(result);
        }

        Err(result)
    }
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct HiddenSinglesInRegion<Group, const GRID_SIZE: usize>(pub Group)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>;

impl<Group, const GRID_SIZE: usize> RuleSet<GRID_SIZE> for HiddenSinglesInRegion<Group, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        let mut result = grid.clone();

        for region in self.0.regions() {
            for digit in Digit::<GRID_SIZE>::ALL {
                let mut options = 0u16;
                for (i, cell) in region.cells_in(grid).enumerate() {
                    if cell.has_option(digit) {
                        options |= 1 << i;
                    }
                }

                match options.count_ones() {
                    0 => return Err(result),
                    1 => {
                        let i = options.trailing_zeros() as usize;
                        *region.cells_mut_in(&mut result).nth(i).unwrap() = Cell::solved(digit);
                    }
                    _ => (),
                }
            }
        }

        Ok(result)
    }
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct TuplesInRegion<Group, const GRID_SIZE: usize>(pub Group)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>;

impl<Group, const GRID_SIZE: usize> RuleSet<GRID_SIZE> for TuplesInRegion<Group, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        let mut result = grid.clone();

        for region in self.0.regions() {
            let mut candidates: SmallVec<[(Cell<GRID_SIZE>, u16); GRID_SIZE]> = smallvec![];
            for (i, &cell) in region.cells_in(grid).enumerate() {
                if let Some((_, indices)) =
                    candidates.iter_mut().find(|&&mut (other, _)| other == cell)
                {
                    *indices |= 1 << i;
                } else {
                    candidates.push((cell, 1 << i));
                }
            }

            for (candidates, mut indices) in candidates {
                if candidates.option_count() == (indices.count_ones() as u8) {
                    for cell in region.cells_mut_in(&mut result) {
                        if (indices & 0b1) == 0 {
                            cell.subtract(candidates);
                        }

                        indices >>= 1;
                    }
                }
            }
        }

        Ok(result)
    }
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct HiddenTuplesInRegion<Group, const GRID_SIZE: usize>(pub Group)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>;

impl<Group, const GRID_SIZE: usize> RuleSet<GRID_SIZE> for HiddenTuplesInRegion<Group, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    Group: RegionGroup<GRID_SIZE>,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        let mut result = grid.clone();

        'scan: {
            for region in self.0.regions() {
                let unsolved_cells = region
                    .cells_in(grid)
                    .filter(|cell| !cell.is_solved())
                    .count();

                if (unsolved_cells / 2) < 2 {
                    continue;
                }

                let mut options = DigitMap::<u16, GRID_SIZE>::default();

                for digit in Digit::<GRID_SIZE>::ALL {
                    for (i, cell) in region.cells_in(grid).enumerate() {
                        if cell.has_option(digit) {
                            options[digit] |= 1 << i;
                        }
                    }
                }

                for n in 2..=(unsolved_cells / 2) {
                    let mut candidates: SmallVec<[(u16, Cell<GRID_SIZE>); GRID_SIZE]> = smallvec![];

                    for (digit, &options) in &options {
                        if (options.count_ones() as usize) == n {
                            if let Some((_, digits)) =
                                candidates.iter_mut().find(|(o, _)| *o == options)
                            {
                                digits.insert_option(digit);
                            } else {
                                candidates.push((options, Cell::solved(digit)));
                            }
                        }
                    }

                    for (digit, &options) in &options {
                        if (2..n).contains(&(options.count_ones() as usize)) {
                            for (_, digits) in candidates
                                .iter_mut()
                                .filter(|(o, _)| (*o & options) == options)
                            {
                                digits.insert_option(digit);
                            }
                        }
                    }

                    candidates.retain(|(_, digits)| (digits.option_count() as usize) == n);

                    for (mut options, digits) in candidates {
                        for cell in region.cells_mut_in(&mut result) {
                            if (options & 0b1) > 0 {
                                cell.intersect(digits);
                            } else {
                                cell.subtract(digits);
                            }

                            if !cell.is_solvable() {
                                break 'scan;
                            }

                            options >>= 1;
                        }
                    }
                }
            }

            return Ok(result);
        }

        Err(result)
    }
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct HyperXWings<const GRID_SIZE: usize>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> RuleSet<GRID_SIZE> for HyperXWings<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        let mut result = grid.clone();

        for digit in Digit::<GRID_SIZE>::ALL {
            let mut candidates = [0u16; GRID_SIZE];
            for (column, candidates) in ColumnGroup::<GRID_SIZE>.regions().zip(&mut candidates) {
                for (i, (_, cell)) in column.cell_positions_in(grid).enumerate() {
                    if cell.has_option(digit) {
                        *candidates |= 1 << i;
                    }
                }

                if *candidates == 0 {
                    return Err(result);
                }
            }

            let mut equal_columns: SmallVec<[usize; GRID_SIZE]> = smallvec![];
            for current_column in 0..(GRID_SIZE - 1) {
                equal_columns.clear();
                equal_columns.push(current_column);

                for other_column in (current_column + 1)..GRID_SIZE {
                    if candidates[other_column] == candidates[current_column] {
                        equal_columns.push(other_column);
                    }
                }

                let candidate_count = candidates[current_column].count_ones() as usize;
                if equal_columns.len() > candidate_count {
                    return Err(result);
                } else if equal_columns.len() == candidate_count {
                    for (i, column) in ColumnGroup::<GRID_SIZE>.regions().enumerate() {
                        if equal_columns.contains(&i) {
                            continue;
                        }

                        let mut to_clear = candidates[current_column];
                        for (_, cell) in column.cell_positions_mut_in(&mut result) {
                            if (to_clear & 0b1) > 0 {
                                cell.remove_option(digit);
                            }

                            to_clear >>= 1;
                        }
                    }
                }
            }

            let mut candidates = [0u16; GRID_SIZE];
            for (row, candidates) in RowGroup::<GRID_SIZE>.regions().zip(&mut candidates) {
                for (i, (_, cell)) in row.cell_positions_in(grid).enumerate() {
                    if cell.has_option(digit) {
                        *candidates |= 1 << i;
                    }
                }

                if *candidates == 0 {
                    return Err(result);
                }
            }

            let mut equal_rows: SmallVec<[usize; GRID_SIZE]> = smallvec![];
            for current_row in 0..(GRID_SIZE - 1) {
                equal_rows.clear();
                equal_rows.push(current_row);

                for other_row in (current_row + 1)..GRID_SIZE {
                    if candidates[other_row] == candidates[current_row] {
                        equal_rows.push(other_row);
                    }
                }

                let candidate_count = candidates[current_row].count_ones() as usize;
                if equal_rows.len() > candidate_count {
                    return Err(result);
                } else if equal_rows.len() == candidate_count {
                    for (i, row) in RowGroup::<GRID_SIZE>.regions().enumerate() {
                        if equal_rows.contains(&i) {
                            continue;
                        }

                        let mut to_clear = candidates[current_row];
                        for (_, cell) in row.cell_positions_mut_in(&mut result) {
                            if (to_clear & 0b1) > 0 {
                                cell.remove_option(digit);
                            }

                            to_clear >>= 1;
                        }
                    }
                }
            }
        }

        Ok(result)
    }
}

#[derive(Debug, Default, Clone, Copy)]
#[repr(transparent)]
pub struct YWings<Groups, const GRID_SIZE: usize>(pub Groups)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

macro_rules! impl_y_wings {
    ($($t:ident),+ $(,)?) => {
        impl<$($t,)+ const GRID_SIZE: usize> RuleSet<GRID_SIZE> for YWings<($($t,)+), GRID_SIZE>
        where
            GridTemplate<GRID_SIZE>: ValidGridTemplate,
            ($($t,)+): std::fmt::Debug,
            $($t: RegionGroup<GRID_SIZE>,)+
        {
            fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
                #[allow(non_snake_case)]
                let ($($t,)+) = &self.0;

                let mut result = grid.clone();

                for index_a in 0..(GRID_SIZE*GRID_SIZE-1) {
                    let pos_a = GridPosition {
                        column: index_a % GRID_SIZE,
                        row: index_a / GRID_SIZE,
                    };
                    let cell_a = grid[pos_a];

                    if cell_a.option_count() != 2 {
                        continue;
                    }

                    let mut influence_grid_a = [[false; GRID_SIZE]; GRID_SIZE];
                    $({
                        for region in $t.regions() {
                            if region.contains(pos_a) {
                                for (pos, _) in region.cell_positions_in(grid) {
                                    if pos != pos_a {
                                        influence_grid_a[pos.row][pos.column] = true;
                                    }
                                }
                            }
                        }
                    })+

                    for index_b in (index_a+1)..(GRID_SIZE*GRID_SIZE) {
                        let pos_b = GridPosition {
                            column: index_b % GRID_SIZE,
                            row: index_b / GRID_SIZE,
                        };
                        let cell_b = grid[pos_b];

                        if cell_b.option_count() != 2 {
                            continue;
                        }

                        let mut shared_options = cell_a;
                        shared_options.intersect(cell_b);
                        if shared_options.option_count() != 1 {
                            continue;
                        }

                        let mut other_a = cell_a;
                        other_a.subtract(shared_options);
                        let mut other_b = cell_b;
                        other_b.subtract(shared_options);
                        let mut search_options = other_a;
                        search_options.union(other_b);

                        let mut influence_grid_b = [[false; GRID_SIZE]; GRID_SIZE];
                        $({
                            for region in $t.regions() {
                                if region.contains(pos_b) {
                                    for (pos, _) in region.cell_positions_in(grid) {
                                        if pos != pos_b {
                                            influence_grid_b[pos.row][pos.column] = true;
                                        }
                                    }
                                }
                            }
                        })+

                        'search: for row in 0..GRID_SIZE {
                            for column in 0..GRID_SIZE {
                                if !(influence_grid_a[row][column] & influence_grid_b[row][column]) {
                                    continue;
                                }

                                let pos = GridPosition { column, row };
                                if grid[pos] == search_options {
                                    for row in 0..GRID_SIZE {
                                        for column in 0..GRID_SIZE {
                                            if !(influence_grid_a[row][column] & influence_grid_b[row][column]) {
                                                continue;
                                            }

                                            let reduce_pos = GridPosition { column, row };
                                            if reduce_pos == pos {
                                                continue;
                                            }

                                            result[reduce_pos].subtract(shared_options);
                                            if !result[reduce_pos].is_solvable() {
                                                return Err(result);
                                            }
                                        }
                                    }

                                    break 'search;
                                }
                            }
                        }
                    }
                }

                Ok(result)
            }
        }
    };
}

impl_y_wings!(T1);
impl_y_wings!(T1, T2);
impl_y_wings!(T1, T2, T3);
impl_y_wings!(T1, T2, T3, T4);
impl_y_wings!(T1, T2, T3, T4, T5);
impl_y_wings!(T1, T2, T3, T4, T5, T6);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_y_wings!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);

#[allow(dead_code)]
pub type LineRules<const GRID_SIZE: usize> = (
    SinglesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    SinglesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenSinglesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenSinglesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    TuplesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    TuplesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenTuplesInRegion<ColumnGroup<GRID_SIZE>, GRID_SIZE>,
    HiddenTuplesInRegion<RowGroup<GRID_SIZE>, GRID_SIZE>,
);

macro_rules! box_rules {
    (($group:ty, $grid_size:literal) => $name:ident) => {
        #[allow(dead_code)]
        pub type $name = (
            SinglesInRegion<$group, $grid_size>,
            HiddenSinglesInRegion<$group, $grid_size>,
            TuplesInRegion<$group, $grid_size>,
            HiddenTuplesInRegion<$group, $grid_size>,
        );

        assert_impl_all!($name: RuleSet<$grid_size>);
    };
}

box_rules!((Box2x2Group, 4) => Box2x2Rules);
box_rules!((Box3x2Group, 6) => Box3x2Rules);
box_rules!((Box2x3Group, 6) => Box2x3Rules);
box_rules!((Box3x3Group, 9) => Box3x3Rules);
box_rules!((Box4x3Group, 12) => Box4x3Rules);
box_rules!((Box3x4Group, 12) => Box3x4Rules);
box_rules!((Box4x4Group, 16) => Box4x4Rules);

macro_rules! standard_rule {
    ($name:ident<$grid_size:literal>[$($rule:ty),* $(,)?][$($group:ty),* $(,)?]) => {
        #[allow(dead_code)]
        pub type $name = (
            LineRules<$grid_size>,
            HyperXWings<$grid_size>,
            YWings<
                (
                    ColumnGroup<$grid_size>,
                    RowGroup<$grid_size>,
                    $($group,)*
                ),
                $grid_size
            >,
            $($rule,)*
        );

        assert_impl_all!($name: RuleSet<$grid_size>);
    };
}

standard_rule!(Standard4x4Rules<4>[Box2x2Rules][Box2x2Group]);
standard_rule!(Standard6x6HRules<6>[Box3x2Rules][Box3x2Group]);
standard_rule!(Standard6x6VRules<6>[Box2x3Rules][Box2x3Group]);
standard_rule!(Standard9x9Rules<9>[Box3x3Rules][Box3x3Group]);
standard_rule!(Standard12x12HRules<12>[Box4x3Rules][Box4x3Group]);
standard_rule!(Standard12x12VRules<12>[Box3x4Rules][Box3x4Group]);
standard_rule!(Standard16x16Rules<16>[Box4x4Rules][Box4x4Group]);
