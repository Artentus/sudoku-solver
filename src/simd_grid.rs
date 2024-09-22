use std::fmt;
use std::simd::prelude::*;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SimdGrid {
    cells: [u16x16; 9],
}

#[derive(Debug, Clone)]
pub enum SimdGridFromStrError {
    InvalidLength,
    InvalidChar,
}

impl FromStr for SimdGrid {
    type Err = SimdGridFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.as_bytes();
        if s.len() != 81 {
            return Err(SimdGridFromStrError::InvalidLength);
        }

        let mut grid = SimdGrid {
            cells: [u16x16::from_array([0; 16]); 9],
        };

        let mut s_index = 0;
        for row in grid.cells.iter_mut() {
            for (column, cell) in row[0..11].iter_mut().enumerate() {
                if (column % 4) == 3 {
                    continue;
                }

                let value = s[s_index]
                    .checked_sub(b'0')
                    .ok_or(SimdGridFromStrError::InvalidChar)?;

                if (0..=9).contains(&value) {
                    if value == 0 {
                        *cell = 0x1FF;
                    } else {
                        *cell = 1 << (value - 1);
                    }
                }

                s_index += 1;
            }
        }

        Ok(grid)
    }
}

impl SimdGrid {
    fn fmt_row(&self, row: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for column in 0..11 {
            if (column % 4) == 3 {
                continue;
            }

            if [0, 4, 8].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if cell.count_ones() <= 1 {
                write!(f, "      ")?;
            } else {
                for digit in [1 << 0, 1 << 1, 1 << 2] {
                    if (cell & digit) > 0 {
                        write!(f, "{digit} ")?;
                    } else {
                        write!(f, "· ")?;
                    }
                }
            }
        }
        writeln!(f, "┃")?;

        for column in 0..11 {
            if (column % 4) == 3 {
                continue;
            }

            if [0, 4, 8].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if cell.count_ones() == 1 {
                write!(f, "  {}   ", cell.trailing_zeros() + 1)?;
            } else if cell == 0 {
                write!(f, "  X   ")?;
            } else {
                for digit in [1 << 3, 1 << 4, 1 << 5] {
                    if (cell & digit) > 0 {
                        write!(f, "{digit} ")?;
                    } else {
                        write!(f, "· ")?;
                    }
                }
            }
        }
        writeln!(f, "┃")?;

        for column in 0..11 {
            if (column % 4) == 3 {
                continue;
            }

            if [0, 4, 8].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if cell.count_ones() <= 1 {
                write!(f, "      ")?;
            } else {
                for digit in [1 << 6, 1 << 7, 1 << 8] {
                    if (cell & digit) > 0 {
                        write!(f, "{digit} ")?;
                    } else {
                        write!(f, "· ")?;
                    }
                }
            }
        }
        writeln!(f, "┃")?;

        Ok(())
    }
}

impl fmt::Display for SimdGrid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "┏━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┓"
        )?;
        self.fmt_row(0, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(1, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(2, f)?;
        writeln!(
            f,
            "┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫"
        )?;
        self.fmt_row(3, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(4, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(5, f)?;
        writeln!(
            f,
            "┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫"
        )?;
        self.fmt_row(6, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(7, f)?;
        writeln!(
            f,
            "┠───────┼───────┼───────╂───────┼───────┼───────╂───────┼───────┼───────┨"
        )?;
        self.fmt_row(8, f)?;
        write!(
            f,
            "┗━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┛"
        )
    }
}

const ZERO: u16x16 = u16x16::from_array([0; 16]);
const ONE: u16x16 = u16x16::from_array([1; 16]);
const MASK: u64 = 0x777;

#[inline]
fn is_unsolvable(cells: u16x16) -> bool {
    (cells.simd_eq(ZERO).to_bitmask() & MASK) > 0
}

#[inline]
fn is_solved(cells: u16x16) -> mask16x16 {
    let lsb_removed = cells & (cells - ONE);
    cells.simd_ne(ZERO) & lsb_removed.simd_eq(ZERO)
}

#[rustfmt::skip]
#[allow(non_snake_case)]
#[inline]
fn transpose3x3(rows: &mut [u16x16]) {
    let a0_a1_a2_00__a4_a5_a6_00__a8_a9_a10_00__00_00_00_00 = rows[0];
    let b0_b1_b2_00__b4_b5_b6_00__b8_b9_b10_00__00_00_00_00 = rows[1];
    let c0_c1_c2_00__c4_c5_c6_00__c8_c9_c10_00__00_00_00_00 = rows[2];

    let a0_a1_a2_00__b0_b1_b2_00__a8_a9_a10_00__b8_b9_b10_00 = simd_swizzle!(
        a0_a1_a2_00__a4_a5_a6_00__a8_a9_a10_00__00_00_00_00,
        b0_b1_b2_00__b4_b5_b6_00__b8_b9_b10_00__00_00_00_00,
        [
             0,  1,  2,  3,
            16, 17, 18, 19,
             8,  9, 10, 11,
            24, 25, 26, 27,
        ]
    );

    let a4_a5_a6_00__b4_b5_b6_00__00_00_00_00__00_00_00_00 = simd_swizzle!(
        a0_a1_a2_00__a4_a5_a6_00__a8_a9_a10_00__00_00_00_00,
        b0_b1_b2_00__b4_b5_b6_00__b8_b9_b10_00__00_00_00_00,
        [
             4,  5,  6,  7,
            20, 21, 22, 23,
            12, 13, 14, 15,
            28, 29, 30, 31,
        ]
    );

    let a0_a1_a2_00__b0_b1_b2_00__c0_c1_c2_00__00_00_00_00 = simd_swizzle!(
        a0_a1_a2_00__b0_b1_b2_00__a8_a9_a10_00__b8_b9_b10_00,
        c0_c1_c2_00__c4_c5_c6_00__c8_c9_c10_00__00_00_00_00,
        [
            0, 1, 2, 3, 4, 5, 6, 7,
            16, 17, 18, 19,
            28, 29, 30, 31,
        ]
    );

    let a4_a5_a6_00__b4_b5_b6_00__c4_c5_c6_00__00_00_00_00 = simd_swizzle!(
        a4_a5_a6_00__b4_b5_b6_00__00_00_00_00__00_00_00_00,
        c0_c1_c2_00__c4_c5_c6_00__c8_c9_c10_00__00_00_00_00,
        [
            0, 1, 2, 3, 4, 5, 6, 7,
            20, 21, 22, 23,
            12, 13, 14, 15,
        ]
    );

    let a8_a9_a10_00__b8_b9_b10_00__c8_c9_c10_00__00_00_00_00 = simd_swizzle!(
        a0_a1_a2_00__b0_b1_b2_00__a8_a9_a10_00__b8_b9_b10_00,
        c0_c1_c2_00__c4_c5_c6_00__c8_c9_c10_00__00_00_00_00,
        [
             8,  9, 10, 11, 12, 13, 14, 15,
            24, 25, 26, 27, 28, 29, 30, 31,
        ]
    );

    rows[0] = a0_a1_a2_00__b0_b1_b2_00__c0_c1_c2_00__00_00_00_00;
    rows[1] = a4_a5_a6_00__b4_b5_b6_00__c4_c5_c6_00__00_00_00_00;
    rows[2] = a8_a9_a10_00__b8_b9_b10_00__c8_c9_c10_00__00_00_00_00;
}

#[inline]
fn rows_to_boxes(rows: &mut [u16x16; 9]) {
    transpose3x3(&mut rows[0..3]);
    transpose3x3(&mut rows[3..6]);
    transpose3x3(&mut rows[6..9]);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SolveStepResult {
    Continue,
    Solved,
    Unsolvable,
}

impl SimdGrid {
    fn solve_step(&mut self) -> SolveStepResult {
        let mut all_solved = MASK;
        let mut unsolvable = false;

        for this_row_index in 0..self.cells.len() {
            let this_row = &mut self.cells[this_row_index];

            let is_solved = is_solved(*this_row);
            let solved_cells = is_solved.select(*this_row, ZERO);
            let solved_digits = u16x16::splat(solved_cells.reduce_or());

            *this_row = is_solved.select(*this_row, *this_row & !solved_digits);
            unsolvable |= is_unsolvable(*this_row);

            for i in 0..9 {
                let digit = u16x16::splat(1 << i);
                let choices = (*this_row & digit).simd_eq(digit);
                if choices.to_bitmask().count_ones() == 1 {
                    *this_row = choices.select(digit, *this_row);
                }
            }

            for (other_row_index, other_row) in self.cells.iter_mut().enumerate() {
                if this_row_index == other_row_index {
                    continue;
                }

                *other_row &= !solved_cells;
                unsolvable |= is_unsolvable(*other_row);
            }

            all_solved &= is_solved.to_bitmask();
        }

        if all_solved == MASK {
            return SolveStepResult::Solved;
        }

        rows_to_boxes(&mut self.cells);
        for i in 0..self.cells.len() {
            let this_box = &mut self.cells[i];

            let is_solved = is_solved(*this_box);
            let solved_cells = is_solved.select(*this_box, ZERO);
            let solved_digits = u16x16::splat(solved_cells.reduce_or());

            *this_box = is_solved.select(*this_box, *this_box & !solved_digits);
            unsolvable |= is_unsolvable(*this_box);

            for i in 0..9 {
                let digit = u16x16::splat(1 << i);
                let choices = (*this_box & digit).simd_eq(digit);
                if choices.to_bitmask().count_ones() == 1 {
                    *this_box = choices.select(digit, *this_box);
                }
            }
        }
        rows_to_boxes(&mut self.cells);

        if unsolvable {
            SolveStepResult::Unsolvable
        } else {
            SolveStepResult::Continue
        }
    }

    fn guess(self, max_guessing_depth: usize, depth: usize) -> Result<Self, Self> {
        let mut min_opts = 10;
        let mut min_column = 16;
        let mut min_row = 10;

        for row in 0..self.cells.len() {
            for column in 0..11 {
                if (column % 4) == 3 {
                    continue;
                }

                let opts = self.cells[row][column].count_ones();
                if (opts > 1) && (opts < min_opts) {
                    min_opts = opts;
                    min_column = column;
                    min_row = row;
                }
            }
        }

        let mut opts = self.cells[min_row][min_column];
        while opts > 0 {
            let digit = opts & !(opts - 1);
            opts &= opts - 1;

            let mut new_grid = self.clone();
            new_grid.cells[min_row][min_column] = digit;

            if let Ok(solved) = new_grid.try_solve_rec(max_guessing_depth, depth) {
                return Ok(solved);
            }
        }

        Err(self)
    }

    fn try_solve_rec(&self, max_guessing_depth: usize, depth: usize) -> Result<Self, Self> {
        let mut prev = self.clone();
        loop {
            let mut next = prev.clone();

            match next.solve_step() {
                SolveStepResult::Continue => {
                    if next == prev {
                        if depth < max_guessing_depth {
                            return next.guess(max_guessing_depth, depth + 1);
                        } else {
                            return Err(next);
                        }
                    }
                }
                SolveStepResult::Solved => return Ok(next),
                SolveStepResult::Unsolvable => return Err(next),
            }

            prev = next;
        }
    }

    #[inline]
    pub fn try_solve(&self, max_guessing_depth: usize) -> Result<Self, Self> {
        self.try_solve_rec(max_guessing_depth, 0)
    }
}
