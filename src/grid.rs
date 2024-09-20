pub mod region;

use crate::rule::RuleSet;
use std::fmt;
use std::ops::*;
use std::str::FromStr;

pub trait CellStorage:
    fmt::Debug
    + fmt::Display
    + Copy
    + Eq
    + Ord
    + std::hash::Hash
    + BitAnd<Output = Self>
    + BitAndAssign
    + BitOr<Output = Self>
    + BitOrAssign
    + BitXor<Output = Self>
    + BitXorAssign
    + Not<Output = Self>
    + Add<Output = Self>
    + AddAssign
    + Sub<Output = Self>
    + SubAssign
    + Shl<u8, Output = Self>
    + ShlAssign<u8>
    + Shr<u8, Output = Self>
    + ShrAssign<u8>
{
    const ZERO: Self;
    const ONE: Self;

    fn option_count(self) -> u8;
    fn first_option(self) -> u8;
}

macro_rules! impl_cell_storage {
    ($t:ty) => {
        impl CellStorage for $t {
            const ZERO: Self = 0;
            const ONE: Self = 1;

            #[inline]
            fn option_count(self) -> u8 {
                self.count_ones() as u8
            }

            #[inline]
            fn first_option(self) -> u8 {
                self.trailing_zeros() as u8
            }
        }
    };
}

impl_cell_storage!(u8);
impl_cell_storage!(u16);

pub enum GridTemplate<const N: usize> {}

pub trait ValidGridTemplate {
    type CellStorage: CellStorage;
}

impl ValidGridTemplate for GridTemplate<4> {
    type CellStorage = u8;
}

impl ValidGridTemplate for GridTemplate<6> {
    type CellStorage = u8;
}

impl ValidGridTemplate for GridTemplate<9> {
    type CellStorage = u16;
}

impl ValidGridTemplate for GridTemplate<12> {
    type CellStorage = u16;
}

impl ValidGridTemplate for GridTemplate<16> {
    type CellStorage = u16;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Digit<const GRID_SIZE: usize>(u8)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> Digit<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    pub const ALL: [Self; GRID_SIZE] = {
        let mut all = [Self(0); GRID_SIZE];
        let mut i = 0u8;
        while i < (GRID_SIZE as u8) {
            all[i as usize] = Self(i);
            i += 1;
        }
        all
    };

    #[inline]
    pub fn from_offset(value: u8) -> Option<Self> {
        if value < (Self::ALL.len() as u8) {
            Some(unsafe { std::mem::transmute(value) })
        } else {
            None
        }
    }

    #[inline]
    pub fn offset(self) -> u8 {
        self.0
    }

    #[inline]
    pub fn from_value(value: u8) -> Option<Self> {
        value.checked_sub(1).and_then(Self::from_offset)
    }

    #[inline]
    pub fn value(self) -> u8 {
        self.offset() + 1
    }
}

impl<const GRID_SIZE: usize> fmt::Display for Digit<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.value(), f)
    }
}

type CellStorageFor<const GRID_SIZE: usize> =
    <GridTemplate<GRID_SIZE> as ValidGridTemplate>::CellStorage;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Cell<const GRID_SIZE: usize>(
    <GridTemplate<GRID_SIZE> as ValidGridTemplate>::CellStorage,
)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> Cell<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    pub fn all_options() -> Self {
        let mut state = CellStorageFor::<GRID_SIZE>::ZERO;
        for digit in Digit::<GRID_SIZE>::ALL {
            state |= CellStorageFor::<GRID_SIZE>::ONE << digit.offset();
        }
        Self(state)
    }

    #[inline]
    pub fn solved(digit: Digit<GRID_SIZE>) -> Self {
        Self(CellStorageFor::<GRID_SIZE>::ONE << digit.offset())
    }

    #[inline]
    pub fn option_count(self) -> u8 {
        self.0.option_count()
    }

    #[inline]
    pub fn is_solved(&self) -> bool {
        self.option_count() == 1
    }

    #[inline]
    pub fn is_solvable(self) -> bool {
        self.option_count() > 0
    }

    pub fn first_option(self) -> Option<Digit<GRID_SIZE>> {
        if self.is_solvable() {
            Some(
                Digit::<GRID_SIZE>::from_offset(self.0.first_option()).expect("invalid cell state"),
            )
        } else {
            None
        }
    }

    pub fn solved_digit(self) -> Option<Digit<GRID_SIZE>> {
        if self.is_solved() {
            Some(
                Digit::<GRID_SIZE>::from_offset(self.0.first_option()).expect("invalid cell state"),
            )
        } else {
            None
        }
    }

    #[inline]
    pub fn has_option(self, digit: Digit<GRID_SIZE>) -> bool {
        let digit_bit = CellStorageFor::<GRID_SIZE>::ONE << digit.offset();
        (self.0 & digit_bit) > CellStorageFor::<GRID_SIZE>::ZERO
    }

    #[inline]
    pub fn insert_option(&mut self, digit: Digit<GRID_SIZE>) {
        let digit_bit = CellStorageFor::<GRID_SIZE>::ONE << digit.offset();
        self.0 |= digit_bit;
    }

    #[inline]
    pub fn remove_option(&mut self, digit: Digit<GRID_SIZE>) {
        let digit_bit = CellStorageFor::<GRID_SIZE>::ONE << digit.offset();
        self.0 &= !digit_bit;
    }

    #[inline]
    pub fn intersect(&mut self, other: Self) {
        self.0 &= other.0;
    }

    #[inline]
    pub fn subtract(&mut self, other: Self) {
        self.0 &= !other.0;
    }

    #[inline]
    pub fn union(&mut self, other: Self) {
        self.0 |= other.0;
    }
}

impl<const GRID_SIZE: usize> fmt::Display for Cell<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(digit) = self.solved_digit() {
            write!(f, "    {digit}    ")
        } else {
            for digit in Digit::<GRID_SIZE>::ALL {
                if self.has_option(digit) {
                    write!(f, "{digit}")?;
                } else {
                    write!(f, "·")?;
                }
            }

            Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GridPosition {
    pub column: usize,
    pub row: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Grid<const SIZE: usize>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    cells: [[Cell<SIZE>; SIZE]; SIZE],
}

impl<const SIZE: usize> Grid<SIZE>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    pub fn empty() -> Self {
        Self {
            cells: [[Cell::all_options(); SIZE]; SIZE],
        }
    }

    pub fn is_solved(&self) -> bool {
        self.cells.iter().flatten().all(Cell::is_solved)
    }
}

impl<const SIZE: usize> Index<GridPosition> for Grid<SIZE>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    type Output = Cell<SIZE>;

    #[inline]
    fn index(&self, position: GridPosition) -> &Self::Output {
        &self.cells[position.row][position.column]
    }
}

impl<const SIZE: usize> IndexMut<GridPosition> for Grid<SIZE>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    #[inline]
    fn index_mut(&mut self, position: GridPosition) -> &mut Self::Output {
        &mut self.cells[position.row][position.column]
    }
}

#[derive(Debug, Clone)]
pub enum GridFromStrError {
    InvalidLength,
    InvalidChar,
}

impl<const SIZE: usize> FromStr for Grid<SIZE>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    type Err = GridFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.as_bytes();
        if s.len() != (SIZE * SIZE) {
            return Err(GridFromStrError::InvalidLength);
        }

        let mut grid = Grid::empty();
        for (&c, cell) in s.iter().zip(grid.cells.iter_mut().flatten()) {
            let value = c.checked_sub(b'0').ok_or(GridFromStrError::InvalidChar)?;

            if value > 0 {
                let digit = Digit::from_value(value).ok_or(GridFromStrError::InvalidChar)?;
                *cell = Cell::solved(digit);
            }
        }

        Ok(grid)
    }
}

impl Grid<9> {
    fn fmt_row(&self, row: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for column in 0..9 {
            if [0, 3, 6].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if cell.option_count() <= 1 {
                write!(f, "      ")?;
            } else {
                for digit in [Digit(0), Digit(1), Digit(2)] {
                    if cell.has_option(digit) {
                        write!(f, "{digit} ")?;
                    } else {
                        write!(f, "· ")?;
                    }
                }
            }
        }
        writeln!(f, "┃")?;

        for column in 0..9 {
            if [0, 3, 6].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if let Some(digit) = cell.solved_digit() {
                write!(f, "  {digit}   ")?;
            } else if !cell.is_solvable() {
                write!(f, "  X   ")?;
            } else {
                for digit in [Digit(3), Digit(4), Digit(5)] {
                    if cell.has_option(digit) {
                        write!(f, "{digit} ")?;
                    } else {
                        write!(f, "· ")?;
                    }
                }
            }
        }
        writeln!(f, "┃")?;

        for column in 0..9 {
            if [0, 3, 6].contains(&column) {
                write!(f, "┃ ")?;
            } else {
                write!(f, "│ ")?;
            }

            let cell = self.cells[row][column];
            if cell.option_count() <= 1 {
                write!(f, "      ")?;
            } else {
                for digit in [Digit(6), Digit(7), Digit(8)] {
                    if cell.has_option(digit) {
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

impl fmt::Display for Grid<9> {
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

impl<const SIZE: usize> Grid<SIZE>
where
    GridTemplate<SIZE>: ValidGridTemplate,
{
    fn guess(
        self,
        rules: impl RuleSet<SIZE> + Copy,
        max_guessing_depth: usize,
        depth: usize,
    ) -> Result<Self, Self> {
        let mut min_opts = SIZE as u8;
        let mut min_pos = GridPosition {
            column: SIZE,
            row: SIZE,
        };

        for row in 0..SIZE {
            for column in 0..SIZE {
                let opts = self.cells[row][column].option_count();
                let pos = GridPosition { column, row };

                if (opts > 1) && (opts < min_opts) {
                    min_opts = opts;
                    min_pos = pos;
                }
            }
        }

        let mut opts = self.cells[min_pos.row][min_pos.column];
        while let Some(digit) = opts.first_option() {
            opts.remove_option(digit);

            let mut new_grid = self.clone();
            new_grid.cells[min_pos.row][min_pos.column] = Cell::solved(digit);

            if let Ok(solved) = new_grid.try_solve_rec(rules, max_guessing_depth, depth) {
                return Ok(solved);
            }
        }

        Err(self)
    }

    fn try_solve_rec(
        &self,
        rules: impl RuleSet<SIZE> + Copy,
        max_guessing_depth: usize,
        depth: usize,
    ) -> Result<Self, Self> {
        let mut prev = self.clone();
        loop {
            let next = rules.apply(&prev)?;
            if next.is_solved() {
                return Ok(next);
            } else if next == prev {
                if depth < max_guessing_depth {
                    return next.guess(rules, max_guessing_depth, depth + 1);
                } else {
                    return Err(next);
                }
            }

            prev = next;
        }
    }

    #[inline]
    pub fn try_solve(
        &self,
        rules: impl RuleSet<SIZE> + Copy,
        max_guessing_depth: usize,
    ) -> Result<Self, Self> {
        self.try_solve_rec(rules, max_guessing_depth, 0)
    }
}
