pub mod standard;

use crate::grid::*;
use std::ops::{Index, IndexMut};

pub trait RuleSet<const GRID_SIZE: usize>: std::fmt::Debug
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>>;
}

impl<'a, S, const GRID_SIZE: usize> RuleSet<GRID_SIZE> for &'a S
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    S: RuleSet<GRID_SIZE>,
{
    #[inline]
    fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
        S::apply(self, grid)
    }
}

macro_rules! impl_tuple {
    ($($t:ident),+ $(,)?) => {
        impl<$($t,)+ const GRID_SIZE: usize> RuleSet<GRID_SIZE> for ($($t,)+)
        where
            GridTemplate<GRID_SIZE>: ValidGridTemplate,
            ($($t,)+): std::fmt::Debug,
            $($t: RuleSet<GRID_SIZE>,)+
        {
            fn apply(&self, grid: &Grid<GRID_SIZE>) -> Result<Grid<GRID_SIZE>, Grid<GRID_SIZE>> {
                #[allow(non_snake_case)]
                let ($($t,)+) = self;
                $(let grid = RuleSet::apply($t, &grid)?;)+
                Ok(grid)
            }
        }
    };
}

impl_tuple!(T1);
impl_tuple!(T1, T2);
impl_tuple!(T1, T2, T3);
impl_tuple!(T1, T2, T3, T4);
impl_tuple!(T1, T2, T3, T4, T5);
impl_tuple!(T1, T2, T3, T4, T5, T6);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);

#[derive(Debug, Clone)]
#[repr(transparent)]
struct DigitMap<T, const GRID_SIZE: usize>([T; GRID_SIZE])
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<T: Default, const GRID_SIZE: usize> Default for DigitMap<T, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    #[inline]
    fn default() -> Self {
        Self(std::array::from_fn(|_| T::default()))
    }
}

impl<T, const GRID_SIZE: usize> Index<Digit<GRID_SIZE>> for DigitMap<T, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Output = T;

    #[inline]
    fn index(&self, index: Digit<GRID_SIZE>) -> &Self::Output {
        &self.0[(index.offset()) as usize]
    }
}

impl<T, const GRID_SIZE: usize> IndexMut<Digit<GRID_SIZE>> for DigitMap<T, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    #[inline]
    fn index_mut(&mut self, index: Digit<GRID_SIZE>) -> &mut Self::Output {
        &mut self.0[(index.offset()) as usize]
    }
}

impl<'a, T, const GRID_SIZE: usize> IntoIterator for &'a DigitMap<T, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Item = (Digit<GRID_SIZE>, &'a T);
    type IntoIter = impl Iterator<Item = (Digit<GRID_SIZE>, &'a T)>;

    fn into_iter(self) -> Self::IntoIter {
        Digit::<GRID_SIZE>::ALL.iter().copied().zip(&self.0)
    }
}

impl<'a, T, const GRID_SIZE: usize> IntoIterator for &'a mut DigitMap<T, GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Item = (Digit<GRID_SIZE>, &'a mut T);
    type IntoIter = impl Iterator<Item = (Digit<GRID_SIZE>, &'a mut T)>;

    fn into_iter(self) -> Self::IntoIter {
        Digit::<GRID_SIZE>::ALL.iter().copied().zip(&mut self.0)
    }
}
