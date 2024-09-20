use super::*;

pub trait Region<const GRID_SIZE: usize>: fmt::Debug
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Cells<'a>: Iterator<Item = &'a Cell<GRID_SIZE>>;
    type CellsMut<'a>: Iterator<Item = &'a mut Cell<GRID_SIZE>>;
    type CellPositions<'a>: Iterator<Item = (GridPosition, &'a Cell<GRID_SIZE>)>;
    type CellPositionsMut<'a>: Iterator<Item = (GridPosition, &'a mut Cell<GRID_SIZE>)>;

    fn contains(&self, pos: GridPosition) -> bool;

    fn cells_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::Cells<'a>;
    fn cells_mut_in<'a>(&self, grid: &'a mut Grid<GRID_SIZE>) -> Self::CellsMut<'a>;
    fn cell_positions_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::CellPositions<'a>;
    fn cell_positions_mut_in<'a>(
        &self,
        grid: &'a mut Grid<GRID_SIZE>,
    ) -> Self::CellPositionsMut<'a>;
}

impl<'r, R, const GRID_SIZE: usize> Region<GRID_SIZE> for &'r R
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    R: Region<GRID_SIZE>,
{
    type Cells<'a> = R::Cells<'a>;
    type CellsMut<'a> = R::CellsMut<'a>;
    type CellPositions<'a> = R::CellPositions<'a>;
    type CellPositionsMut<'a> = R::CellPositionsMut<'a>;

    #[inline]
    fn contains(&self, pos: GridPosition) -> bool {
        R::contains(&self, pos)
    }

    #[inline]
    fn cells_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::Cells<'a> {
        R::cells_in(self, grid)
    }

    #[inline]
    fn cells_mut_in<'a>(&self, grid: &'a mut Grid<GRID_SIZE>) -> Self::CellsMut<'a> {
        R::cells_mut_in(self, grid)
    }

    #[inline]
    fn cell_positions_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::CellPositions<'a> {
        R::cell_positions_in(self, grid)
    }

    #[inline]
    fn cell_positions_mut_in<'a>(
        &self,
        grid: &'a mut Grid<GRID_SIZE>,
    ) -> Self::CellPositionsMut<'a> {
        R::cell_positions_mut_in(self, grid)
    }
}

pub trait RegionGroup<const GRID_SIZE: usize>: fmt::Debug
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Regions<'a>: Iterator<Item: Region<GRID_SIZE>>
    where
        Self: 'a;

    fn regions<'a>(&'a self) -> Self::Regions<'a>;
}

impl<R, const GRID_SIZE: usize> RegionGroup<GRID_SIZE> for R
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
    R: Region<GRID_SIZE>,
{
    type Regions<'a> = impl Iterator<Item = &'a R>
    where
        Self: 'a;

    #[inline]
    fn regions<'a>(&'a self) -> Self::Regions<'a> {
        std::iter::once(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Column<const GRID_SIZE: usize>(pub usize)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> Region<GRID_SIZE> for Column<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Cells<'a> = impl Iterator<Item = &'a Cell<GRID_SIZE>> + 'a;
    type CellsMut<'a> = impl Iterator<Item = &'a mut Cell<GRID_SIZE>> + 'a;
    type CellPositions<'a> = impl Iterator<Item = (GridPosition, &'a Cell<GRID_SIZE>)> + 'a;
    type CellPositionsMut<'a> = impl Iterator<Item = (GridPosition, &'a mut Cell<GRID_SIZE>)> + 'a;

    #[inline]
    fn contains(&self, pos: GridPosition) -> bool {
        let column = self.0;
        assert!(column < GRID_SIZE);
        assert!(pos.column < GRID_SIZE);
        pos.column == column
    }

    #[inline]
    fn cells_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::Cells<'a> {
        let column = self.0;
        assert!(column < GRID_SIZE);
        grid.cells.iter().map(move |cells| &cells[column])
    }

    #[inline]
    fn cells_mut_in<'a>(&self, grid: &'a mut Grid<GRID_SIZE>) -> Self::CellsMut<'a> {
        let column = self.0;
        assert!(column < GRID_SIZE);
        grid.cells.iter_mut().map(move |cells| &mut cells[column])
    }

    fn cell_positions_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::CellPositions<'a> {
        let column = self.0;
        assert!(column < GRID_SIZE);

        grid.cells
            .iter()
            .map(move |cells| &cells[column])
            .enumerate()
            .map(move |(i, cell)| (GridPosition { column, row: i }, cell))
    }

    fn cell_positions_mut_in<'a>(
        &self,
        grid: &'a mut Grid<GRID_SIZE>,
    ) -> Self::CellPositionsMut<'a> {
        let column = self.0;
        assert!(column < GRID_SIZE);

        grid.cells
            .iter_mut()
            .map(move |cells| &mut cells[column])
            .enumerate()
            .map(move |(i, cell)| (GridPosition { column, row: i }, cell))
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct ColumnGroup<const GRID_SIZE: usize>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> RegionGroup<GRID_SIZE> for ColumnGroup<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Regions<'a> = impl Iterator<Item = Column<GRID_SIZE>> where Self: 'a;

    #[inline]
    fn regions<'a>(&'a self) -> Self::Regions<'a> {
        (0..GRID_SIZE).map(Column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Row<const GRID_SIZE: usize>(pub usize)
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> Region<GRID_SIZE> for Row<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Cells<'a> = impl Iterator<Item = &'a Cell<GRID_SIZE>> + 'a;
    type CellsMut<'a> = impl Iterator<Item = &'a mut Cell<GRID_SIZE>> + 'a;
    type CellPositions<'a> = impl Iterator<Item = (GridPosition, &'a Cell<GRID_SIZE>)>;
    type CellPositionsMut<'a> = impl Iterator<Item = (GridPosition, &'a mut Cell<GRID_SIZE>)>;

    #[inline]
    fn contains(&self, pos: GridPosition) -> bool {
        let row = self.0;
        assert!(row < GRID_SIZE);
        assert!(pos.row < GRID_SIZE);
        pos.row == row
    }

    #[inline]
    fn cells_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::Cells<'a> {
        let row = self.0;
        assert!(row < GRID_SIZE);
        grid.cells[row].iter()
    }

    #[inline]
    fn cells_mut_in<'a>(&self, grid: &'a mut Grid<GRID_SIZE>) -> Self::CellsMut<'a> {
        let row = self.0;
        assert!(row < GRID_SIZE);
        grid.cells[row].iter_mut()
    }

    fn cell_positions_in<'a>(&self, grid: &'a Grid<GRID_SIZE>) -> Self::CellPositions<'a> {
        let row = self.0;
        assert!(row < GRID_SIZE);

        grid.cells[row]
            .iter()
            .enumerate()
            .map(move |(i, cell)| (GridPosition { column: i, row }, cell))
    }

    fn cell_positions_mut_in<'a>(
        &self,
        grid: &'a mut Grid<GRID_SIZE>,
    ) -> Self::CellPositionsMut<'a> {
        let row = self.0;
        assert!(row < GRID_SIZE);

        grid.cells[row]
            .iter_mut()
            .enumerate()
            .map(move |(i, cell)| (GridPosition { column: i, row }, cell))
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct RowGroup<const GRID_SIZE: usize>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate;

impl<const GRID_SIZE: usize> RegionGroup<GRID_SIZE> for RowGroup<GRID_SIZE>
where
    GridTemplate<GRID_SIZE>: ValidGridTemplate,
{
    type Regions<'a> = impl Iterator<Item = Row<GRID_SIZE>> where Self: 'a;

    #[inline]
    fn regions<'a>(&'a self) -> Self::Regions<'a> {
        (0..GRID_SIZE).map(Row)
    }
}

macro_rules! box_region {
    ([$box_columns:literal, $box_rows:literal] => ($box_name:ident, $group_name:ident)) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(transparent)]
        pub struct $box_name(pub usize);

        impl Region<{ $box_columns * $box_rows }> for $box_name {
            type Cells<'a> = impl Iterator<Item = &'a Cell<{ $box_columns * $box_rows }>>;
            type CellsMut<'a> = impl Iterator<Item = &'a mut Cell<{ $box_columns * $box_rows }>>;
            type CellPositions<'a> =
                impl Iterator<Item = (GridPosition, &'a Cell<{ $box_columns * $box_rows }>)>;
            type CellPositionsMut<'a> =
                impl Iterator<Item = (GridPosition, &'a mut Cell<{ $box_columns * $box_rows }>)>;

            fn contains(&self, pos: GridPosition) -> bool {
                assert!(self.0 < ($box_columns * $box_rows));
                assert!(pos.column < ($box_columns * $box_rows));
                assert!(pos.row < ($box_columns * $box_rows));

                let column_offset = (self.0 % $box_rows) * $box_columns;
                let row_offset = (self.0 / $box_columns) * $box_rows;

                (column_offset..(column_offset + $box_columns)).contains(&pos.column)
                    && (row_offset..(row_offset + $box_rows)).contains(&pos.row)
            }

            fn cells_in<'a>(
                &self,
                grid: &'a Grid<{ $box_columns * $box_rows }>,
            ) -> Self::Cells<'a> {
                assert!(self.0 < ($box_columns * $box_rows));

                let column_offset = (self.0 % $box_rows) * $box_columns;
                let row_offset = (self.0 / $box_columns) * $box_rows;

                grid.cells[row_offset..(row_offset + $box_rows)]
                    .iter()
                    .flat_map(move |cells| {
                        cells[column_offset..(column_offset + $box_columns)].iter()
                    })
            }

            fn cells_mut_in<'a>(
                &self,
                grid: &'a mut Grid<{ $box_columns * $box_rows }>,
            ) -> Self::CellsMut<'a> {
                assert!(self.0 < ($box_columns * $box_rows));

                let column_offset = (self.0 % $box_rows) * $box_columns;
                let row_offset = (self.0 / $box_columns) * $box_rows;

                grid.cells[row_offset..(row_offset + $box_rows)]
                    .iter_mut()
                    .flat_map(move |cells| {
                        cells[column_offset..(column_offset + $box_columns)].iter_mut()
                    })
            }

            fn cell_positions_in<'a>(
                &self,
                grid: &'a Grid<{ $box_columns * $box_rows }>,
            ) -> Self::CellPositions<'a> {
                assert!(self.0 < ($box_columns * $box_rows));

                let column_offset = (self.0 % $box_rows) * $box_columns;
                let row_offset = (self.0 / $box_columns) * $box_rows;

                grid.cells[row_offset..(row_offset + $box_rows)]
                    .iter()
                    .enumerate()
                    .flat_map(move |(row, cells)| {
                        cells[column_offset..(column_offset + $box_columns)]
                            .iter()
                            .enumerate()
                            .map(move |(column, cell)| {
                                (
                                    GridPosition {
                                        column: column + column_offset,
                                        row: row + row_offset,
                                    },
                                    cell,
                                )
                            })
                    })
            }

            fn cell_positions_mut_in<'a>(
                &self,
                grid: &'a mut Grid<{ $box_columns * $box_rows }>,
            ) -> Self::CellPositionsMut<'a> {
                assert!(self.0 < ($box_columns * $box_rows));

                let column_offset = (self.0 % $box_rows) * $box_columns;
                let row_offset = (self.0 / $box_columns) * $box_rows;

                grid.cells[row_offset..(row_offset + $box_rows)]
                    .iter_mut()
                    .enumerate()
                    .flat_map(move |(row, cells)| {
                        cells[column_offset..(column_offset + $box_columns)]
                            .iter_mut()
                            .enumerate()
                            .map(move |(column, cell)| {
                                (
                                    GridPosition {
                                        column: column + column_offset,
                                        row: row + row_offset,
                                    },
                                    cell,
                                )
                            })
                    })
            }
        }

        #[derive(Debug, Default, Clone, Copy)]
        pub struct $group_name;

        impl RegionGroup<{ $box_columns * $box_rows }> for $group_name {
            type Regions<'a> = impl Iterator<Item = $box_name> where Self: 'a;

            #[inline]
            fn regions<'a>(&'a self) -> Self::Regions<'a> {
                (0..($box_columns * $box_rows)).map($box_name)
            }
        }
    };
}

box_region!([2, 2] => (Box2x2, Box2x2Group));
box_region!([3, 2] => (Box3x2, Box3x2Group));
box_region!([2, 3] => (Box2x3, Box2x3Group));
box_region!([3, 3] => (Box3x3, Box3x3Group));
box_region!([4, 3] => (Box4x3, Box4x3Group));
box_region!([3, 4] => (Box3x4, Box3x4Group));
box_region!([4, 4] => (Box4x4, Box4x4Group));
