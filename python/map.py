"""A two-dimensional map where each cell is a single value.

[0, 0] is at the top left of the map. row increases down, col increases
to the right.
"""

from copy import deepcopy
from typing import Any, Callable, Iterator, Optional


class Map:
    def __init__(self, lines: list[str], wrap_type: Optional[str] = None):
        """Given `lines` of text, initializes a two-dimensional map of characters.
        Assumes all lines are the same length.

        `wrap_type` may be None, 'both', 'row', or 'col'.
        """
        self.rows = [list(line) for line in lines]
        self.wrap_type = wrap_type
        self.height = len(self.rows)
        self.width = len(self.rows[0]) if self.rows else 0

    @classmethod
    def from_size(
        cls, num_rows: int, num_cols: int, initial_value: str = "."
    ) -> "Map":
        """Create a map of the given size filled with initial_value."""
        return cls([initial_value * num_cols for _ in range(num_rows)])

    def cells_to_ints(self) -> None:
        """Converts each cell's character, presumably a single digit char, to an integer."""
        for row_idx in range(self.height):
            self.rows[row_idx] = [int(cell) for cell in self.rows[row_idx]]

    def at(self, row: int, col: int) -> Optional[Any]:
        """Returns the value at [row][col]."""
        row, col = self.wrap(row, col)
        if not self.in_bounds(row, col):
            return None
        return self.rows[row][col]

    def get_row(self, row: int) -> list[Any]:
        """Returns the specified row."""
        return self.rows[row]

    def columns(self) -> list[list[Any]]:
        """Returns all columns as a list of lists."""
        return [self.column(col) for col in range(self.width)]

    def column(self, col: int) -> list[Any]:
        """Returns the specified column."""
        return [row[col] for row in self.rows]

    def find(self, val: Any) -> Optional[tuple[int, int]]:
        """Returns the first occurrence of `val` (min row, then min col) as a
        two-element tuple containing (row, col), or None if not found.
        """
        for row_idx, row in enumerate(self.rows):
            try:
                col_idx = row.index(val)
                return (row_idx, col_idx)
            except ValueError:
                continue
        return None

    def each(self) -> Iterator[tuple[int, int, Any]]:
        """For each row and column, yields the row number, column number, and value."""
        for row_idx, row in enumerate(self.rows):
            for col_idx, val in enumerate(row):
                yield row_idx, col_idx, val

    def clone(self) -> "Map":
        """Returns a deep copy of the map."""
        new_map = Map.__new__(Map)
        new_map.rows = deepcopy(self.rows)
        new_map.wrap_type = self.wrap_type
        new_map.height = self.height
        new_map.width = self.width
        return new_map

    def set(self, row: int, col: int, val: Any = None) -> None:
        """Sets value at `row`, `col` to `val`."""
        row, col = self.wrap(row, col)
        self.rows[row][col] = val

    def delete_row(self, row: int) -> None:
        """Delete the specified row (defaults to current row).

        Does nothing if row is out of bounds."""
        if row < 0 or row >= self.height or self.height == 0:
            return
        del self.rows[row]
        self.height -= 1

    def insert_row(self, index: int, row: list[Any]) -> None:
        """Insert a row at the specified index."""
        self.rows.insert(index, row.copy())
        self.height += 1

    def delete_col(self, col: Optional[int] = None) -> None:
        """Delete the specified column (defaults to current column)."""
        if col is None:
            col = self.col
        if col < 0 or col >= self.width or self.width == 0:
            return
        for row in self.rows:
            del row[col]
        self.width -= 1

    def insert_col(self, index: int, col: list[Any]) -> None:
        """Insert a column at the specified index."""
        for row_idx, row in enumerate(self.rows):
            row.insert(index, col[row_idx])
        self.width += 1

    def update_all(self, func: Callable[[Any], Any]) -> None:
        """Calls func on each cell and updates the cell with the result."""
        for row_idx, col_idx, val in self.each():
            self.rows[row_idx][col_idx] = func(val)

    def update(self, func: Callable[[Any], Any], row: int, col: int) -> None:
        """Updates value at `row`, `col` by calling func with its value and storing the result."""
        self.rows[row][col] = func(self.at(row, col))

    def in_bounds(self, row: int, col: int) -> bool:
        """Check if the given position (or current position) is in bounds."""
        return 0 <= row < self.height and 0 <= col < self.width

    def wrap(self, row: int, col: int) -> tuple[int, int]:
        """Wrap coordinates based on wrap_type.

        wrap_type may be None, 'both', 'row', or 'col'.
        """
        if self.wrap_type == "both":
            row %= self.height
            col %= self.width
        elif self.wrap_type == "row":
            row %= self.height
        elif self.wrap_type == "col":
            col %= self.width
        return row, col

    def __str__(self) -> str:
        """String representation of the map."""
        return "\n".join(
            "".join(str(cell) for cell in row) for row in self.rows
        )

    def pretty(self) -> str:
        """Stretches out each row so that a square map prints closer to a square."""
        return "\n".join(
            " ".join(str(cell) for cell in row) for row in self.rows
        )

    def flood_fill(
        self, row: int, col: int, old_value: Any, new_value: Any
    ) -> None:
        """A non-recursive flood fill that changes `old_value` to `new_value`
        starting at (`row`, `col`).
        """
        queue = [(row, col)]
        while queue:
            r, c = queue.pop(0)
            if not self.in_bounds(r, c):
                continue

            ch = self.at(r, c)
            if ch != old_value:
                continue

            self.set(r, c, new_value)
            queue.append((r - 1, c))
            queue.append((r + 1, c))
            queue.append((r, c - 1))
            queue.append((r, c + 1))
