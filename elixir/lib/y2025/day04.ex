# ================ Printing Department ================

defmodule Y2025.Day04 do
  def part1(_ctx, lines) do
    map = parse_2d(lines)
    num_cols = length(Enum.at(map, 0))

    0..(length(map) - 1)
    |> Enum.reduce(0, fn row, row_acc ->
      0..(num_cols - 1)
      |> Enum.reduce(row_acc, fn col, col_acc ->
        col_acc + if(accessable_roll?(map, row, col), do: 1, else: 0)
      end)
    end)
  end

  def part2(_ctx, lines) do
    IO.puts(lines)
  end

  # ================ helpers ================

  defp parse_2d(lines) do
    Enum.map(lines, &String.codepoints/1)
  end

  def at(map, row, col) do
    Enum.at(Enum.at(map, row), col)
  end

  defp accessable_roll?(map, row, col) do
    cell = at(map, row, col)
    cell == "@" and neighbor_count(map, row, col) < 4
  end

  defp neighbor_count(map, row, col) do
    num_cols = length(Enum.at(map, 0))

    (row - 1)..(row + 1)
    |> Enum.reduce(0, fn r, row_acc ->
      (col - 1)..(col + 1)
      |> Enum.reduce(row_acc, fn c, col_acc ->
        if r == row and col == c do
          col_acc
        else
          ok_coords = in_bounds?(length(map), num_cols, r, c)
          if ok_coords and at(map, r, c) == "@", do: col_acc + 1, else: col_acc
        end
      end)
    end)
  end

  defp in_bounds?(num_rows, num_cols, r, c) do
    r >= 0 and c >= 0 and r < num_rows and c < num_cols
  end
end
