# ================ Printing Department ================

defmodule Y2025.Day04 do
  @roll "@"

  def part1(_ctx, lines) do
    map = parse_2d(lines)

    map
    |> Map.keys()
    |> Enum.reduce(0, fn pos, acc ->
      acc + if(accessable_roll?(map, pos), do: 1, else: 0)
    end)
  end

  def part2(_ctx, lines) do
    map = parse_2d(lines)
    count_removables(map)
  end

  # ================ helpers ================

  # Returns a map acting as a set. Keys are {row, col} locations of @roll
  # characters. Values are all 1 but they are ignored.
  defp parse_2d(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, r}, roll_locs ->
      line
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.reduce(roll_locs, fn {cell, c}, acc ->
        if(cell == @roll, do: Map.put(acc, {r, c}, 1), else: acc)
      end)
    end)
  end

  defp accessable_roll?(map, pos) do
    neighbor_count(map, pos) < 4
  end

  defp neighbor_count(map, {row, col}) do
    (row - 1)..(row + 1)
    |> Enum.reduce(0, fn r, row_acc ->
      (col - 1)..(col + 1)
      |> Enum.reduce(row_acc, fn c, col_acc ->
        if not (r == row and c == col) and Map.has_key?(map, {r, c}),
          do: col_acc + 1,
          else: col_acc
      end)
    end)
  end

  defp count_removables(map) do
    count_removables(map, find_removables(map), 0)
  end

  defp count_removables(_, [], total), do: total

  defp count_removables(map, removables, total) do
    new_map = remove(map, removables)
    new_movables = find_removables(new_map)
    count_removables(new_map, new_movables, total + length(removables))
  end

  def find_removables(map) do
    map
    |> Map.keys()
    |> Enum.reduce([], fn roll_pos, acc ->
      if accessable_roll?(map, roll_pos), do: [roll_pos | acc], else: acc
    end)
  end

  def remove(map, removables) do
    Map.reject(map, fn {pos, _} -> pos in removables end)
  end
end
