# ================ Printing Department ================

defmodule Y2025.Day04 do
  @roll "@"

  def part1(_ctx, lines) do
    roll_locs = parse_2d(lines)

    roll_locs
    |> Enum.reduce(0, fn pos, acc ->
      acc + if(accessable_roll?(roll_locs, pos), do: 1, else: 0)
    end)
  end

  def part2(_ctx, lines) do
    roll_locs = parse_2d(lines)
    count_removables(roll_locs)
  end

  # ================ helpers ================

  # Returns a MapSet containing @roll {row, col} locations.
  defp parse_2d(lines) do
    lines
    |> Enum.with_index()
    |> Enum.reduce(MapSet.new(), fn {line, r}, roll_locs ->
      line
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.reduce(roll_locs, fn {cell, c}, acc ->
        if(cell == @roll, do: MapSet.put(acc, {r, c}), else: acc)
      end)
    end)
  end

  defp accessable_roll?(roll_locs, pos) do
    neighbor_count(roll_locs, pos) < 4
  end

  defp neighbor_count(roll_locs, {row, col}) do
    (row - 1)..(row + 1)
    |> Enum.reduce(0, fn r, row_acc ->
      (col - 1)..(col + 1)
      |> Enum.reduce(row_acc, fn c, col_acc ->
        if not (r == row and c == col) and MapSet.member?(roll_locs, {r, c}),
          do: col_acc + 1,
          else: col_acc
      end)
    end)
  end

  defp count_removables(roll_locs) do
    count_removables(roll_locs, find_removables(roll_locs), 0)
  end

  defp count_removables(_, [], total), do: total

  defp count_removables(roll_locs, removables, total) do
    new_roll_locs = remove(roll_locs, removables)
    new_movables = find_removables(new_roll_locs)
    count_removables(new_roll_locs, new_movables, total + length(removables))
  end

  def find_removables(roll_locs) do
    roll_locs
    |> Enum.reduce([], fn roll_pos, acc ->
      if accessable_roll?(roll_locs, roll_pos), do: [roll_pos | acc], else: acc
    end)
  end

  def remove(roll_locs, removables) do
    MapSet.reject(roll_locs, &(&1 in removables))
  end
end
