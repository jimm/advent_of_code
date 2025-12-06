# Perfectly Spherical Houses in a Vacuum

defmodule Y2015.Day03 do
  require Integer

  # received_present_count
  def part1(_ctx, lines) do
    lines
    |> hd
    |> String.codepoints()
    |> locs_and_counts
    |> Map.keys()
    |> length
  end

  # santa_plus_robo_present_count
  def part2(_ctx, lines) do
    directions_with_indexes =
      lines
      |> hd
      |> String.codepoints()
      |> Enum.with_index()

    {odd_dirs_and_indexes, even_dirs_and_indexes} =
      directions_with_indexes
      |> Enum.split_with(fn {_, idx} -> Integer.is_odd(idx) end)

    odd_dirs = odd_dirs_and_indexes |> Enum.map(fn {dir, _} -> dir end)
    even_dirs = even_dirs_and_indexes |> Enum.map(fn {dir, _} -> dir end)

    odd_dirs
    |> locs_and_counts
    |> Map.merge(even_dirs |> locs_and_counts)
    |> Map.keys()
    |> length
  end

  defp locs_and_counts(directions) do
    {_, counts} =
      directions
      |> Enum.reduce({{0, 0}, %{{0, 0} => 1}}, fn c, {curr_pos, counts} ->
        new_pos = move(curr_pos, c)
        {new_pos, Map.update(counts, new_pos, 1, &(&1 + 1))}
      end)

    counts
  end

  defp move({x, y}, c) when c == "^", do: {x, y + 1}
  defp move({x, y}, c) when c == ">", do: {x + 1, y}
  defp move({x, y}, c) when c == "v", do: {x, y - 1}
  defp move({x, y}, c) when c == "<", do: {x - 1, y}
end
