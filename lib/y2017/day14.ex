# Disk Defragmentation

defmodule Y2017.Day14 do
  alias Y2017.Day10, as: D10
  use Bitwise

  @puzzle_input "ugkiagan"
  @test_puzzle_input "flqrgnkx"

  # Number of used sectors
  def part1(input \\ @puzzle_input) do
    input
    |> input_as_mapset()
    |> MapSet.size
  end

  # Number of contiguous sector groups, where connection is horizontal and
  # vertical but not diagonal.
  #
  # 2007 is too high
  def part2(input \\ @puzzle_input) do
    input
    |> input_as_mapset()
    |> count_groups
  end

  # ================ part 2 ================

  defp count_groups(ones), do: count_groups(ones, 0)

  defp count_groups(ones, count) do
    # IO.puts "\ncount_groups" # DEBUG
    # debug_print_memory(ones) # DEBUG

    one_bit_coord = find_one_bit(ones)
    if one_bit_coord do
      count_groups(remove_group_touching(ones, one_bit_coord), count + 1)
    else
      count
    end
  end

  defp find_one_bit(ones) do
    if MapSet.size(ones) == 0 do
      nil
    else
      ones |> MapSet.to_list |> hd
    end
  end

  defp remove_group_touching(ones, one_bit_coord) when is_tuple(one_bit_coord) do
    remove_group_touching(ones, MapSet.new([one_bit_coord]))
  end

  defp remove_group_touching(ones, ones_coords) do
    if MapSet.size(ones_coords) == 0 do
      ones
    else
      coord = ones_coords |> MapSet.to_list |> hd
      tcs = touching_coords(ones, coord) |> MapSet.new
      new_ones = MapSet.difference(ones, MapSet.put(tcs, coord))
      new_ones_coords = MapSet.union(tcs, MapSet.delete(ones_coords, coord))
      remove_group_touching(new_ones, new_ones_coords)
    end
  end

  defp touching_coords(ones, {row, col}) do
    [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
    |> Enum.map(fn({dr, dc}) ->
      coord = {row + dr, col+dc}
      if MapSet.member?(ones, coord), do: coord
    end)
    |> Enum.filter(&(&1))
  end

  # ================ testing ================

  def test_puzzle_input, do: @test_puzzle_input

  def test_print do
    @test_puzzle_input
    |> input_as_mapset()
    |> debug_print_memory()
    :ok
  end

  def test_part1 do
    part1(@test_puzzle_input)
  end

  def test_part2 do
    part2(@test_puzzle_input)
  end

  # ================ helpers ================

  defp input_as_mapset(input) do
    input
    |> input_as_charlist()
    |> as_mapset()
  end

  defp input_as_charlist(input) do
    0..127
    |> Enum.map(fn i ->
      charlist = "#{input}-#{i}" |> String.to_charlist()
      D10.part2(256, charlist)
    end)
    |> Enum.map(&String.to_charlist/1)
  end

  defp as_mapset(charlist), do: as_mapset(charlist, 0, [])

  defp as_mapset([], _, coords), do: MapSet.new(coords)

  defp as_mapset([row | t], row_idx, coords) do
    {_, new_coords} =
      Enum.reduce(row, {0, coords}, fn(ch, {col_idx, cs}) ->
        i = hex_char_to_int(ch)
        new_cs = [
        (if (i &&& 8) != 0, do: {row_idx, col_idx    }),
        (if (i &&& 4) != 0, do: {row_idx, col_idx + 1}),
        (if (i &&& 2) != 0, do: {row_idx, col_idx + 2}),
        (if (i &&& 1) != 0, do: {row_idx, col_idx + 3})
        ]
        |> Enum.filter(&(&1))
        {col_idx + 4, new_cs ++ cs}
      end)
    as_mapset(t, row_idx + 1, new_coords)
  end

  defp hex_char_to_int(ch) when ch >= ?0 and ch <= ?9, do: ch - ?0
  defp hex_char_to_int(ch) when ch >= ?a and ch <= ?f, do: ch - ?a + 10

  defp debug_print_memory(ones, size \\ 16) do
    (0..size-1)
    |> Enum.map(fn(row) ->
      (0..size-1) |> Enum.map(fn(col) ->
        IO.write(if MapSet.member?(ones, {row, col}), do: "#", else: ".")
      end)
      IO.puts("")
    end)
  end
end
