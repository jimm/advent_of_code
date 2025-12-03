defmodule Y2016.Day13 do
  import Bitwise
  require Integer

  @fave_number 1352
  @too_far 0xFFFFFFFF

  def run1(fave_number \\ @fave_number, x \\ 31, y \\ 39) do
    distance_to(fave_number, {x, y})
  end

  def run2 do
    reachable_in(50, @fave_number) |> Enum.count()
  end

  # ================ part one ================

  defp distance_to(fave_number, goal_loc) do
    dist(%{}, fave_number, %{}, {1, 1}, goal_loc, 0)
  end

  defp dist(_, _, _, goal_loc, goal_loc, num_steps), do: num_steps
  defp dist(_, _, _, {x, y}, _, _) when x < 0 or y < 0, do: @too_far

  defp dist(map, fnum, visited, {x, y} = curr_loc, goal_loc, num_steps) do
    new_locs = [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
    new_map = new_locs |> Enum.reduce(map, fn loc, m -> expand_map(m, fnum, loc) end)

    if wall?(new_map, curr_loc) do
      @too_far
    else
      new_locs
      |> Enum.filter(fn loc -> Map.get(visited, loc) != true end)
      |> Enum.sort_by(fn loc -> taxi_dist(loc, goal_loc) end)
      |> Enum.map(
        &dist(new_map, fnum, Map.put(visited, curr_loc, true), &1, goal_loc, num_steps + 1)
      )
      |> Enum.min()
    end
  end

  # ================ part two ================

  defp reachable_in(steps, fnum) do
    reachable_in(%{}, fnum, %{{1, 1} => true}, [{1, 1}], steps)
  end

  defp reachable_in(_, _, visited, _, 0), do: visited

  defp reachable_in(map, fnum, visited, recently_added, steps_remaining) do
    # DEBUG
    IO.puts("")
    # DEBUG
    steps_remaining |> IO.inspect(label: "steps_remaining")
    # DEBUG
    recently_added |> IO.inspect(label: "recently_added")
    print_map(map, visited, 30, 30)

    new_places_to_visit =
      recently_added
      |> Enum.flat_map(fn {x, y} -> [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}] end)
      |> Enum.reduce(%{}, fn loc, m -> Map.put(m, loc, true) end)
      |> Map.keys()
      |> Enum.filter(fn {x, y} -> x >= 0 && y >= 0 end)

    new_map =
      new_places_to_visit
      |> Enum.reduce(map, fn loc, m -> expand_map(m, fnum, loc) end)

    newly_added =
      new_places_to_visit
      |> Enum.filter(fn loc -> !wall?(new_map, loc) end)

    new_visited =
      newly_added
      |> Enum.reduce(visited, fn loc, m -> Map.put(m, loc, true) end)

    reachable_in(new_map, fnum, Map.merge(visited, new_visited), newly_added, steps_remaining - 1)
  end

  # ================ helpers ================

  defp expand_map(map, _, {x, y}) when x < 0 or y < 0, do: map

  defp expand_map(map, fnum, {x, y} = loc) do
    curr = Map.get(map, loc, @too_far)

    if curr == @too_far do
      # simplify?
      a = x * x + 3 * x + 2 * x * y + y + y * y
      a = a + fnum
      b = Integer.is_odd(num_bits(a))
      Map.put(map, loc, b)
    else
      map
    end
  end

  defp wall?(_, {x, y}) when x < 0 or y < 0, do: true
  defp wall?(map, loc), do: Map.get(map, loc)

  defp num_bits(a), do: do_num_bits(a, 0)

  defp do_num_bits(0, n), do: n

  defp do_num_bits(a, n) when Integer.is_even(a) do
    do_num_bits(bsr(a, 1), n)
  end

  defp do_num_bits(a, n) do
    do_num_bits(bsr(a, 1), n + 1)
  end

  defp taxi_dist({x0, y0}, {x1, y1}) do
    abs(x0 - x1) + abs(y0 - y1)
  end

  defp print_map(map, visited = %{}, max_x, max_y) do
    for y <- 0..max_y do
      0..max_x
      |> Enum.map(fn x ->
        cond do
          wall?(map, {x, y}) -> "#"
          Map.get(visited, {x, y}) -> "O"
          true -> "."
        end
      end)
      |> Enum.join("")
      |> IO.puts()
    end
  end
end

# Y2016.Day13.run1(10, 7, 4) # test
# # => 11
# Y2016.Day13.run1
# # => 90

# Y2016.Day13.run2
# # => 135
