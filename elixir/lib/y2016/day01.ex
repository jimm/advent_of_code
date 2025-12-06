defmodule Y2016.Day01 do
  def part1(_ctx, lines) do
    read_dirs(lines)
    |> follow_directions(:north, 0, 0)
    |> grid_distance
  end

  def run2(_ctx, lines) do
    read_dirs(lines)
    |> first_loc_visited_twice(:north, {0, 0}, [])
    |> grid_distance
  end

  defp read_dirs(lines) do
    lines
    |> hd
    |> String.split(", ")
  end

  defp grid_distance({x, y}), do: abs(x) + abs(y)

  # ================

  defp follow_directions([], _, x, y), do: {x, y}

  defp follow_directions([h | t], dir, x, y) do
    {dir, x, y} = move(h, dir, x, y)
    follow_directions(t, dir, x, y)
  end

  defp move(s, dir, x, y) do
    dir_change = String.at(s, 0)
    dist = s |> String.slice(1..-1//-1) |> String.to_integer()
    add(dir_change, dir, dist, x, y)
  end

  defp add("R", :north, dist, x, y), do: {:east, x + dist, y}
  defp add("R", :east, dist, x, y), do: {:south, x, y - dist}
  defp add("R", :south, dist, x, y), do: {:west, x - dist, y}
  defp add("R", :west, dist, x, y), do: {:north, x, y + dist}
  defp add("L", :north, dist, x, y), do: {:west, x - dist, y}
  defp add("L", :east, dist, x, y), do: {:north, x, y + dist}
  defp add("L", :south, dist, x, y), do: {:east, x + dist, y}
  defp add("L", :west, dist, x, y), do: {:south, x, y - dist}

  # ================

  defp first_loc_visited_twice([], _, loc, _), do: loc

  defp first_loc_visited_twice([h | t], dir, {x, y}, prev_locs) do
    new_dir = h |> String.at(0) |> new_dir(dir)
    dist = h |> String.slice(1..-1//-1) |> String.to_integer()
    locs = move_one_at_a_time(new_dir, dist, {x, y}, [])
    new_xy = hd(locs)

    new_locs = locs ++ prev_locs

    case find_dupe(new_locs, []) do
      nil -> first_loc_visited_twice(t, new_dir, new_xy, new_locs)
      loc -> loc
    end
  end

  # Inefficient (O(n^2) because of list traversal), could use a map.
  defp find_dupe([], _), do: nil

  defp find_dupe([loc | t], seen_locs) do
    if Enum.member?(seen_locs, loc) do
      loc
    else
      find_dupe(t, [loc | seen_locs])
    end
  end

  defp move_one_at_a_time(_, 0, _, prev_locs), do: prev_locs

  defp move_one_at_a_time(:north, dist, {x, y}, prev_locs) do
    loc = {x, y + 1}
    move_one_at_a_time(:north, dist - 1, loc, [loc | prev_locs])
  end

  defp move_one_at_a_time(:east, dist, {x, y}, prev_locs) do
    loc = {x + 1, y}
    move_one_at_a_time(:east, dist - 1, loc, [loc | prev_locs])
  end

  defp move_one_at_a_time(:south, dist, {x, y}, prev_locs) do
    loc = {x, y - 1}
    move_one_at_a_time(:south, dist - 1, loc, [loc | prev_locs])
  end

  defp move_one_at_a_time(:west, dist, {x, y}, prev_locs) do
    loc = {x - 1, y}
    move_one_at_a_time(:west, dist - 1, loc, [loc | prev_locs])
  end

  defp new_dir("R", :north), do: :east
  defp new_dir("R", :east), do: :south
  defp new_dir("R", :south), do: :west
  defp new_dir("R", :west), do: :north
  defp new_dir("L", :north), do: :west
  defp new_dir("L", :east), do: :north
  defp new_dir("L", :south), do: :east
  defp new_dir("L", :west), do: :south
end

# Y2016.Day01.run1
# Y2016.Day01.run2
