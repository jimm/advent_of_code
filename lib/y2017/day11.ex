# Hex Ed
# https://www.redblobgames.com/grids/hexagons/
# http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/

defmodule Y2017.Day11 do

  use Common.File

  def part1 do
    {dest, _} = travel()
    astar_distance(dest)
  end

  def part2 do
    {_, max_dist} = travel()
    max_dist
  end

  # Returns {destination, max_dist}
  defp travel do
    read_steps()
    |> Enum.reduce({{0, 0, 0}, 0}, fn(step, {{x, y, z}, max_dist}) ->
      loc = move({x, y, z}, step)
      dist = astar_distance(loc)
      new_max_dist = max(max_dist, dist)
      {loc, max(max_dist, dist)}
    end)
  end

  defp move({x, y, z}, :n ), do: {x,     y + 1, z - 1}
  defp move({x, y, z}, :s ), do: {x,     y - 1, z + 1}
  defp move({x, y, z}, :ne), do: {x + 1, y    , z - 1}
  defp move({x, y, z}, :nw), do: {x - 1, y + 1, z    }
  defp move({x, y, z}, :se), do: {x + 1, y - 1, z    }
  defp move({x, y, z}, :sw), do: {x - 1, y    , z + 1}

  # TODO cache answers so we don't recalculate them all the time, using an
  # Atom.
  defp astar_distance(dest) do
    astar_distance(dest, {0, 0, 0}, 0)
  end

  defp astar_distance(loc, loc, steps), do: steps
  defp astar_distance(loc, goal, steps) do
    next_loc = Enum.min_by(neighbors(loc), fn(x) -> cube_distance(x, goal) end)
    astar_distance(next_loc, goal, steps + 1)
  end

  defp neighbors({x, y, z}) do
    [{x,     y + 1, z - 1},
     {x,     y - 1, z + 1},
     {x + 1, y    , z - 1},
     {x - 1, y + 1, z    },
     {x + 1, y - 1, z    },
     {x - 1, y    , z + 1}]
  end

  defp cube_distance({ax, ay, az}, {bx, by, bz}) do
    (abs(ax - bx) + abs(ay - by) + abs(az - bz)) / 2
  end

  defp read_steps do
    input_lines()
    |> hd
    |> String.split(",")
    |> Enum.map(&String.to_atom/1)
  end
end
