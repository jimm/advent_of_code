# Spiral Memory

defmodule Y2017.Day03 do
  import Bitwise

  @start_location 368_078

  def part1(_ctx, _lines) do
    manhattan_distance_to_start()
  end

  def part2(_ctx, _lines) do
    stress_test_value(2, Map.put(%{}, {0, 0}, 1))
  end

  # ================ part 1 ================

  defp manhattan_distance_to_start, do: manhattan_distance_to(@start_location)

  defp manhattan_distance_to(i) do
    {x, y} = coord_of(i)
    abs(x) + abs(y)
  end

  # ================ part 2 ================

  defp stress_test_value(i, state) do
    coord = coord_of(i)
    val = sum_of_surounding(coord, state)

    if val > @start_location do
      val
    else
      new_state = Map.put(state, coord, val)
      stress_test_value(i + 1, new_state)
    end
  end

  defp sum_of_surounding({x, y}, state) do
    at(x - 1, y + 1, state) + at(x, y + 1, state) + at(x + 1, y + 1, state) + at(x - 1, y, state) +
      at(x + 1, y, state) + at(x - 1, y - 1, state) + at(x, y - 1, state) +
      at(x + 1, y - 1, state)
  end

  defp at(x, y, state) do
    Map.get(state, {x, y}, 0)
  end

  # ================ helpers ================

  defp sqrt_of_bottom_corner_containing(i) do
    isqrt = trunc(:math.sqrt(i))

    if band(isqrt, 1) == 1 do
      if isqrt * isqrt == i, do: isqrt, else: isqrt + 2
    else
      isqrt + 1
    end
  end

  defp coord_of(i) do
    sqrt = sqrt_of_bottom_corner_containing(i)
    bot_right = sqrt * sqrt
    find_coord(i, bot_right, sqrt, max_coord(sqrt))
  end

  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt - 1) do
    # bottom row
    x = mc - (bot_right - i)
    y = -mc
    {x, y}
  end

  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt - 1) * 2 do
    # left row
    x = -mc
    y = -max_coord(sqrt) + (bot_right - (sqrt - 1) - i)
    {x, y}
  end

  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt - 1) * 3 do
    # top row
    x = -mc + (bot_right - (sqrt - 1) * 2 - i)
    y = mc
    {x, y}
  end

  defp find_coord(i, bot_right, sqrt, mc) do
    # right row
    x = mc
    y = mc - (bot_right - (sqrt - 1) * 3 - i)
    {x, y}
  end

  defp max_coord(sqrt), do: div(sqrt, 2)
end
