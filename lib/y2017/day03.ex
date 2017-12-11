# Spiral Memory

# 37 36 35 34 33 32 31
# 38 17 16 15 14 13 30
# 39 18  5  4  3 12 29
# 40 19  6  1  2 11 28
# 41 20  7  8  9 10 27
# 42 21 22 23 24 25 26
# 43 44 45 46 47 48 49

# Part 2:
#
# As a stress test on the system, the programs here clear the grid and then
# store the value 1 in square 1. Then, in the same allocation order as shown
# above, they store the sum of the values in all adjacent squares, including
# diagonals.
#
# So, the first few squares' values are chosen as follows:
#
# - Square 1 starts with the value 1.
# - Square 2 has only one adjacent filled square (with value 1), so it also
#   stores 1.
# - Square 3 has both of the above squares as neighbors and stores the sum
#   of their values, 2.
# - Square 4 has all three of the aforementioned squares as neighbors and
#   stores the sum of their values, 4.
# - Square 5 only has the first and fourth squares as neighbors, so it gets
#   the value 5.
# - Once a square is written, its value does not change. Therefore, the
#   first few squares would receive the following values:
#
# 147  142  133  122   59
# 304    5    4    2   57
# 330   10    1    1   54
# 351   11   23   25   26
# 362  747  806--->   ...
#
# What is the first value written that is larger than your puzzle input?


defmodule Y2017.Day03 do
  import Bitwise
  alias Common.File, as: CF

  @start_location 368078

  def part1 do
    manhattan_distance_to_start()
  end

  def part2 do
  end

  # ================ part 1 ================

  defp manhattan_distance_to_start, do: manhattan_distance_to(@start_location)

  defp manhattan_distance_to(i) do
    sqrt = sqrt_of_bottom_corner_containing(i)
    bot_right = sqrt * sqrt
    if bot_right == i do
      sqrt - 1
    else
      mc = max_coord(sqrt)
      {x, y} = find_coord(i, bot_right, sqrt, mc)
      abs(x) + abs(y)
    end
  end

  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt-1) do
    # bottom row
    x = mc - (bot_right - i)
    y = -mc
    {x, y}
  end
  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt-1) * 2 do
    # left row
    x = -mc
    y = -max_coord(sqrt) + ((bot_right - (sqrt-1)) - i)
    {x, y}
  end
  defp find_coord(i, bot_right, sqrt, mc) when i >= bot_right - (sqrt-1) * 3 do
    # top row
    x = -mc + ((bot_right - (sqrt-1) * 2) - i)
    y = mc
    {x, y}
  end
  defp find_coord(i, bot_right, sqrt, mc) do
    # right row
    x = mc
    y = mc - ((bot_right - (sqrt-1) * 3) - i)
    {x, y}
  end

  defp max_coord(sqrt), do: div(sqrt, 2)

  defp sqrt_of_bottom_corner_containing(i) do
    isqrt = trunc(:math.sqrt(i))
    if band(isqrt, 1) == 1 do
      if isqrt * isqrt == i, do: isqrt, else: isqrt+2
    else
      isqrt+1
    end
  end
end
