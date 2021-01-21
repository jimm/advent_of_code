# Infinite Elves and Infinite Houses

defmodule Y2015.Day20 do
  use Common.File

  @puzzle_input 36000000

  def run1 do
    # sum of factors of house number >= @puzzle_input / 10
    num_houses_below = Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&sum_of_factors/1)
    |> Stream.take_while(&(&1 < @puzzle_input / 10))
    |> Enum.count()
    num_houses_below + 1
  end

  # 776160 is too low
  def run2 do
    # sum of factors of house number >= @puzzle_input / 11
    num_houses_below = Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&sum_of_factors/1)
    |> Stream.take_while(&(&1 < @puzzle_input / 11))
    |> Enum.count()
    num_houses_below + 1
  end

  # Returns a Stream of tuples with house numbers and present counts.
  defp sum_of_factors(1) do
    1
  end
  defp sum_of_factors(n) do
    sqrt_n = round(:math.sqrt(n))
    sum_of_factors(n, [1, n], 2, sqrt_n)
  end
  defp sum_of_factors(_n, factors, i, sqrt_n) when i > sqrt_n do
    Enum.sum(factors)
  end
  defp sum_of_factors(n, factors, i, sqrt_n) do
    if rem(n, i) == 0 do
      other = n / i
      new_factors = if i == other do
        [i | factors]
      else
        [other | [i | factors]]
      end
      sum_of_factors(n, new_factors, i+1, sqrt_n)
    else
      sum_of_factors(n, factors, i+1, sqrt_n)
    end
  end
end
