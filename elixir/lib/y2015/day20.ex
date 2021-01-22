# Infinite Elves and Infinite Houses

defmodule Y2015.Day20 do
  use Common.File

  @puzzle_input 36000000

  def run1 do
    # sum of factors of house number >= @puzzle_input / 10
    num_houses_below = Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&factors/1)
    |> Stream.map(&Enum.sum/1)
    |> Stream.take_while(&(&1 < @puzzle_input / 10))
    |> Enum.count()
    num_houses_below + 1
  end

  def run2 do
    num_houses_below = Stream.iterate(1, &(&1 + 1))
    |> Stream.map(&factors/1)
    |> Stream.map(&num_presents_max_50_visits_11_each/1)
    |> Stream.take_while(&(&1 < @puzzle_input))
    |> Enum.count()
    num_houses_below + 1
  end

  defp factors(1) do
    [1]
  end
  defp factors(n) do
    sqrt_n = round(:math.sqrt(n))
    factors(n, [1, n], 2, sqrt_n)
  end
  defp factors(_n, factors, i, sqrt_n) when i > sqrt_n do
    factors
  end
  defp factors(n, factors, i, sqrt_n) do
    if rem(n, i) == 0 do
      other = round(n / i)
      new_factors = if i == other do
        [i | factors]
      else
        [other | [i | factors]]
      end
      factors(n, new_factors, i+1, sqrt_n)
    else
      factors(n, factors, i+1, sqrt_n)
    end
  end

  defp num_presents_max_50_visits_11_each(factors) do
    n = Enum.max(factors)
    factors
    |> Enum.map(fn fac ->
      if (fac * 50) >= n, do: (fac * 11), else: 0
    end)
    |> Enum.sum
  end
end
