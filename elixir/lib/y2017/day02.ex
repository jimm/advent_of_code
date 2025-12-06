# Corruption Checksum

defmodule Y2017.Day02 do
  alias Common.Set

  def part1(_ctx, lines) do
    lines
    |> Enum.map(&minmax_diff_line/1)
    |> Enum.sum()
  end

  def part2(_ctx, lines) do
    lines
    |> Enum.map(&find_integer_divisor_line/1)
    |> Enum.sum()
  end

  defp minmax_diff_line(line) do
    line
    |> line_nums
    |> minmax_diff
  end

  defp minmax_diff(nums) do
    {min, max} = Enum.min_max(nums)
    max - min
  end

  defp find_integer_divisor_line(line) do
    line
    |> line_nums
    |> find_integer_divisor
  end

  def find_integer_divisor(nums) do
    pair =
      nums
      |> Set.combinations(2)
      |> Enum.find(fn pair ->
        {min, max} = Enum.min_max(pair)
        rem(max, min) == 0
      end)

    {min, max} = Enum.min_max(pair)
    div(max, min)
  end

  defp line_nums(line) do
    line
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end
end
