# Inverse Captcha

defmodule Y2017.Day01 do
  alias Common.File, as: CF

  def part1 do
    digits = puzzle_digits()
    wrapped = digits ++ [hd(digits)]
    sum_next_matchers(wrapped, 0)
  end

  def part2 do
    digits = puzzle_digits()
    half_len = div(length(digits), 2)
    sum_mid_matchers(Enum.take(digits, half_len), Enum.drop(digits, half_len), 0)
  end

  defp puzzle_digits do
    __MODULE__
    |> CF.default_input_path
    |> CF.lines
    |> hd
    |> String.to_integer
    |> Integer.digits
  end

  defp sum_next_matchers([_], sum) do
    sum
  end
  defp sum_next_matchers([h | [h | t]], sum) do
    sum_next_matchers([h|t], sum + h)
  end
  defp sum_next_matchers([_ | t], sum) do
    sum_next_matchers(t, sum)
  end

  defp sum_mid_matchers([], [], sum) do
    sum * 2
  end
  defp sum_mid_matchers([h | t1], [h | t2], sum) do
    sum_mid_matchers(t1, t2, sum + h)
  end
  defp sum_mid_matchers([_ | t1], [_ | t2], sum) do
    sum_mid_matchers(t1, t2, sum)
  end
end
