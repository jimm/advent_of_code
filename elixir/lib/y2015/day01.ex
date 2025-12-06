# Not Quite Lisp

defmodule Y2015.Day01 do
  # destination floor
  def part1(_ctx, lines) do
    lines
    |> hd
    |> String.split("", trim: true)
    |> Enum.reduce(0, fn c, acc -> acc + move(c) end)
  end

  # first basement index
  def part2(_ctx, lines) do
    lines
    |> hd
    |> String.codepoints()
    |> first_negative_one(0, 0)
  end

  defp move("("), do: 1
  defp move(")"), do: -1

  defp first_negative_one(_, -1, index), do: index
  defp first_negative_one([], _, _), do: raise("not found")

  defp first_negative_one([c | rest], floor, index) when c == "(" do
    first_negative_one(rest, floor + 1, index + 1)
  end

  defp first_negative_one([_ | rest], floor, index) do
    first_negative_one(rest, floor - 1, index + 1)
  end
end
