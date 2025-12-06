# JSAbacusFramework.io

defmodule Y2015.Day12 do
  # sum
  def part1(_ctx, lines) do
    sum(lines, fn _ -> true end)
  end

  # color filter
  def part2(_ctx, lines) do
    sum(lines, fn vals -> !Enum.member?(vals, "red") end)
  end

  defp sum(lines, filter) do
    lines
    |> collect_numbers(filter)
    |> Enum.sum()
  end

  defp collect_numbers(_s, _filter) do
    # s
    # |> Poison.decode!()
    # |> filtered_numbers(filter)
    # |> List.flatten()
    []
  end

  # defp filtered_numbers(m, f) when is_map(m) do
  #   vals = Map.values(m)

  #   if f.(vals) do
  #     vals |> Enum.map(&filtered_numbers(&1, f))
  #   else
  #     []
  #   end
  # end

  # defp filtered_numbers(l, f) when is_list(l) do
  #   l |> Enum.map(&filtered_numbers(&1, f))
  # end

  # defp filtered_numbers(val, _) when is_integer(val), do: val
  # defp filtered_numbers(_, _), do: []
end
