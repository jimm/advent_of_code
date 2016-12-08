defmodule Y2015.Day12 do
  @input_file "data/y2015/day_12.txt"

  

  def sum do
    sum(fn _ -> true end)
  end

  def color_filter(color \\ "red") do
    sum(fn vals -> !Enum.member?(vals, color) end)
  end

  defp sum(filter) do
    File.read!(@input_file)
    |> collect_numbers(filter)
    |> Enum.sum
  end

  defp collect_numbers(s, filter) do
    {:ok, m} = HTTPoison.decode(s)
    m |> filtered_numbers(filter) |> List.flatten
  end

  defp filtered_numbers(m, f) when is_map(m) do
    vals = Map.values(m)
    if f.(vals) do
      vals |> Enum.map(&(filtered_numbers(&1, f)))
    else
      []
    end
  end
  defp filtered_numbers(l, f) when is_list(l) do
    l |> Enum.map(&(filtered_numbers(&1, f)))
  end
  defp filtered_numbers(val, _) when is_integer(val), do: val
  defp filtered_numbers(_, _), do: []
end

# Y2015.Day12.sum
# # => 119433

# Y2015.Day12.color_filter
# # => 68466
