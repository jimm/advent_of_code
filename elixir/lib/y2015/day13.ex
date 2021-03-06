# Knights of the Dinner Table

defmodule Y2015.Day13 do
  use Common.File

  @input_file default_input_path()
  @parse_regex ~r{(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\.}

  # happiest
  def run1 do
    File.stream!(@input_file)
    |> Enum.reduce(%{}, fn line, acc ->
      [src, dest, dist] = parse(line)
      acc |> add_distance(src, dest, dist)
    end)
    |> max_distance
  end

  # happiest including me
  def run2 do
    File.stream!(@input_file)
    |> Enum.reduce(%{}, fn line, acc ->
      [src, dest, dist] = parse(line)

      acc
      |> add_distance(src, dest, dist)
      |> add_distance(src, "me", 0)
      |> add_distance("me", src, 0)
    end)
    |> max_distance
  end

  defp max_distance(distances) do
    circular_permutations =
      Map.keys(distances)
      |> permutations
      |> Enum.map(fn perm -> [List.last(perm) | perm] end)

    Enum.max(path_lengths(distances, circular_permutations, []))
  end

  defp parse(line) do
    [_, name1, gain_or_lose, num_str, name2] = Regex.run(@parse_regex, line)
    dist = String.to_integer(num_str)
    dist = if gain_or_lose == "lose", do: -dist, else: dist
    [name1, name2, dist]
  end

  defp add_distance(m, src, dest, dist) do
    distances = Map.get(m, src, %{})
    Map.put(m, src, Map.put(distances, dest, dist))
  end

  defp permutations([]), do: []
  defp permutations([x]), do: [x]
  defp permutations([x, y]), do: [[x, y], [y, x]]

  defp permutations(xs) do
    for x <- xs,
        y <- permutations(List.delete(xs, x)) do
      [x | y]
    end
  end

  defp path_lengths(_, [], lengths), do: lengths

  defp path_lengths(distances, [path | paths], lengths) do
    path_lengths(distances, paths, [path_length(distances, path) | lengths])
  end

  defp path_length(distances, path) do
    path
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [src, dest] -> distances[src][dest] + distances[dest][src] end)
    |> Enum.sum()
  end
end
