defmodule Y2015.Day09 do
  use Common.File

  # shortest
  def run1, do: run(&Enum.min/1)

  # longest
  def run2, do: run(&Enum.max/1)

  defp run(f) do
    distances =
      default_input_path()
      |> File.stream!()
      |> Enum.reduce(%{}, fn line, acc ->
        [src, "to", dest, "=", num_str] = String.split(line)
        dist = String.to_integer(num_str)

        acc
        |> add_distance(src, dest, dist)
        |> add_distance(dest, src, dist)
      end)

    f.(path_lengths(distances, permutations(Map.keys(distances)), []))
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
    |> Enum.map(fn [src, dest] -> distances[src][dest] end)
    |> Enum.sum()
  end
end
