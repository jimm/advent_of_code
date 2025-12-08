# ================ Playground ================

defmodule Y2025.Day08 do
  import Common.Set

  def part1(ctx, lines) do
    coords = parse_3d(lines)

    coords_and_dists =
      coords
      |> pairs_and_distances_between
      |> Enum.sort_by(fn {_, _, dist} -> dist end)

    coords_and_dists
    |> create_circuits(if(ctx.test, do: 10, else: 1000), MapSet.new())
    |> Enum.sort_by(&MapSet.size/1, :desc)
    |> Enum.take(3)
    |> Enum.map(&MapSet.size/1)
    |> Enum.reduce(&(&1 * &2))
  end

  def part2(_ctx, lines) do
    coords = parse_3d(lines)

    coords_and_dists =
      coords
      |> pairs_and_distances_between
      |> Enum.sort_by(fn {_, _, dist} -> dist end)

    {{x0, _, _}, {x1, _, _}} =
      create_one_circuit(coords_and_dists, length(coords), MapSet.new())

    x0 * x1
  end

  # ================ helpers ================

  # Recursively connects coords and add to the proper circuits, creating and
  # merging circuits when necessary. Returns the resulting circuits after
  # `num_iters`.
  defp create_circuits(_, 0, circuits), do: MapSet.to_list(circuits)

  defp create_circuits([{c0, c1, _} | coords_and_dists], num_iters, circuits) do
    create_circuits(coords_and_dists, num_iters - 1, add_coords_to_circuits(c0, c1, circuits))
  end

  # Recursively connects coords and add to the proper circuits, creating and
  # merging circuits when necessary. When we finally create one circuit
  # containing all the coordinates, return the last two coords that made
  # that happen.
  defp create_one_circuit([{c0, c1, _} | coords_and_dists], num_coords, circuits) do
    circuits = add_coords_to_circuits(c0, c1, circuits)

    if MapSet.size(circuits) == 1 && MapSet.size(hd(MapSet.to_list(circuits))) == num_coords do
      {c0, c1}
    else
      create_one_circuit(coords_and_dists, num_coords, circuits)
    end
  end

  def add_coords_to_circuits(c0, c1, circuits) do
    c0_circuit = Enum.find(circuits, fn circuit -> MapSet.member?(circuit, c0) end)
    c1_circuit = Enum.find(circuits, fn circuit -> MapSet.member?(circuit, c1) end)

    case {c0_circuit, c1_circuit} do
      {nil, nil} ->
        circuits |> MapSet.put(MapSet.new([c0, c1]))

      {nil, c1c} ->
        c1c = MapSet.put(c1c, c0)
        circuits |> MapSet.put(c1c) |> MapSet.delete(c1_circuit)

      {c0c, nil} ->
        c0c = MapSet.put(c0c, c1)
        circuits |> MapSet.put(c0c) |> MapSet.delete(c0_circuit)

      {cc, cc} ->
        circuits

      {c0c, c1c} ->
        circuits
        |> MapSet.put(MapSet.union(c0c, c1c))
        |> MapSet.delete(c0_circuit)
        |> MapSet.delete(c1_circuit)
    end
  end

  defp pairs_and_distances_between(coords) do
    coords
    |> combinations(2)
    |> Enum.map(fn [c0, c1] -> {c0, c1, squared_dist(c0, c1)} end)
  end

  defp squared_dist({x0, y0, z0}, {x1, y1, z1}) do
    dx = abs(x1 - x0)
    dy = abs(y1 - y0)
    dz = abs(z1 - z0)
    dx * dx + dy * dy + dz * dz
  end

  # Returns a list of {x, y, z} tuples
  defp parse_3d(lines) do
    lines |> Enum.map(&line_to_coords_tuple/1)
  end

  defp line_to_coords_tuple(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end
end
