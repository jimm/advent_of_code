# ================ Laboratories ================

defmodule Y2025.Day07 do
  @start "S"
  @splitter "^"

  def part1(_ctx, lines) do
    lines
    |> parse_2d
    |> count_splits_from_start
  end

  def part2(_ctx, lines) do
    lines
    |> parse_2d
    |> count_timelines
  end

  # ================ helpers ================

  defp parse_2d(lines) do
    Enum.map(lines, &String.codepoints/1)
  end

  defp count_splits_from_start(manifold) do
    beam_cols = [start_column(manifold)]
    count_splits_from_start(tl(manifold), beam_cols, 0)
  end

  defp count_splits_from_start([], _, total_splits), do: total_splits

  defp count_splits_from_start([row | manifold], beam_cols, total_splits) do
    {new_beam_cols, new_total_splits} =
      beam_cols
      |> Enum.reduce({%{}, total_splits}, fn beam_col, {cols_map, tsplits} ->
        if Enum.at(row, beam_col) == @splitter do
          {
            cols_map |> Map.put(beam_col - 1, true) |> Map.put(beam_col + 1, true),
            tsplits + 1
          }
        else
          {Map.put(cols_map, beam_col, true), tsplits}
        end
      end)

    count_splits_from_start(manifold, Map.keys(new_beam_cols), new_total_splits)
  end

  defp count_timelines(manifold) do
    # each entry is {column, number of beams that got us here}
    beam_cols = %{start_column(manifold) => 1}
    count_timelines(tl(manifold), beam_cols)
  end

  defp count_timelines([], beam_cols) do
    beam_cols |> Map.values() |> Enum.sum()
  end

  defp count_timelines([row | manifold], beam_cols) do
    new_beam_cols =
      beam_cols
      |> Enum.reduce(%{}, fn {col, num_beams}, new_cols ->
        if Enum.at(row, col) == @splitter do
          new_cols
          |> Map.put(col - 1, Map.get(new_cols, col - 1, 0) + num_beams)
          |> Map.put(col + 1, Map.get(new_cols, col + 1, 0) + num_beams)
        else
          Map.put(new_cols, col, Map.get(new_cols, col, 0) + num_beams)
        end
      end)

    count_timelines(manifold, new_beam_cols)
  end

  # I'm going to assume that @start is in the first row
  defp start_column(manifold), do: Enum.find_index(hd(manifold), &(&1 == @start))
end
