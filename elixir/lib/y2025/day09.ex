# ================ Movie Theater ================

defmodule Y2025.Day09 do
  alias Common.Enum, as: CE

  def part1(_ctx, lines) do
    lines
    |> Enum.map(&parse_2d/1)
    |> CE.combinations(2)
    |> Enum.map(fn [p0, p1] -> p2_area(p0, p1) end)
    |> Enum.max()
  end

  # A rectangle is a min/max tuple of {x_min, y_min, x_max, y_max}.
  # An edge is a tuple of two points {{x0, y0}, {x1, y1}}.
  def part2(_ctx, lines) do
    red_tiles = lines |> Enum.map(&parse_2d/1)

    edges =
      [List.last(red_tiles) | red_tiles]
      |> CE.window(2)
      |> Enum.map(fn [{x0, y0}, {x1, y1}] ->
        {x_min, x_max} = Enum.min_max([x0, x1])
        {y_min, y_max} = Enum.min_max([y0, y1])
        {{x_min, y_min}, {x_max, y_max}}
      end)

    red_tiles
    |> CE.combinations(2)
    |> Enum.map(fn [p1, p2] -> rect_from(p1, p2) end)
    |> Enum.reduce(0, fn rect, max_area ->
      area = rect_area(rect)
      if area > max_area and not crosses_any_edge?(rect, edges), do: area, else: max_area
    end)
  end

  # ================ helpers ================

  # Returns the area of the rect under two corners.
  defp p2_area({x0, y0}, {x1, y1}) do
    (abs(x1 - x0) + 1) * (abs(y1 - y0) + 1)
  end

  # Given two corners of a rectangle, return the area.
  defp rect_area({x_min, y_min, x_max, y_max}) do
    (x_max - x_min + 1) * (y_max - y_min + 1)
  end

  # Returns a tuple of {x_min, y_min, x_max, y_max}.
  defp rect_from({x0, y0}, {x1, y1}) do
    [x_min, x_max] = Enum.sort([x0, x1])
    [y_min, y_max] = Enum.sort([y0, y1])
    {x_min, y_min, x_max, y_max}
  end

  defp crosses_any_edge?({x_min, y_min, x_max, y_max}, edges) do
    edges
    |> Enum.any?(fn {{e_x0, e_y0}, {e_x1, e_y1}} ->
      {e_x_min, e_x_max} = Enum.min_max([e_x0, e_x1])
      {e_y_min, e_y_max} = Enum.min_max([e_y0, e_y1])
      x_min < e_x_max && x_max > e_x_min && y_min < e_y_max && y_max > e_y_min
    end)
  end

  # Returns an {x, y} tuple.
  defp parse_2d(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end
end
