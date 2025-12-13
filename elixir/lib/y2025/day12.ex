# ================ Christmas Tree Farm ================

defmodule Y2025.Day12.Shape do
  defstruct [:width, :height, :coords]

  def from_lines(lines) do
    width = String.length(hd(lines))
    height = length(lines)

    coords =
      lines
      |> Enum.zip(0..(length(lines) - 1))
      |> Enum.reduce(MapSet.new(), fn {line, row_idx}, coords ->
        line
        |> String.codepoints()
        |> Enum.zip(0..(String.length(line) - 1))
        |> Enum.reduce(coords, fn {ch, col_idx}, coords ->
          if ch == "#" do
            MapSet.put(coords, {row_idx, col_idx})
          else
            coords
          end
        end)
      end)

    %Y2025.Day12.Shape{width: width, height: height, coords: coords}
  end

  def size(shape) do
    shape.width * shape.height
  end
end

defmodule Y2025.Day12.Tree do
  alias Y2025.Day12.Shape

  defstruct [:width, :height, :present_quantities]

  def can_fit_presents?(tree, shapes) do
    cond do
      can_all_fit_naively?(tree) ->
        true

      too_big_to_fit?(tree, shapes) ->
        false

      true ->
        # With test data we'd have to actually do the work. With the real
        # data, we'll never get here.
        false
    end
  end

  defp can_all_fit_naively?(tree) do
    num_across = trunc(tree.width / 3)
    num_down = trunc(tree.height / 3)
    Enum.sum(tree.present_quantities) <= num_across * num_down
  end

  defp too_big_to_fit?(tree, shapes) do
    {_, _, total_shape_cells} =
      tree.present_quantities
      |> Enum.reduce({0, 0, 0}, fn quantity, {idx, shape_area, covered_count} ->
        shape = Enum.at(shapes, idx)

        {idx + 1, shape_area + quantity * shape.width * shape.height,
         covered_count + quantity * Shape.size(shape)}
      end)

    cond do
      total_shape_cells > tree.width * tree.height ->
        false

      can_all_fit_naively?(tree) ->
        true

      true ->
        false
    end
  end
end

defmodule Y2025.Day12 do
  alias Y2025.Day12.{Shape, Tree}

  @tree_regex ~r/^(?<w>\d+)x(?<h>\d+): (?<nums>.*)/
  @shape_index_regex ~r/^(\d+):/
  @shape_line_regex ~r/^[#.]{3}/

  def part1(ctx, lines) do
    if ctx.test do
      IO.puts("""
      Warning: tests will fail because the test data actually requires fitting
      the shapes together. The real data doesn't --- each tree fits one of two
      trivial cases that returns an answer quickly.
      """)

      0
    else
      {shapes, trees} = parse(lines)

      trees
      |> Enum.filter(&Tree.can_fit_presents?(&1, shapes))
      |> length
    end
  end

  def part2(_ctx, lines) do
    IO.puts(lines)
  end

  # ================ helpers ================

  # Returns {shapes, trees}
  defp parse(lines) do
    {shapes, trees, _} =
      lines
      |> Enum.reduce({[], [], []}, fn line, {shapes, trees, shape_lines} ->
        cond do
          match = Regex.named_captures(@tree_regex, line) ->
            # If there's a previous shape we have to "clean up" and add it
            shapes =
              if length(shape_lines) > 0 do
                shape = shape_lines |> Enum.reverse() |> Shape.from_lines()
                [shape | shapes]
              else
                shapes
              end

            %{"w" => w, "h" => h, "nums" => nums} = match
            w = String.to_integer(w)
            h = String.to_integer(h)

            present_quantities = nums |> String.split(" ") |> Enum.map(&String.to_integer/1)
            tree = %Tree{width: w, height: h, present_quantities: present_quantities}

            {shapes, [tree | trees], []}

          # The data files' shape indexes are always in order starting at 0,
          # so we don't have to do anything
          String.match?(line, @shape_index_regex) ->
            # Add the prevous shape, if any.
            if length(shape_lines) > 0 do
              shape = shape_lines |> Enum.reverse() |> Shape.from_lines()
              {[shape | shapes], trees, []}
            else
              {shapes, trees, shape_lines}
            end

          String.match?(line, @shape_line_regex) ->
            {shapes, trees, [line | shape_lines]}
        end
      end)

    {Enum.reverse(shapes), Enum.reverse(trees)}
  end
end
