defmodule Y2016.Day08 do
  @rows 6
  @cols 50
  @off " "
  @on "#"

  require Common.File, as: CF

  def run1(file \\ nil) do
    generate_display(file)
    |> count_on_pixels
  end

  def run2(file \\ nil) do
    generate_display(file)
    |> print_display
    nil
  end

  defp generate_display(file) do
    lines = (file || CF.default_input_path(__MODULE__)) |> CF.lines
    lines
    |> Enum.reduce(init_display(), fn(command, display) ->
      run_command(command, display)
    end)
  end

  defp init_display do
    List.duplicate(@off, @cols) |> List.duplicate(@rows)
  end

  defp run_command(command, display) do
    do_run_command(String.split(command), display)
  end

  defp do_run_command(["rect", dim], display) do
    [width, height] = dim |> String.split("x") |> Enum.map(&String.to_integer/1)
    set_rect(display, width, height)
  end
  defp do_run_command(["rotate", "column", xstr, "by", nstr], display) do
    x = num_from_equals(xstr)
    n = String.to_integer(nstr)
    rotate_column(display, x, n)
  end
  defp do_run_command(["rotate", "row", ystr, "by", nstr], display) do
    y = num_from_equals(ystr)
    n = String.to_integer(nstr)
    rotate_row(display, y, n)
  end

  defp set_rect(display, width, height) do
    display
    |> Enum.with_index
    |> Enum.map(fn {row, row_index} ->
      if row_index < height do
        List.duplicate(@on, width) ++ Enum.drop(row, width)
      else
        row
      end
    end)
  end

  defp rotate_row(display, y, n) do
    display
    |> Enum.with_index
    |> Enum.map(fn {row, row_index} ->
      if row_index == y do
        (Enum.drop(row, @cols - n) ++ Enum.take(row, @cols - n))
      else
        row
      end
    end)
  end

  defp rotate_column(display, x, n) do
    display
    |> Enum.with_index
    |> Enum.map(fn {row, row_index} ->
      from_row = row_index - n
      from_row = if from_row < 0, do: from_row + @rows, else: from_row
      Enum.take(row, x) ++ [at(display, x, from_row)] ++ Enum.drop(row, x+1)
    end)
  end

  defp at(display, x, y) do
    display |> Enum.at(y) |> Enum.at(x)
  end

  defp count_on_pixels(display) do
    display
    |> Enum.map(fn row -> row |> Enum.count(fn cell -> cell == @on end) end)
    |> Enum.sum
  end

  defp num_from_equals(str) do
    str |> String.split("=") |> tl |> hd |> String.to_integer
  end

  defp print_display(display) do
    IO.puts ""
    display |> Enum.each(fn row -> row |> Enum.join("") |> IO.puts end)
    display
  end
end

# Y2016.Day08.run1 # => 128
# Y2016.Day08.run2 # => "EOARGPHYAO"
