defmodule Y2016.Day18 do
  alias Common.File, as: CF

  @safe "."
  @trap "^"
  @input_file CF.default_input_path(__MODULE__)
  @run1_rows 40

  def run(num_rows \\ @run1_rows, file \\ @input_file) do
    row =
      file
      |> CF.lines
      |> hd
      |> String.split("", trim: true)
    total_safe(row, num_rows-1, count_safe(row))
  end

  defp total_safe(_, 0, count), do: count
  defp total_safe(row, num_rows, count) do
    nr = next_row(row)
    total_safe(nr, num_rows-1, count + count_safe(nr))
  end

  defp count_safe(row) do
    count_safe(row, 0)
  end

  defp count_safe([], count), do: count
  defp count_safe([@safe|t], count), do: count_safe(t, count+1)
  defp count_safe([_|t], count), do: count_safe(t, count)

  defp next_row(row) do
    next_row([@safe|row] ++ [@safe], [])
  end

  defp next_row([], [_|[_|nr]]), do: Enum.reverse(nr)
  defp next_row([@trap|[@trap|[@safe|_]]=rest], nr) do
    next_row(rest, [@trap|nr])
  end
  defp next_row([@safe|[@trap|[@trap|_]]=rest], nr) do
    next_row(rest, [@trap|nr])
  end
  defp next_row([@trap|[@safe|[@safe|_]]=rest], nr) do
    next_row(rest, [@trap|nr])
  end
  defp next_row([@safe|[@safe|[@trap|_]]=rest], nr) do
    next_row(rest, [@trap|nr])
  end
  defp next_row([_|rest], nr) do
    next_row(rest, [@safe|nr])
  end
end

# Y2016.Day18.run
# #=> 1974

# Y2016.Day18.run(400_000)
# #=> 
