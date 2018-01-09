defmodule Y2016.Day15 do
  use Common.File

  @input_file default_input_path()
  @input_regex ~r{Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)\.}

  def run1(file \\ @input_file) do
    run(file, [])
  end

  def run2(file \\ @input_file) do
    run(file, [{11, 0}])
  end

  def run(file, additional_discs) do
    discs = read_file(file) ++ additional_discs

    Stream.iterate(0, &(&1 + 1))
    |> Stream.drop_while(&(!lines_up(discs, &1)))
    |> Enum.take(1)
    |> hd
  end

  # Assumes discs are in order in the file, starting at number 1.
  # Returns a list of {num_positions, start_positions} tuples.
  defp read_file(file) do
    input_lines(file)
    |> Enum.map(fn line ->
      [_, num_pos, start_pos] = Regex.run(@input_regex, line)
      {String.to_integer(num_pos), String.to_integer(start_pos)}
    end)
  end

  # Could be more efficient and rewrite this to stop at the first sign of a
  # different position.
  defp lines_up(discs, start_time) do
    positions = positions_at(discs, start_time + 1, [])
    first_pos = hd(positions)
    Enum.all?(positions, &(&1 == first_pos))
  end

  defp positions_at([], _, positions), do: positions

  defp positions_at([{n, start} | rest], t, positions) do
    pos = Integer.mod(start + t, n)
    positions_at(rest, t + 1, [pos | positions])
  end
end

# Y2016.Day15.run1
# # => 16824

# Y2016.Day15.run2
# # => 3543984
