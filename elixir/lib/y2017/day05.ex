# A Maze of Twisty Trampolines, All Alike

defmodule Y2017.Day05 do
  use Common.File

  def part1 do
    steps_until_out_of_bounds(fn offset -> offset + 1 end)
  end

  def part2 do
    steps_until_out_of_bounds(fn offset ->
      if offset >= 3, do: offset - 1, else: offset + 1
    end)
  end

  # ================ helpers ================

  defp steps_until_out_of_bounds(f) do
    indices = Stream.iterate(0, &(&1 + 1))

    nums =
      input_lines()
      |> Enum.map(&String.to_integer/1)

    m = Map.new(Enum.zip(indices, nums))
    do_steps_until_out_of_bounds(m, length(nums), 0, 0, f)
  end

  defp do_steps_until_out_of_bounds(_, len, idx, steps, _)
       when idx < 0 or idx >= len do
    steps
  end

  defp do_steps_until_out_of_bounds(m, len, idx, steps, f) do
    offset = m[idx]
    new_idx = idx + offset
    new_m = Map.put(m, idx, Map.get(m, f.(offset)))
    do_steps_until_out_of_bounds(new_m, len, new_idx, steps + 1, f)
  end
end
