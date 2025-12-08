# ================ Secret Entrance ================

defmodule Y2025.Day01 do
  def part1(_ctx, lines) do
    [_, num_zeroes] =
      Enum.reduce(lines, [50, 0], fn line, [pos, num_zeroes] ->
        [pos, _] = move(pos, line)
        [pos, num_zeroes + if(pos == 0, do: 1, else: 0)]
      end)

    num_zeroes
  end

  def part2(_ctx, lines) do
    [_, num_zeroes] =
      Enum.reduce(lines, [50, 0], fn line, [pos, num_zeroes] ->
        [pos, num_zeroes_seen] = move(pos, line)
        [pos, num_zeroes + num_zeroes_seen]
      end)

    num_zeroes
  end

  # ================ helpers ================

  defp move(pos, line) do
    direction = String.first(line)
    num = String.to_integer(String.slice(line, 1, String.length(line) - 1))

    delta =
      case direction do
        "R" -> 1
        "L" -> -1
      end

    [pos, num_zeroes] =
      Enum.reduce(Enum.to_list(1..num), [pos, 0], fn _, [pos, num_zeroes] ->
        pos = rem(pos + delta, 100) |> rem(100)
        [pos, if(pos == 0, do: num_zeroes + 1, else: num_zeroes)]
      end)

    [pos, num_zeroes]
  end
end
