# ================ Secret Entrance ================

defmodule Y2025.Day01 do
  use Common.File

  # @tests [
  #   {"input", "expected_result"}
  # ]

  def part1 do
    [_, num_zeroes] =
      Enum.reduce(input_lines(default_input_path(__MODULE__, 1)), [50, 0], fn line,
                                                                              [pos, num_zeroes] ->
        [pos, _] = move(pos, line)
        [pos, num_zeroes + if(pos == 0, do: 1, else: 0)]
      end)

    IO.puts(num_zeroes)
  end

  def part2 do
    [_, num_zeroes] =
      Enum.reduce(input_lines(default_input_path(__MODULE__, 1)), [50, 0], fn line,
                                                                              [pos, num_zeroes] ->
        [pos, num_zeroes_seen] = move(pos, line)
        [pos, num_zeroes + num_zeroes_seen]
      end)

    IO.puts(num_zeroes)
  end

  # ================ testing ================

  def part1_test do
  end

  def part2_test do
  end

  # ================ helpers ================

  defp move(pos, line) do
    direction = String.first(line)
    num = String.to_integer(String.slice(line, 1, String.length(line) - 1))

    delta =
      case direction do
        "R" -> 1
        "L" -> -1
        _ -> raise "bad direction"
      end

    [pos, num_zeroes] =
      Enum.reduce(Enum.to_list(1..num), [pos, 0], fn _, [pos, num_zeroes] ->
        pos = rem(pos + delta, 100) |> rem(100)
        [pos, if(pos == 0, do: num_zeroes + 1, else: num_zeroes)]
      end)

    [pos, num_zeroes]
  end
end
