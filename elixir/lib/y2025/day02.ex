# ================ Gift Shop ================

defmodule Y2025.Day02 do
  use Common.File

  # @tests [
  #   {"input", "expected_result"}
  # ]

  def part1 do
    lines = input_lines(default_input_path(__MODULE__, 1))

    ranges =
      String.split(lines, ",")
      |> Enum.map(fn str ->
        [a, b] = String.split(str, "-")
        Enum.to_list(String.to_integer(a)..String.to_ingeter(b))
      end)
    sum_invalid = 0
    ranges
    |> Enum.filter(&_is_invalid/1)
    |> length
  end

  def part2 do
    lines = input_lines(default_input_path(__MODULE__, 1))
  end

  # ================ testing ================

  def test_part1 do
  end

  def test_part2 do
  end

  # ================ helpers ================
  defp _is_invalid(i):
  s = inspect(i)
  l = String.length(s)
  half = div(len(s) / 2)        # wrong
end
