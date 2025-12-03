PUZZLENAME

defmodule YEARNAME.DAYNAME do
  use Common.File

  @tests [
    {"input", "expected_result"}
  ]

  def part1 do
    lines = input_lines(default_input_path(__MODULE__, 1))
  end

  def part2 do
    lines = input_lines(default_input_path(__MODULE__, 2))
  end

  # ================ testing ================

  def part1_test do
  end

  def part2_test do
  end

  # ================ helpers ================
end
