# ================ Gift Shop ================

defmodule Y2025.Day02 do
  require Integer
  use Common.File

  # @tests [
  #   {"input", "expected_result"}
  # ]

  def part1 do
    lines = input_lines(default_input_path(__MODULE__, 1))
    part_common(hd(lines), &invalid1/1)
  end

  def part2 do
    lines = input_lines(default_input_path(__MODULE__, 1))
    part_common(hd(lines), &invalid2/1)
  end

  # ================ testing ================

  def part1_test do
    part1()
  end

  def part2_test do
    part2()
  end

  # ================ helpers ================

  defp part_common(line, invalid_func) do
    ranges =
      String.split(line, ",")
      |> Enum.map(fn str ->
        [a, b] = String.split(str, "-")
        Enum.to_list(String.to_integer(a)..String.to_integer(b))
      end)

    invalid_ids =
      for range <- ranges,
          id <- range do
        if invalid_func.(id), do: id, else: 0
      end

    invalid_ids
    |> List.flatten()
    |> Enum.reduce(0, &+/2)
  end

  defp invalid1(i) do
    s = inspect(i)
    l = String.length(s)

    if Integer.is_odd(l) do
      false
    else
      half = div(l, 2)
      String.slice(s, 0..(half - 1)) == String.slice(s, half..(l - 1))
    end
  end

  defp invalid2(i) do
    s = inspect(i)
    l = String.length(s)
    half = div(l, 2)

    1..half
    |> Enum.filter(
      &(String.at(s, 0) == String.at(s, l - &1 - 1) and
          String.at(s, &1 - 1) == String.at(s, l - 1))
    )
  end
end
