# ================ Trash Compactor ================

defmodule Y2025.Day06 do
  def part1(_ctx, lines) do
    do_part(lines, &parse_problems/1)
  end

  def part2(_ctx, lines) do
    do_part(lines, &parse_problems_the_cephalopod_way/1)
  end

  defp do_part(lines, parse_func) do
    parse_func.(lines)
    |> Enum.map(fn {numbers, operation} -> Enum.reduce(numbers, operation) end)
    |> Enum.sum()
  end

  # ================ helpers ================

  @doc """
  Returns a list of {numbers, operation} tuples.
  """
  def parse_problems(lines), do: parse_problems(lines, [])

  # Returns a list of {numbers, operation} tuples.
  defp parse_problems([], problems), do: problems

  defp parse_problems([line | lines], problems) do
    vals = String.split(line, " ", trim: true)

    new_problems =
      if Enum.at(vals, 0) in ["+", "*"] do
        add_operations(vals, problems)
      else
        add_numbers(vals, problems)
      end

    parse_problems(lines, new_problems)
  end

  # Add operations to problems.
  defp add_operations(vals, problems) do
    operations = vals |> Enum.map(fn s -> if(s == "+", do: &+/2, else: &*/2) end)

    problems
    |> Enum.zip(operations)
    |> Enum.map(fn {{numbers, _}, op} -> {numbers, op} end)
  end

  # Add numbers, but we don't have any problems yet so create empty ones.
  defp add_numbers(vals, []) do
    add_numbers(vals, Enum.map(vals, fn _ -> {[], nil} end))
  end

  # Add numbers to problems.
  defp add_numbers(vals, problems) do
    numbers = vals |> Enum.map(&String.to_integer/1)

    problems
    |> Enum.zip(numbers)
    |> Enum.map(fn {{numbers, op}, num} -> {[num | numbers], op} end)
  end

  @doc """
  Returns a list of {numbers, operation} tuples.
  """
  def parse_problems_the_cephalopod_way(_lines) do
    # TODO
  end
end
