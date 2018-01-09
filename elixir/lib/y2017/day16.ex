# Permutation Promenade

defmodule Y2017.Day16 do
  use Common.File

  def part1 do
    line = Enum.to_list(?a..?p)
    instructions = read_instructions()
    dance(line, instructions)
  end

  def part2 do
    line = Enum.to_list(?a..?p)
    instructions = read_instructions()

    cycle = generate_dance_cycle(line, instructions)
    cycle_len = length(cycle)
    num_dances_needed = Integer.mod(1_000_000_000, cycle_len)

    0..(num_dances_needed - 1)
    |> Enum.reduce(line, fn _, l -> dance(l, instructions) end)
  end

  # ================ testing ================

  def test1 do
    line = Enum.to_list(?a..?e)

    instructions = [
      {:spin, 1},
      {:exchange, 3, 4},
      {:partner, ?e, ?b}
    ]

    Enum.reduce(instructions, line, &interpret/2)
  end

  # ================ part 1 ================

  defp dance(line, instructions) do
    Enum.reduce(instructions, line, &interpret/2)
  end

  defp interpret({:spin, n}, line) do
    m = length(line) - n
    Enum.drop(line, m) ++ Enum.take(line, m)
  end

  defp interpret({:exchange, i, j}, line) when i > j do
    interpret({:exchange, j, i}, line)
  end

  defp interpret({:exchange, i, j}, line) do
    {part1, parts23} = Enum.split(line, i)
    {part2, part3} = Enum.split(parts23, j - i)
    [ei | p2] = part2

    {ej_part, p3} =
      if part3 == [] do
        {[], []}
      else
        [ej | p3] = part3
        {[ej], p3}
      end

    part1 ++ ej_part ++ p2 ++ [ei] ++ p3
  end

  defp interpret({:partner, a, b}, line) do
    i = Enum.find_index(line, fn s -> s == a end)
    j = Enum.find_index(line, fn s -> s == b end)
    interpret({:exchange, i, j}, line)
  end

  # ================ part 2 ================

  defp generate_dance_cycle(line, instructions) do
    generate_dance_cycle(line, instructions, [], MapSet.new())
  end

  defp generate_dance_cycle(line, instructions, cycle, ms) do
    next_line = dance(line, instructions)

    if MapSet.member?(ms, next_line) do
      Enum.reverse(cycle)
    else
      generate_dance_cycle(
        next_line,
        instructions,
        [next_line | cycle],
        MapSet.put(ms, next_line)
      )
    end
  end

  # ================ helpers ================

  defp read_instructions do
    input_lines()
    |> hd()
    |> String.split(",")
    |> Enum.map(&parse_instruction/1)
  end

  def parse_instruction(<<?s>> <> rest) do
    {:spin, String.to_integer(rest)}
  end

  def parse_instruction(<<?x>> <> rest) do
    [a, b] = String.split(rest, "/")
    {:exchange, String.to_integer(a), String.to_integer(b)}
  end

  def parse_instruction(<<?p>> <> rest) do
    [a, b] = String.split(rest, "/")
    {:partner, a |> String.to_charlist() |> hd(), b |> String.to_charlist() |> hd()}
  end
end
