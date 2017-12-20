# Memory Reallocation

defmodule Y2017.Day06 do

  use Common.File

  def part1 do
    banks = read_banks()
    {num_cycles, _} = cycle_until_repeat(banks)
    num_cycles
  end

  def part2 do
    banks = read_banks()
    {_, cycle_length} = cycle_until_repeat(banks)
    cycle_length
  end

  # ================ helpers ================

  # returns {num cycles, length between original and repeat}
  defp cycle_until_repeat(banks) do
    cycle_until_repeat(banks, 0, %{})
  end

  defp cycle_until_repeat(banks, cycles, state) do
    if seen_before?(banks, state) do
      {cycles, cycles - Map.get(state, banks)}
    else
      new_banks = redistribute(banks)
      new_state = Map.put(state, banks, cycles)
      cycle_until_repeat(new_banks, cycles + 1, new_state)
    end
  end

  defp seen_before?(banks, state), do: Map.get(state, banks) != nil

  defp read_banks do
    # DEBUG test data
    # [0, 2, 7, 0]

    input_lines()
    |> hd
    |> String.split
    |> Enum.map(&String.to_integer/1)
  end

  defp redistribute(banks) do
    max = Enum.max(banks)
    max_index = Enum.find_index(banks, fn(x) -> x == max end)
    new_banks = List.replace_at(banks, max_index, 0)
    len = length(new_banks)
    redistribute(new_banks, len, max, clamp(max_index + 1, len))
  end

  defp redistribute(banks, _, 0, _) do
    banks
  end
  defp redistribute(banks, len, max, i) do
    val = Enum.at(banks, i)
    new_banks = List.replace_at(banks, i, val+1)
    redistribute(new_banks, len, max-1, clamp(i+1, len))
  end

  # Simplifying assumption: we know we'll only be incrementing by 1
  defp clamp(val, maxval) when val >= maxval, do: 0
  defp clamp(val, _), do: val
end
