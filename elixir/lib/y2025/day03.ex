# ================ Lobby ================

defmodule Y2025.Day03 do
  def part1(_ctx, lines) do
    lines |> Enum.map(&max_joltage_2/1) |> Enum.sum()
  end

  def part2(_ctx, lines) do
    lines |> Enum.map(&max_joltage_12/1) |> Enum.sum()
  end

  # ================ helpers ================

  defp max_joltage_2(line) do
    charges = line |> String.codepoints() |> Enum.map(&String.to_integer/1)
    len = length(charges)
    # brute force
    Enum.reduce(0..(len - 2), 0, fn i, acc ->
      ci10 = Enum.at(charges, i) * 10

      Enum.reduce((i + 1)..(len - 1), acc, fn j, acc ->
        charge = ci10 + Enum.at(charges, j)
        if charge > acc, do: charge, else: acc
      end)
    end)
  end

  defp max_joltage_12(line) do
    charges = line |> String.codepoints() |> Enum.map(&String.to_integer/1)
    len = length(charges)
    max_joltage(charges, len, 12, [])
  end

  defp max_joltage(_, len, n, max_charge) when n >= len do
    # DEBUG
    IO.puts("wrapping up, len = #{len}, n = #{n}, max_charge = #{max_charge}")
    max_charge
  end

  defp max_joltage(charges, len, n, max_charge) do
    # DEBUG
    IO.puts("\nin second one, input charges = #{inspect(charges)}")

    charges =
      Enum.drop_while(charges, fn
        [first | [second | _]] -> first < second
        [_] -> false
      end)

    # DEBUG
    IO.puts("in second one, output charges = #{inspect(charges)}")
    max_joltage(tl(charges), len, n - 1, max_charge * 10 + hd(charges))
  end
end
