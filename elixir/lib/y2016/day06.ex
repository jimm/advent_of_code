defmodule Y2016.Day06 do
  def part1(_ctx, lines) do
    run(lines, fn {_, freq} -> freq end)
  end

  def part2(_ctx, lines) do
    run(lines, fn {_, freq} -> -freq end)
  end

  defp run(lines, f) do
    len = lines |> hd |> String.length()

    0..(len - 1)
    |> Enum.map(fn index ->
      {ch, _} =
        lines
        |> Enum.map(fn line -> line |> String.at(index) end)
        |> Enum.reduce(%{}, fn ch, m ->
          Map.put(m, ch, Map.get(m, ch, 0) + 1)
        end)
        |> Enum.max_by(f)

      ch
    end)
    |> Enum.join("")
    |> IO.puts()
  end
end

# Y2016.Day06.run1
# Y2016.Day06.run2
