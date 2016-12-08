defmodule Y2016.Day06 do
  @input_file "data/y2016/day_06.txt"

  def run1(file \\ @input_file) do
    run(file, fn {_, freq} -> freq end)
  end

  def run2(file \\ @input_file) do
    run(file, fn {_, freq} -> -freq end)
  end

  defp run(file, f) do
    lines = read_lines(file)
    len = lines |> hd |> String.length
    (0..len-1)
    |> Enum.map(fn index ->
      {ch, _} =
        lines
        |> Enum.map(fn line -> line |> String.at(index) end)
        |> Enum.reduce(%{}, fn(ch, m) ->
             Map.put(m, ch, Map.get(m, ch, 0) + 1)
           end)
        |> Enum.max_by(f)
      ch
    end)
    |> Enum.join("")
    |> IO.puts
  end

  defp read_lines(file) do
    file
    |> File.read!
    |> String.split("\n", trim: true)
  end
end

# Y2016.Day06.run1
# Y2016.Day06.run2
