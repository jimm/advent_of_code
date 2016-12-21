defmodule Y2016.Day06 do
  require Common.File, as: CF

  @input_file CF.default_input_path(__MODULE__)

  def run1(file \\ @input_file) do
    run(file, fn {_, freq} -> freq end)
  end

  def run2(file \\ @input_file) do
    run(file, fn {_, freq} -> -freq end)
  end

  defp run(file, f) do
    lines = CF.lines(file)
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
end

# Y2016.Day06.run1
# Y2016.Day06.run2
