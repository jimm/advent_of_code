# Hex Ed
defmodule Y2017.Day11 do
  def part1 do
    steps = read_steps()
  end

  defp read_steps do
    __MODULE__
    |> CF.default_input_path
    |> CF.lines
    |> hd
    |> String.split(",")
  end
end
