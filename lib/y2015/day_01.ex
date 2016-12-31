defmodule Y2015.Day01 do
  alias Common.File, as: CF

  def destination_floor do
    __MODULE__
    |> CF.default_input_path
    |> File.read!
    |> String.split("", trim: true)
    |> IO.inspect(label: "codepoints") # DEBUG
    |> Enum.reduce(0, fn(c, acc) -> acc + move(c) end)
  end

  def first_basement_index do
    __MODULE__
    |> CF.default_input_path
    |> File.read!
    |> String.codepoints
    |> first_negative_one(0, 0)
  end

  defp move("("), do: 1
  defp move(")"), do: -1

  defp first_negative_one(_, -1, index), do: index
  defp first_negative_one([], _, _), do: raise "not found"
  defp first_negative_one([c|rest], floor, index) when c == "(" do
    first_negative_one(rest, floor+1, index+1)
  end
  defp first_negative_one([_|rest], floor, index) do
    first_negative_one(rest, floor-1, index+1)
  end
end

# Y2015.Day01.destination_floor
# Y2015.Day01.first_basement_index
