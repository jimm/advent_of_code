defmodule Y2016.Day02 do
  require Common.File, as: CF

  @input_file CF.default_input_path(__MODULE__)
  @keypad1 %{
    1 => %{"U" => nil, "D" => 4, "L" => nil, "R" => 2},
    2 => %{"U" => nil, "D" => 5, "L" => 1, "R" => 3},
    3 => %{"U" => nil, "D" => 6, "L" => 2, "R" => nil},
    4 => %{"U" => 1, "D" => 7, "L" => nil, "R" => 5},
    5 => %{"U" => 2, "D" => 8, "L" => 4, "R" => 6},
    6 => %{"U" => 3, "D" => 9, "L" => 5, "R" => nil},
    7 => %{"U" => 4, "D" => nil, "L" => nil, "R" => 8},
    8 => %{"U" => 5, "D" => nil, "L" => 7, "R" => 9},
    9 => %{"U" => 6, "D" => nil, "L" => 8, "R" => nil},
  }
  @keypad2 %{
    1 => %{"U" => nil, "D" => 3, "L" => nil, "R" => nil},
    2 => %{"U" => nil, "D" => 6, "L" => nil, "R" => 3},
    3 => %{"U" => 1, "D" => 7, "L" => 2, "R" => 4},
    4 => %{"U" => nil, "D" => 8, "L" => 3, "R" => nil},
    5 => %{"U" => nil, "D" => nil, "L" => nil, "R" => 6},
    6 => %{"U" => 2, "D" => "A", "L" => 5, "R" => 7},
    7 => %{"U" => 3, "D" => "B", "L" => 6, "R" => 8},
    8 => %{"U" => 4, "D" => "C", "L" => 7, "R" => 9},
    9 => %{"U" => nil, "D" => nil, "L" => 8, "R" => nil},
    "A" => %{"U" => 6, "D" => nil, "L" => nil, "R" => "B"},
    "B" => %{"U" => 7, "D" => "D", "L" => "A", "R" => "C"},
    "C" => %{"U" => 8, "D" => nil, "L" => "B", "R" => nil},
    "D" => %{"U" => "B", "D" => nil, "L" => nil, "R" => nil},
  }

  def run1(keypad, file \\ @input_file)
  def run1(1, file), do: run1(@keypad1, file)
  def run1(2, file), do: run1(@keypad2, file)
  def run1(keypad, file) do
    {_, digits} =
      read_dirs(file)
      |> Enum.reduce({5, []}, fn (dirs, {start, digits}) ->
        new_digit = dirs |> Enum.reduce(start, fn (dir, digit) ->
          next_digit(keypad, digit, dir)
        end)
        {new_digit, [new_digit | digits]}
      end)
      Enum.reverse(digits)
      |> Enum.join("")
      |> IO.puts
  end

  defp read_dirs(file) do
    CF.lines(file)
    |> Enum.map(&(String.split(&1, "", trim: true)))
  end

  defp next_digit(keypad, digit, dir) do
    new_digit = keypad |> Map.get(digit) |> Map.get(dir)
    new_digit || digit
  end
end

# Y2016.Day02.run1(1)
# Y2016.Day02.run1(2)
