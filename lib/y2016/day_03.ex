defmodule Y2016.Day03 do
  @input_file "data/y2016/day_03.txt"

  def run1(file \\ @input_file) do
    read_file(file) |> do_run
  end

  def run2(file \\ @input_file) do
    read_file2(file)
    |> IO.inspect(label: "file2 output") # DEBUG
    |> do_run
  end

  def do_run(triplets) do
    triplets
    |> Enum.filter(fn {a, b, c} -> triangle?(a, b, c) end)
    |> Enum.count
    |> IO.puts
  end

  defp read_file(file) do
    file
    |> File.read!
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [a, b, c] = line |> String.split |> Enum.map(&String.to_integer/1)
      {a, b, c}
    end)
  end

  defp read_file2(file) do
    file
    |> File.read!
    |> String.split("\n", trim: true)
    |> Enum.chunk(3)
    |> Enum.map(fn three ->
      [{a0, a1, a2},
       {b0, b1, b2},
       {c0, c1, c2}] =
        three |> Enum.map(fn line ->
           [a, b, c] = line |> String.split |> Enum.map(&String.to_integer/1)
           {a, b, c}
          end)
      [{a0, b0, c0}, {a1, b1, c1}, {a2, b2, c2}]
    end)
    |> List.flatten
  end

  defp triangle?(a, b, c) when c >= a and c >= b do
    a + b > c
  end
  defp triangle?(a, b, c) when b >= a and b >= c do
    triangle?(a, c, b)
  end
  defp triangle?(a, b, c) do
    triangle?(b, c, a)
  end
end

# Y2016.Day03.run1
# Y2016.Day03.run2
