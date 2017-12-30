# Dueling Generators

defmodule Y2017.Day15 do
  use Bitwise

  @factor_a 16807
  @factor_b 48271
  @product 2147483647
  @part_1_sample_size 40_000_000
  @part_2_sample_size  5_000_000

  # I could read the values from my input file, but it's easier to hard-code
  # them here.
  @start_a 679
  @start_b 771

  @test_start_a 65
  @test_start_b 8921

  def part1(start_a \\ @start_a, start_b \\ @start_b) do
    a = generator(start_a, @factor_a)
    b = generator(start_b, @factor_b)

    Stream.zip(a, b)
    |> Stream.take(@part_1_sample_size)
    |> Stream.filter(fn({a, b}) -> (a &&& 0xffff) == (b &&& 0xffff) end)
    |> Enum.count
  end

  def part2(start_a \\ @start_a, start_b \\ @start_b) do
    a = generator(start_a, @factor_a)
    |> Stream.filter(fn(i) -> (i &&& 3) == 0 end)
    b = generator(start_b, @factor_b)
    |> Stream.filter(fn(i) -> (i &&& 7) == 0 end)

    Stream.zip(a, b)
    |> Stream.take(@part_2_sample_size)
    |> Stream.filter(fn({a, b}) -> (a &&& 0xffff) == (b &&& 0xffff) end)
    |> Enum.count
  end

  # ================ testing ================

  def test_values do
    a = generator(@test_start_a, @factor_a)
    b = generator(@test_start_b, @factor_b)
    a |> Enum.take(5) |> IO.inspect(label: "generator a")
    b |> Enum.take(5) |> IO.inspect(label: "generator b")
  end

  def test_part1 do
    part1(@test_start_a, @test_start_b)
  end

  def test_part2 do
    part2(@test_start_a, @test_start_b)
  end

  # ================ helpers ================

  defp generator(initial_value, factor) do
    Stream.iterate(initial_value, fn(val) ->
      Integer.mod(val * factor, @product)
    end)
    |> Stream.drop(1)
  end
end
