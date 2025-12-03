# Knot Hash

defmodule Y2017.Day10 do
  use Common.File
  import Bitwise

  @rope_len 256
  @test_rope_len 5
  @test_input [3, 4, 1, 5]

  @part2_input_suffix [17, 31, 73, 47, 23]

  def part1 do
    rope = init_rope(@rope_len)
    {[first, second | _], _, _} = knot(rope, length(rope), read_input_lengths(), 0, 0)
    first * second
  end

  def test1 do
    rope = init_rope(@test_rope_len)
    {[first, second | _], _, _} = knot(rope, length(rope), @test_input, 0, 0)
    first * second
  end

  def part2 do
    part2(@rope_len, read_input_chars())
  end

  # For use by the external world.
  def part2(rope_len, input_charlist) do
    rope = init_rope(rope_len)
    chars = input_charlist ++ @part2_input_suffix

    {sparse_hash, _, _, _} =
      0..63
      |> Enum.reduce({rope, length(rope), 0, 0}, fn _, {rope, rlen, i, s} ->
        {new_rope, new_i, new_s} = knot(rope, rlen, chars, i, s)
        {new_rope, rlen, new_i, new_s}
      end)

    dense_hash = consolidate(sparse_hash)
    to_hex(dense_hash)
  end

  # ================ part 1 ================

  defp read_input_lengths do
    input_lines()
    |> hd
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  # ================ part 2 ================

  defp read_input_chars do
    input_lines()
    |> hd
    |> String.to_charlist()
  end

  defp consolidate(sparse_hash) do
    consolidate(sparse_hash, [])
  end

  defp consolidate([], dense), do: Enum.reverse(dense)

  defp consolidate(sparse_hash, dense) do
    block = Enum.take(sparse_hash, 16)
    consolidate(Enum.drop(sparse_hash, 16), [xor(block) | dense])
  end

  defp xor([byte | t]) do
    Enum.reduce(t, byte, fn b, val -> bxor(val, b) end)
  end

  defp to_hex(xs), do: to_hex(xs, "")

  defp to_hex([], str), do: String.downcase(str)

  defp to_hex([x | xs], str) when x < 16 do
    to_hex(xs, str <> "0#{Integer.to_string(x, 16)}")
  end

  defp to_hex([x | xs], str) do
    to_hex(xs, str <> Integer.to_string(x, 16))
  end

  # ================ helpers ================

  def knot(rope, _, [], index, skip_size) do
    {rope, index, skip_size}
  end

  def knot(rope, rope_len, [len | t], index, skip_size) do
    knot(
      reverse(rope, rope_len, index, len),
      rope_len,
      t,
      clamp(index + len + skip_size, rope_len),
      skip_size + 1
    )
  end

  defp reverse(rope, rope_len, i, len) do
    rope
    |> double
    |> Enum.reverse_slice(i, len)
    |> resplice(rope_len, i, len)
  end

  defp double(rope), do: rope ++ rope

  defp resplice(rope, rope_len, i, len) when i + len < rope_len do
    Enum.take(rope, rope_len)
  end

  defp resplice(rope, rope_len, i, len) do
    from_end = rope_len - i
    from_start_of_double = len - from_end

    Enum.slice(rope, rope_len..(rope_len + from_start_of_double - 1)) ++
      Enum.slice(rope, from_start_of_double..(rope_len - 1))
  end

  defp clamp(n, max) when n < max, do: n
  defp clamp(n, max), do: clamp(n - max, max)

  defp init_rope(len) do
    0..(len - 1) |> Enum.to_list()
  end
end
