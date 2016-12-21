defmodule Y2016.Day16 do
  @disk_len 272
  @input "10111100110001111"

  require Integer

  def run1(disk_len \\ @disk_len, input \\ @input) do
    bits =
      input
      |> to_bits
      |> Stream.iterate(&next_gen/1)
      |> Stream.drop_while(fn bits -> length(bits) < disk_len end)
      |> Enum.take(1)
      |> hd

    Stream.iterate(bits |> Enum.take(disk_len), &checksum/1)
    |> Stream.drop_while(fn csum -> csum |> length |> Integer.is_even end)
    |> Enum.take(1)
    |> hd
    |> Enum.join
  end

  def to_bits(s) do
    s
    |> String.split("", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def next_gen(bits) do
    bits ++ [0] ++ (bits |> Enum.reverse |> flip_bits([]))
  end

  def flip_bits([], flipped), do: Enum.reverse(flipped)
  def flip_bits([1|rest], flipped), do: flip_bits(rest, [0|flipped])
  def flip_bits([0|rest], flipped), do: flip_bits(rest, [1|flipped])

  def checksum(bits) when bits |> length |> Integer.is_even, do: checksum(bits, [])
  def checksum(bits), do: bits

  def checksum([], csum), do: csum |> Enum.reverse
  def checksum([b | [b | rest]], csum), do: checksum(rest, [1 | csum])
  def checksum([_ | [_ | rest]], csum), do: checksum(rest, [0 | csum])
end

# Y2016.Day16.run1
# #=> 11100110111101110

# Y2016.Day16.run1(35651584)
# #=>
