defmodule Y2016.Day05 do
  use Bitwise

  def run1(input) do
    Stream.iterate(0, &(&1 + 1))
    |> Stream.map(fn index ->
      <<a, b, c, _::binary>> = :crypto.hash(:md5, "#{input}#{index}")
      {a, b, c}
    end)
    |> Stream.filter(fn {a, b, c} -> a == 0 && b == 0 && c < 16 end)
    |> Enum.take(8)
    |> Enum.map(fn {_, _, c} -> c |> Integer.to_string(16) end)
    |> Enum.join("")
    |> IO.puts()
  end

  def run2(input) do
    run2(input, 0, [0, 0, 0, 0, 0, 0, 0, 0])
    |> IO.puts()
  end

  defp run2(_, _, [a, b, c, d, e, f, g, h] = bytes)
       when a != 0 and b != 0 and c != 0 and d != 0 and e != 0 and f != 0 and g != 0 and h != 0 do
    bytes |> Enum.join("")
  end

  defp run2(input, index, bytes) do
    <<a, b, c, d, _::binary>> = :crypto.hash(:md5, "#{input}#{index}")
    run2(input, index + 1, new_bytes(bytes, a, b, c, d))
  end

  defp new_bytes([0, b1, b2, b3, b4, b5, b6, b7], 0, 0, c, d) when c == 0 do
    # DEBUG
    [hexc(d), b1, b2, b3, b4, b5, b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, 0, b2, b3, b4, b5, b6, b7], 0, 0, c, d) when c == 1 do
    # DEBUG
    [b0, hexc(d), b2, b3, b4, b5, b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, 0, b3, b4, b5, b6, b7], 0, 0, c, d) when c == 2 do
    # DEBUG
    [b0, b1, hexc(d), b3, b4, b5, b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, b2, 0, b4, b5, b6, b7], 0, 0, c, d) when c == 3 do
    # DEBUG
    [b0, b1, b2, hexc(d), b4, b5, b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, b2, b3, 0, b5, b6, b7], 0, 0, c, d) when c == 4 do
    # DEBUG
    [b0, b1, b2, b3, hexc(d), b5, b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, b2, b3, b4, 0, b6, b7], 0, 0, c, d) when c == 5 do
    # DEBUG
    [b0, b1, b2, b3, b4, hexc(d), b6, b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, b2, b3, b4, b5, 0, b7], 0, 0, c, d) when c == 6 do
    # DEBUG
    [b0, b1, b2, b3, b4, b5, hexc(d), b7]
    |> IO.inspect()
  end

  defp new_bytes([b0, b1, b2, b3, b4, b5, b6, 0], 0, 0, c, d) when c == 7 do
    # DEBUG
    [b0, b1, b2, b3, b4, b5, b6, hexc(d)]
    |> IO.inspect()
  end

  defp new_bytes(bytes, _, _, _, _) do
    bytes
  end

  defp hexc(d), do: d |> bsr(4) |> Integer.to_string(16)
end

# Y2016.Day05.run1("abc")
# => 18f47a30
# Y2016.Day05.run1("ffykfhsq")

# Y2016.Day05.run2("abc")
# => 05ace8e3
# Y2016.Day05.run2("ffykfhsq")
