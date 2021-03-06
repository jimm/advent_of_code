# Elves Look, Elves Say

defmodule Y2015.Day10 do
  @start "3113322113"

  def run1, do: look_and_say(40)
  def run2, do: look_and_say(50)

  def look_and_say(iterations) do
    1..iterations
    |> Enum.reduce(@start, fn _, n -> encode(n) end)
    |> String.length()
  end

  defp encode(n) do
    encode(n, 0, 0, "")
  end

  defp encode("", 0, _, _), do: ""
  defp encode("", n, c, answer), do: add_encoded(answer, n, c)

  defp encode(<<c::utf8, rest::binary>>, n, c, answer) do
    encode(rest, n + 1, c, answer)
  end

  defp encode(<<c::utf8, rest::binary>>, 0, _, answer) do
    encode(rest, 1, c, answer)
  end

  defp encode(<<c::utf8, rest::binary>>, n, prev_c, answer) do
    encode(rest, 1, c, add_encoded(answer, n, prev_c))
  end

  defp add_encoded(answer, n, c), do: "#{answer}#{n}#{<<c>>}"
end
