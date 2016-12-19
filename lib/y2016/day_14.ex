defmodule Y2016.Day14 do
  @input "ihaygndm"

  use Bitwise

  def run1(input \\ @input) do
    nth_key(input, 0, %{}, [], 64)
  end

  def nth_key(_, _, _, [i|_] = key_indexes, n) when length(key_indexes) == n, do: i
  def nth_key(input, index, last_seen_threes, key_indexes, n) do
    if index > 10000 do         # DEBUG
      raise "index > 10000"
    end
    hash = :crypto.hash(:md5, "#{input}#{index}") |> to_nibbles([])
    threes = threes_in(hash)
    new_last_seen_threes =
      threes
      |> Enum.reduce(last_seen_threes, fn th, m ->
           past = Map.get(m, th, [])
           Map.put(m, th, [index|past])
         end)

    fives = fives_in(hash)
    if length(fives) > 0 do
      fives |> IO.inspect(label: "fives") # DEBUG
      last_seen_threes |> IO.inspect(label: "  last_seen_threes") # DEBUG
    end
# FIXME add all eligible indexes for each five,
#       remove those and all earlier from new_last_seen_threes
    [new_last_seen_threes, new_key_indexes] =
      if length(fives) > 0 && Enum.any?(fives, fn five ->
           Map.get(last_seen_threes, five, [])
           |> Enum.any?(fn three_index -> index - three_index <= 1000 end)
          end)
      do
        [index | key_indexes]
      else
        [new_last_seen_threes, key_indexes]
      end

    nth_key(input, index+1, new_last_seen_threes, new_key_indexes, n)
  end

  def to_nibbles(<<>>, nibbles), do: Enum.reverse(nibbles)
  def to_nibbles(<<a, rest :: binary>>, nibbles) do
    chars = a |> Integer.to_string(16) |> String.split("", trim: true)
    new_nibbles = case chars do
                    [high, low] -> [low | [high | nibbles]]
                    [low] -> [low | ["0" | nibbles]]
                  end
    to_nibbles(rest, new_nibbles)
  end

  def threes_in(s), do: threes_in(s, [])

  def threes_in(nibbles, found) when length(nibbles) < 3, do: found
  def threes_in([a | [a | [a | rest]]], found) do
    threes_in(rest, [a|found])
  end
  def threes_in([_ | rest], found) do
    threes_in(rest, found)
  end

  def fives_in(s), do: fives_in(s, [])

  def fives_in(nibbles, found) when length(nibbles) < 5, do: found
  def fives_in([a|[a|[a|[a|[a|rest]]]]], found) do
    fives_in(rest, [a|found])
  end
  def fives_in([_ | rest], found) do
    fives_in(rest, found)
  end
end

# Y2016.Day14.run1("abc")
# # =>
