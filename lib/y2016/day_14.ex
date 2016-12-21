defmodule Y2016.Day14 do
  @input "ihaygndm"
  @num_additional_hashes 2016

  use Bitwise

  def run1(input \\ @input) do
    nth_key(input, 0, %{}, [], 64, 0)
  end

  def run2(input \\ @input) do
    nth_key(input, 0, %{}, [], 64, @num_additional_hashes)
  end

  def nth_key(_, _, _, key_indexes, n, _) when length(key_indexes) == n, do: Enum.max(key_indexes)
  def nth_key(input, index, last_seen_threes, key_indexes, n, num_additional_hashes) do
    hash =
      "#{input}#{index}"
      |> hash(1+num_additional_hashes)
      |> String.split("", trim: true)
    triplet_char = first_triplet_in(hash)
    new_last_seen_threes = if triplet_char do
      past = Map.get(last_seen_threes, triplet_char, [])
      Map.put(last_seen_threes, triplet_char, [index|past])
    else
      last_seen_threes
    end

    fives = fives_in(hash)

    # add all eligible indexes for each five, remove those and all earlier
    # from new_last_seen_threes
    new_indexes = 
      fives
      |> Enum.flat_map(fn five ->
           Map.get(last_seen_threes, five, [])
           |> Enum.filter(fn three_index -> index - three_index <= 1000 end)
      end)
    [new_last_seen_threes, new_key_indexes] =
      if length(new_indexes) > 0 do
        lst =
          new_last_seen_threes
          |> Enum.reduce(%{}, fn {k, vals}, m ->
               new_vals = vals |> Enum.filter(fn v -> v >= index - 1000 end)
               Map.put(m, k, new_vals)
          end)
        [lst, new_indexes ++ key_indexes]
      else
        [new_last_seen_threes, key_indexes]
      end

    nth_key(input, index+1, new_last_seen_threes, new_key_indexes, n, num_additional_hashes)
  end

  def hash(s, 0), do: s
  def hash(s, num_additional_hashes) do
    :crypto.hash(:md5, s)
    |> to_nibbles([])
    |> Enum.join
    |> String.downcase
    |> hash(num_additional_hashes - 1)
  end

  def to_nibbles(<<>>, nibbles), do: Enum.reverse(nibbles)
  def to_nibbles(<<a, rest :: binary>>, nibbles) do
    s = Integer.to_string(a, 16)
    new_nibbles = if a < 16, do: [s | ["0" | nibbles]], else: [s | nibbles]
    to_nibbles(rest, new_nibbles)
  end

  def first_triplet_in(nibbles) when length(nibbles) < 3, do: nil
  def first_triplet_in([a | [a | [a | _]]]), do: a
  def first_triplet_in([_ | rest]), do: first_triplet_in(rest)

  def fives_in(s), do: fives_in(s, [])

  def fives_in(nibbles, found) when length(nibbles) < 5, do: found
  def fives_in([a|[a|[a|[a|[a|rest]]]]], found) do
    fives_in(rest, [a|found])
  end
  def fives_in([_ | rest], found) do
    fives_in(rest, found)
  end
end

# Y2016.Day14.run1
# # => 15035

# Y2016.Day14.run2
# # => 
