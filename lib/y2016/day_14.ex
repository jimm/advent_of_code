defmodule Y2016.Day14 do
  @input "ihaygndm"
  @num_additional_hashes 2016
  @nth_key_desired 64

  use Bitwise

  def run1(input \\ @input) do
    nth_key(input, 0, %{}, [], 0, nibbles())
  end

  def run2(input \\ @input) do
    nth_key(input, 0, %{}, [], @num_additional_hashes, nibbles())
  end

  # TODO use this to speed things up
  def nibbles do
    (0..255) |> Enum.reduce(%{}, fn i, m ->
      s = Integer.to_string(i, 16)
      val = if i < 16, do: "0" <> s, else: s
      Map.put(m, i, String.downcase(val))
    end)
  end

  def nth_key(_, _, _, key_indexes, _, _) when length(key_indexes) == @nth_key_desired, do: Enum.max(key_indexes)
  # DEBUG
  def nth_key(_, 25000, _, _, _, _) do
    raise "index 25000 is too high"
  end
  def nth_key(input, index, last_seen_threes, key_indexes, num_additional_hashes, nibbles) do
    if Integer.mod(index, 1000) == 0 do
      IO.puts "*** index #{index}"
    end
    hash =
      "#{input}#{index}"
      |> hash(1+num_additional_hashes, nibbles)
      |> String.split("", trim: true)
    triplet_char = first_triplet_in(hash)
    new_last_seen_threes = if triplet_char do
      # IO.puts "index #{index}: #{triplet_char}" # DEBUG
      past = Map.get(last_seen_threes, triplet_char, [])
      Map.put(last_seen_threes, triplet_char, [index|past])
    else
      last_seen_threes
    end

    fives = fives_in(hash)
    
    # DEBUG
    unless Enum.empty?(fives) do
      fives |> IO.inspect(label: "  fives") # DEBUG
    end

    # Add all eligible indexes for each five, remove those and all earlier
    # from new_last_seen_threes.
    new_indexes = 
      fives
      |> Enum.flat_map(fn five ->
           Map.get(last_seen_threes, five, [])
           |> Enum.filter(fn three_index -> index - three_index <= 1000 end)
      end)
                             
unless Enum.empty?(new_indexes) do
  new_indexes |> IO.inspect(label: "  new_indexes") # DEBUG
  length(new_indexes ++ key_indexes) |> IO.inspect(label: "total keys including new") # DEBUG
end

    [new_last_seen_threes, new_key_indexes] =
      if length(new_indexes) > 0 do
        lst =
          new_last_seen_threes
          |> Enum.reduce(%{}, fn {k, vals}, m ->
               new_vals = vals |> Enum.filter(fn v -> v >= index - 1000 end)
               Map.put(m, k, new_vals)
          end)
        all_keys =
          (key_indexes ++ Enum.reverse(new_indexes)) # we'll ignore highest extras
          |> Enum.take(@nth_key_desired)
        [lst, all_keys]
      else
        [new_last_seen_threes, key_indexes]
      end

    nth_key(input, index+1, new_last_seen_threes, new_key_indexes,
      num_additional_hashes, nibbles)
  end

  def hash(s, 0, _), do: s
  def hash(s, num_additional_hashes, nibbles) do
    :crypto.hash(:md5, s)
    |> to_nibbles([], nibbles)
    |> Enum.join
    |> hash(num_additional_hashes - 1, nibbles)
  end

  def to_nibbles(<<>>, nibbles, _), do: Enum.reverse(nibbles)
  def to_nibbles(<<a, rest :: binary>>, nibbles, nibble_map) do
    to_nibbles(rest, [Map.get(nibble_map, a) | nibbles], nibble_map)
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
# # => (20076 is too high)
