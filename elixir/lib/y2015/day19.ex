# Medicine for Rudolph

defmodule Y2015.Day19 do
  use Common.File

  @input_file default_input_path()

  # single sub unique count
  def run1(input_file \\ @input_file) do
    {replacements, start} = parse(input_file)

    replacements
    |> Enum.map(fn [key, val] -> start |> String.split(key) |> make_subs(key, val) end)
    |> List.flatten()
    |> Enum.uniq()
    |> length
  end

  # "e" to medicine
  # Will it help to work backwards? Start with longest substitutions?
  def run2(input_file \\ @input_file) do
    {replacements, medicine} = parse(input_file)
    reverse_replacements = replacements |> Enum.map(fn [k, v] -> [v, k] end)
    by_length = reverse_replacements |> Enum.group_by(fn [k, _] -> String.length(k) end)
    reduce_to_e(medicine, by_length, by_length |> Map.keys() |> Enum.max(), 0)
  end

  def parse(input_file) do
    lines = File.read!(input_file) |> String.split("\n")
    {replacement_lines, end_lines} = lines |> Enum.split(length(lines) - 2)
    {
      replacement_lines
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&String.split(&1, " => ")),
      end_lines |> hd
    }
  end

  def make_subs(split_points, key, val) do
    make_subs(split_points, key, val, 1, length(split_points), [])
  end

  # split points after, split points before, val, subs
  def make_subs(_, _, _, len, len, subs), do: subs

  def make_subs(split_points, key, val, n, len, subs) do
    {befores, afters} = Enum.split(split_points, n)
    subbed = (befores |> Enum.join(key)) <> val <> (afters |> Enum.join(key))
    make_subs(split_points, key, val, n + 1, len, [subbed | subs])
  end

  # ================ reduction ================

  def reduce_to_e("e", _, _, num_steps) do
    num_steps
  end

  def reduce_to_e(s, mappings, 0, num_steps) do
    reduce_to_e(s, mappings, mappings |> Map.keys() |> Enum.max(), num_steps)
  end

  def reduce_to_e(s, mappings, len, num_steps) do
    len_mappings = Map.get(mappings, len, [])

    [reduced, count] =
      len_mappings
      |> Enum.reduce([s, 0], fn [long, short], [s, steps] ->
        [count, s] = replace_and_count(s, long, short, 0)
        [s, steps + count]
      end)

    reduce_to_e(reduced, mappings, len - 1, num_steps + count)
  end

  def replace_and_count(s, old, new, count) do
    replaced = String.replace(s, old, new, global: false)

    if replaced == s do
      [count, s]
    else
      replace_and_count(replaced, old, new, count + 1)
    end
  end
end
