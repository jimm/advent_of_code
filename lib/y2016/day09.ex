defmodule Y2016.Day09 do
  alias Common.File, as: CF

  def run1(file \\ nil) do
    run(file, &uncomp_len_alg1/1)
  end

  def run2(file \\ nil) do
    run(file, &uncomp_len_alg2/1)
  end

  defp run(file, f) do
    input =
      (file || CF.default_input_path(__MODULE__))
      |> File.read!
      |> String.replace(~r/[ \t\n]/, "")
    f.(input)
  end

  def uncomp_len_alg1(s), do: do_uncomp_len_alg1(s, 0)

  defp do_uncomp_len_alg1("", len), do: len
  defp do_uncomp_len_alg1(<<?(, _ :: binary>> = s, len) do
    [match, num_chars, repeats] = Regex.run(~r{\((\d+)x(\d+)\)}, s)
    num_chars = String.to_integer(num_chars)
    repeats = String.to_integer(repeats)
    match_len = String.length(match)
    {_, rest} = split_at(s, match_len + num_chars)
    do_uncomp_len_alg1(rest, len + repeats * num_chars)
  end
  defp do_uncomp_len_alg1(<<_, rest :: binary>>, len) do
    do_uncomp_len_alg1(rest, len + 1)
  end


  def uncomp_len_alg2(s), do: do_uncomp_len_alg2(s, 0)

  defp do_uncomp_len_alg2("", len), do: len
  defp do_uncomp_len_alg2(<<?(, _ :: binary>> = s, len) do
    [match, num_chars, repeats] = Regex.run(~r{\((\d+)x(\d+)\)}, s)
    num_chars = String.to_integer(num_chars)
    repeats = String.to_integer(repeats)
    match_len = String.length(match)

    {_, without_match} = split_at(s, match_len)
    {substr, rest} = split_at(without_match, num_chars)
    section_len = repeats * uncomp_len_alg2(substr)

    do_uncomp_len_alg2(rest, len + section_len)
  end
  defp do_uncomp_len_alg2(<<_, rest :: binary>>, len) do
    do_uncomp_len_alg2(rest, len+1)
  end

  def split_at(s, n), do: do_split_at(s, String.length(s), n)

  defp do_split_at(s, _slen, 0), do: {"", s}
  defp do_split_at(s, slen, slen), do: {s, ""}
  defp do_split_at(s, _slen, n) do
    <<first::binary-size(n), rest::binary>> = s
    {first, rest}
  end
end

# Y2016.Day09.run1                # => 120765
# Y2016.Day09.run2                # => 11658395076
