defmodule Y2015.Day08 do
  alias Common.File, as: CF

  def run1 do
    {code_rep_len, char_count} =
      __MODULE__
      |> CF.default_input_path
      |> File.stream!
      |> Enum.map(&String.strip/1)
      |> Enum.reduce({0, 0}, fn(s, {crl, cc}) ->
        {evalled_s, _} = Code.eval_string(s)
        {crl + String.length(s), cc + String.length(evalled_s)}
      end)

    code_rep_len - char_count
  end

  def run2 do
    {code_rep_len, encoded_rep_len} =
      __MODULE__
      |> CF.default_input_path
      |> File.stream!
      |> Enum.map(&String.strip/1)
      |> Enum.reduce({0, 0}, fn(s, {crl, ecl}) ->
        len = String.length(s)
        {crl + len, ecl + len + escape_chars_count(s, 0) + 2}
      end)

    encoded_rep_len - code_rep_len
  end

  defp escape_chars_count("", len), do: len
  defp escape_chars_count(<<?">> <> rest, len) do
    # " <= workaround for Emacs font lock bug
    escape_chars_count(rest, len+1)
  end
  defp escape_chars_count(<<?\\>> <> rest, len) do
    escape_chars_count(rest, len+1)
  end
  defp escape_chars_count(<<_>> <> rest, len) do
    escape_chars_count(rest, len)
  end
end

# Y2015.Day08.run1
# # => 1350

# Y2015.Day08.run2
# # => 2085
