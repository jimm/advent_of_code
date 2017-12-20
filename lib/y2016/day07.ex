# Format of addr: {words-outside-brackets, words-inside-brackets}
defmodule Y2016.Day07 do

  use Common.File

  @input_file default_input_path()

  def run1(file \\ @input_file) do
    count_addrs(file, &supports_tls?/1)
    |> IO.puts
  end

  def run2(file \\ @input_file) do
    count_addrs(file, &supports_ssl?/1)
    |> IO.puts
  end

  def count_addrs(file, f) do
    input_lines(file)
    |> Enum.map(&parse_addrs/1)
    |> Enum.filter(f)
    |> Enum.count
  end

  defp parse_addrs(line) do
    line
    |> String.split(~r{[\[\]]}, trim: true)
    |> Enum.chunk(2, 2, [nil])
    |> Enum.reduce({[], []}, fn [w1, w2], {outies, innies} ->
         new_innies = if w2, do: [w2 | innies], else: innies
         {[w1 | outies], new_innies}
       end)
  end

  # ================ run1 helpers ================

  defp supports_tls?({outies, innies}) do
    Enum.any?(outies, &abba?/1) && !Enum.any?(innies, &abba?/1)
  end

  # Assumes `word` is made up of 8-bit chars.
  defp abba?(word) do
    any_matching?(word, 4, &abba_quad?/1)
  end

  defp abba_quad?([a, b, b, a]) when a != b, do: true
  defp abba_quad?(_), do: false

  # ================ run2 helpers ================

  defp supports_ssl?({outies, innies}) do
    abas = outies |> Enum.flat_map(&abas/1)
    babs = innies |> Enum.flat_map(&abas/1)
    length(abas) > 0 && Enum.any?(babs, fn bab ->
      [b, a, b] = bab
      Enum.member?(abas, [a, b, a])
    end)
  end

  defp abas(word) do
    chunks_matching(word, 3, &aba_tri?/1)
  end

  defp aba_tri?([a, b, a]) when a != b, do: true
  defp aba_tri?(_), do: false

  # ================ global helpers ================

  defp any_matching?(word, chunk_len, f) do
    # Less efficient than using Enum.any? directly, but good enough
    length(chunks_matching(word, chunk_len, f)) > 0
  end

  defp chunks_matching(word, chunk_len, f) do
    word
    |> to_charlist
    |> Enum.chunk(chunk_len, 1)
    |> Enum.filter(f)
  end
end

# Y2016.Day07.run1
# Y2016.Day07.run2
