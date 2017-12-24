# High-Entropy Passphrases

defmodule Y2017.Day04 do
  use Common.File

  def part1 do
    count_valid(&words_valid?/1)
  end

  def part2 do
    count_valid(&sorted_words_valid?/1)
  end

  # ================ part 1 ================

  defp words_valid?(line) do
    # Need to sort for Enum.dedup
    sorted_words =
      line
      |> String.split()
      |> Enum.sort()

    length(sorted_words) == length(Enum.dedup(sorted_words))
  end

  # ================ part 2 ================

  defp sorted_words_valid?(line) do
    # Need to sort for Enum.dedup
    sorted_dorsw =
      line
      |> String.split()
      |> Enum.map(&sort_letters/1)
      |> Enum.sort()

    length(sorted_dorsw) == length(Enum.dedup(sorted_dorsw))
  end

  defp sort_letters(word) do
    word
    |> String.split("", trim: true)
    |> Enum.sort()
    |> Enum.join()
  end

  # ================ helpers ================

  defp count_valid(f) do
    input_lines()
    |> Enum.count(f)
  end
end
