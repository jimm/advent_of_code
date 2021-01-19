# Doesn't He Have Intern-Elves For This?

defmodule Y2015.Day05 do
  use Common.File

  @forbidden ~r{(ab)|(cd)|(pq)|(xy)}
  @doubled ~r{(.)\1}
  @three_vowels ~r{[aeiou].*[aeiou].*[aeiou]}
  @pairs_doubled ~r{(..).*\1}
  @repeat_one_between ~r{(.).\1}

  def run1 do
    count_nice(fn s ->
      !Regex.match?(@forbidden, s) && Regex.match?(@doubled, s) && Regex.match?(@three_vowels, s)
    end)
  end

  def run2 do
    count_nice(fn s ->
      Regex.match?(@pairs_doubled, s) && Regex.match?(@repeat_one_between, s)
    end)
  end

  def count_nice(f) do
    input_lines()
    |> Stream.map(&String.trim/1)
    |> Stream.filter(fn s -> f.(s) end)
    |> Enum.to_list()
    |> length
  end
end
