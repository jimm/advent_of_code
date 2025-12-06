# Doesn't He Have Intern-Elves For This?

defmodule Y2015.Day05 do
  @forbidden ~r{(ab)|(cd)|(pq)|(xy)}
  @doubled ~r{(.)\1}
  @three_vowels ~r{[aeiou].*[aeiou].*[aeiou]}
  @pairs_doubled ~r{(..).*\1}
  @repeat_one_between ~r{(.).\1}

  def part1(_ctx, lines) do
    count_nice(lines, fn s ->
      !Regex.match?(@forbidden, s) && Regex.match?(@doubled, s) && Regex.match?(@three_vowels, s)
    end)
  end

  def part2(_ctx, lines) do
    count_nice(lines, fn s ->
      Regex.match?(@pairs_doubled, s) && Regex.match?(@repeat_one_between, s)
    end)
  end

  def count_nice(lines, f) do
    lines
    |> Stream.map(&String.trim/1)
    |> Stream.filter(fn s -> f.(s) end)
    |> Enum.to_list()
    |> length
  end
end
