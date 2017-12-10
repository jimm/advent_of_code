defmodule Y2015.Day05 do
  alias Common.File, as: CF

  @forbidden ~r{(ab)|(cd)|(pq)|(xy)}
  @doubled ~r{(.)\1}
  @three_vowels ~r{[aeiou].*[aeiou].*[aeiou]}
  @pairs_doubled ~r{(..).*\1}
  @repeat_one_between ~r{(.).\1}

  def count_nice(f) do
    __MODULE__
    |> CF.default_input_path
    |> CF.lines
    |> Stream.map(&String.strip/1)
    |> Stream.filter(fn(s) -> f.(s) end)
    |> Enum.to_list
    |> length
  end

  def nice1(s) do
    !Regex.match?(@forbidden, s) &&
      Regex.match?(@doubled, s) &&
      Regex.match?(@three_vowels, s)
  end

  def nice2(s) do
    Regex.match?(@pairs_doubled, s) &&
      Regex.match?(@repeat_one_between, s)
  end
end

# Y2015.Day05.count_nice(&Y2015.Day05.nice1/1)
# Y2015.Day05.count_nice(&Y2015.Day05.nice2/1)
