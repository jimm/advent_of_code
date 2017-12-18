# Stream Processing

defmodule Y2017.Day09 do

  alias Common.File, as: CF

  defmodule State do
    defstruct state: :group, score: 0, scores: [], comment_char_count: 0
  end

  @tests [
    {"{}", 1},
    {"{{{}}}", 6},
    {"{{},{}}", 5},
    {"{{{},{},{{}}}}", 16},
    {"{<a>,<a>,<a>,<a>}", 1},
    {"{{<ab>},{<ab>},{<ab>},{<ab>}}", 9},
    {"{{<!!>},{<!!>},{<!!>},{<!!>}}", 9},
    {"{{<a!>},{<a!>},{<a!>},{<ab>}}", 3}
  ]

  def part1 do
    state =
      read_stream()
      |> Enum.reduce(%State{}, fn(ch, s) -> process(ch, s) end)
    Enum.sum(state.scores)
  end

  # 8187 too high
  def part2 do
    chars = read_stream()
    state = chars |> Enum.reduce(%State{}, fn(ch, s) -> process(ch, s) end)
    state.comment_char_count
  end

  def test do
    Enum.map(@tests, fn({str, expected}) ->
      state =
        str
        |> String.to_charlist
        |> Enum.reduce(%State{}, fn(ch, s) -> process(ch, s) end)
      Enum.sum(state.scores) == expected
    end)
  end

  defp read_stream() do
    __MODULE__
    |> CF.default_input_path
    |> File.read!
    |> String.to_charlist
  end

  defp process(?<, s = %State{state: state}) when state == :group do
    %{s | state: :comment}
  end
  defp process(?>, s = %State{state: state}) when state == :comment do
    %{s | state: :group}
  end
  defp process(?{, s = %State{state: state}) when state == :group do
    %{s | score: s.score + 1}
  end
  defp process(?}, s = %State{state: state}) when state == :group do
    %{s | scores: [s.score | s.scores], score: s.score - 1}
  end
  defp process(?,, s = %State{state: state}) when state == :group do
    s
  end
  defp process(?!, s = %State{state: state}) when state == :comment do
    %{s | state: :skip}
  end
  defp process(_, s = %State{state: state}) when state == :skip do
    %{s | state: :comment}
  end
  defp process(_, s = %State{state: state}) when state == :comment do
    %{s | comment_char_count: s.comment_char_count + 1}
  end
  defp process(_, s) do
    s
  end
end
