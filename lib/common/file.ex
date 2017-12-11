defmodule Common.File do
  @doc """
  Given a module atom or string, return a path to the default input file for
  that module, relative to the root of the project.

  ## Examples:

      iex> Common.File.default_input_path(Y2016.Day08)
      "data/y2016/day_08.txt"

      iex> Common.File.default_input_path(Y2016.Day08, 2)
      "data/y2016/day_08_part2.txt"
  """
  def default_input_path(mod) do
    path =
      mod
      |> to_string
      |> String.downcase
      |> String.split(".")
      |> tl
      |> Enum.join("/")
    "data/#{path}.txt"
  end

  def default_input_path(mod, part) do
    path = default_input_path(mod)
    part_str = if part, do: "_part#{part}", else: ""
    path |> String.replace(".txt", "#{part_str}.txt")
  end

  def lines(file) do
    file
    |> File.read!
    |> String.split("\n", trim: true)
  end
end
