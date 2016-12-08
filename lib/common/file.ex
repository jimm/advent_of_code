defmodule Common.File do
  @doc """
  Given a module symbol or string, return a path to the default input file
  for that module, relative to the root of the project.

  ## Examples:

      iex> Common.File.default_input_path(Y2016.Day08)
      "data/y2016/day_08.txt"
  """
  def default_input_path(mod) do
    path =
      mod
      |> to_string
      |> String.downcase
      |> String.replace(".day", ".day_")
      |> String.split(".")
      |> tl
      |> Enum.join("/")
    "data/#{path}.txt"
  end

  def lines(file) do
    file
    |> File.read!
    |> String.split("\n", trim: true)
  end
end
