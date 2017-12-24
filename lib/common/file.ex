defmodule Common.File do
  defmacro __using__(_) do
    quote do
      import Common.File
    end
  end

  defmacro default_input_path do
    quote do
      default_input_path(__MODULE__)
    end
  end

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
      |> String.downcase()
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

  defmacro input_lines() do
    quote do
      __MODULE__ |> default_input_path |> input_lines
    end
  end

  def input_lines(file) do
    file
    |> File.read!()
    |> String.split("\n", trim: true)
  end
end
