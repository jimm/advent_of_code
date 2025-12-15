# ================ Factory ================

defmodule Y2025.Day10.Machine do
  require Integer
  alias Common.Enum, as: CE

  defstruct [:lights, :wiring, :joltage]

  def min_config_press_count(machine) do
    Stream.from_index(1)
    |> Stream.drop_while(fn i -> not configurable_with_n_presses?(machine, i) end)
    |> Enum.take(1)
    |> hd
  end

  defp configurable_with_n_presses?(machine, n) do
    on_indexes =
      0..(length(machine.lights) - 1)
      |> Enum.filter(fn i -> Enum.at(machine.lights, i) end)

    machine.wiring
    |> CE.combinations(n)
    |> Enum.any?(fn schematic -> configures?(on_indexes, schematic) end)
  end

  # Returns true of the button presses in `schematic` can result in the same
  # `on_indexes` as lights.
  defp configures?(on_indexes, schematic) do
    on_after_presses_indexes =
      schematic
      |> List.flatten()
      |> Enum.frequencies()
      |> Map.filter(fn {_, v} -> Integer.is_odd(v) end)
      |> Map.keys()
      |> Enum.sort()

    length(on_indexes) == length(on_after_presses_indexes) and
      on_indexes == on_after_presses_indexes
  end
end

defmodule Y2025.Day10 do
  alias Y2025.Day10.Machine

  def part1(_ctx, lines) do
    machines_from_lines(lines)
    |> Enum.map(fn m -> Machine.min_config_press_count(m) end)
    |> Enum.sum()
  end

  def part2(_ctx, lines) do
    IO.puts(lines)
  end

  # ================ helpers ================

  defp machines_from_lines(lines) do
    lines
    |> Enum.map(fn line ->
      # Regex.run returns named captures in alphabetical order
      [joltage, lights, wiring] =
        Regex.run(~r/\[(?<lights>[.#]+)\](?<wiring>.*?) {(?<joltage>\d+(,\d+)*)}/, line,
          capture: :all_names
        )

      lights = lights |> String.codepoints() |> Enum.map(&(&1 == "#"))
      wiring = Regex.replace(~r/[\(\)]/, wiring, "")

      wiring =
        wiring
        |> String.trim()
        |> String.split(" ")
        |> Enum.map(fn nums ->
          nums
          |> String.split(",")
          |> Enum.map(&String.to_integer/1)
        end)

      joltage = joltage |> String.split(",") |> Enum.map(&String.to_integer/1)
      %Machine{lights: lights, wiring: wiring, joltage: joltage}
    end)
  end
end
