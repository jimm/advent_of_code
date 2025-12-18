# ================ Factory ================

defmodule Y2025.Day10.Machine do
  require Integer
  alias Common.Enum, as: CE

  # `max_presses` is used when passing the machine around during joltage
  # calculations.
  defstruct [:lights, :wiring, :joltage, :max_presses]

  def min_config_press_count(machine) do
    Stream.from_index(1)
    |> Stream.drop_while(fn i -> not configurable_with_n_presses?(machine, i) end)
    |> Enum.take(1)
    |> hd
  end

  def min_joltage_press_count(machine) do
    max_presses =
      machine.wiring
      |> Enum.reduce(%{}, fn buttons, map ->
        # For buttons, find the max number of times they can all be pressed
        # before exceeding each button's corresponding machine.joltage value
        Map.put(
          map,
          buttons,
          buttons
          |> Enum.map(fn button ->
            val = Enum.at(machine.joltage, button)
            if val == nil, do: 0, else: val
          end)
          |> Enum.min()
        )
      end)

    machine = %{machine | max_presses: max_presses}
    min_presses_to_joltage(machine, max_presses)
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

  # Returns the min number of schematics presses needed to achieve
  # `machine.joltage`. Returns nil if it's not possible.
  defp min_presses_to_joltage(machine, max_presses) do
    min_presses(
      machine,
      max_presses,
      machine.wiring,
      0,
      List.duplicate(0, length(machine.joltage))
    )
  end

  # Returns the total number of schematics presses needed to achieve
  # `machine.joltage`, recursively. Returns nil if it's not possible.
  #
  # `machine.max_presses` is an optimization: a pre-calculated map from
  # wiring schematics (e.g. [3, 5]) to max number of times it can be pressed
  # before one of the joltages will be exceeded.
  defp min_presses(machine, max_presses, schematics, num_presses, curr_joltage) do
    cond do
      machine.joltage == curr_joltage ->
        num_presses

      length(schematics) == 0 ->
        nil

      curr_joltage_too_big?(curr_joltage, machine.joltage) ->
        nil

      true ->
        [buttons | rest] = schematics

        # zero presses of the first set of buttons
        n = min_presses(machine, max_presses, rest, num_presses, curr_joltage)
        num_presses_needed = if(n == nil, do: [], else: [n])

        # Try pressing the first schematic 1..max times and see if the
        # remaining schematics will get us to our solution.
        {num_presses_needed, _, _} =
          1..machine.max_presses[buttons]
          |> Enum.reduce({num_presses_needed, num_presses, curr_joltage}, fn _, {npn, np, curr} ->
            # build new joltage obtained after pressing buttons
            new_joltage = press(buttons, curr)
            n = min_presses(machine, max_presses, rest, np + 1, new_joltage)
            {if(n == nil, do: npn, else: [n | npn]), np + 1, new_joltage}
          end)

        case num_presses_needed do
          [] -> nil
          vals -> Enum.min(vals)
        end
    end
  end

  # Returns true if any of the values in curr_joltage is greater than the
  # desired joltage.
  defp curr_joltage_too_big?(curr_joltage, joltage) do
    curr_joltage
    |> Enum.zip(joltage)
    |> Enum.any?(fn {curr, j} -> curr > j end)
  end

  # Presses `buttons` with `curr_joltage` and returns a new joltage array
  defp press(buttons, curr_joltage) do
    buttons
    |> Enum.reduce(curr_joltage, fn button, curr ->
      val = Enum.at(curr, button)
      List.replace_at(curr, button, val + 1)
    end)
  end

  # Returns true if all of the buttons needed by @joltage are included in
  # `schematic`.
  def covers_joltage_slots?(machine, schematic) do
    buttons_in_schematic =
      schematic |> List.flatten() |> MapSet.new() |> MapSet.to_list() |> Enum.sort()

    nonzero_schematic_indexes =
      machine.joltage
      |> Enum.with_index()
      |> Enum.map(fn {joltage, i} -> if joltage != 0, do: i, else: nil end)
      |> Enum.filter(& &1)
      |> Enum.sort()

    buttons_in_schematic == nonzero_schematic_indexes
  end
end

defmodule Y2025.Day10 do
  alias Y2025.Day10.Machine

  def part1(_ctx, lines) do
    machines_from_lines(lines)
    |> Enum.map(&Machine.min_config_press_count/1)
    |> Enum.sum()
  end

  def part2(_ctx, lines) do
    machines_from_lines(lines)
    |> Enum.map(&Machine.min_joltage_press_count/1)
    |> Enum.sum()
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
