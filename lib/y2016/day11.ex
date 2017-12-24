defmodule Y2016.Day11 do
  alias Common.Set, as: CS

  @too_many_steps 999_999_999

  # This comes from reading the input file. I could write the code to read
  # it, but it's not worth it.
  @floors [
    # floor 1
    [{:t, :gen}, {:t, :mchip}, {:p, :gen}, {:s, :gen}],
    # floor 2
    [{:p, :gen}, {:s, :mchip}],
    # floor 3
    [{:o, :gen}, {:o, :mchip}, {:r, :gen}, {:r, :mchip}],
    # floor 4
    []
  ]

  @test_floors [[{:h, :mchip}, {:l, :mchip}], [{:h, :gen}], [{:l, :gen}], []]

  def test_floors, do: @test_floors

  def run1(floors \\ @floors) do
    floors
    |> num_steps_to_solve(1, [], 0)
  end

  def num_steps_to_solve(_, _, _, steps) when steps >= 32 do
    # This is a sanity check that depends on an incorrect previous solution
    32
    # raise "too many steps"
  end

  def num_steps_to_solve(floors, elevator_floor, moves, steps) do
    # DEBUG
    IO.puts("\nnum_steps_to_solve, step #{steps}")
    # print_floors(floors, elevator_floor)
    if solved?(floors, elevator_floor) do
      steps
    else
      ms = possible_moves(floors, elevator_floor, moves)

      if Enum.empty?(ms) do
        @too_many_steps
      else
        ms
        |> Enum.map(fn {new_floors, new_elevator_floor} ->
          # DEBUG
          {new_floors, new_elevator_floor} |> IO.inspect(label: "next")

          num_steps_to_solve(
            new_floors,
            new_elevator_floor,
            [{new_floors, new_elevator_floor} | moves],
            steps + 1
          )
        end)
        |> Enum.min()
      end
    end
  end

  defp possible_moves(floors, elevator_floor, moves) do
    floor = floor_contents(floors, elevator_floor)
    # DEBUG
    loadings =
      (CS.combinations(floor, 2) ++ CS.combinations(floor, 1))
      |> IO.inspect(label: "loadings elev floor #{elevator_floor}")

    next_floors = adjacent_floors(elevator_floor)
    # no repeats
    for loading <- loadings,
        next_floor <- next_floors do
      {loading, next_floor}
    end
    |> Enum.filter(fn {loading, next_floor} ->
      legal_move?(floors, elevator_floor, loading, next_floor)
    end)
    |> Enum.map(&make_move(floors, elevator_floor, &1))
    |> Enum.filter(fn mov -> !Enum.member?(moves, mov) end)
  end

  defp solved?([_, [], [], []], 4), do: true
  defp solved?(_, _), do: false

  defp floor_contents(floors, elevator_floor) do
    Enum.at(floors, elevator_floor - 1)
  end

  defp adjacent_floors(1), do: [2]
  defp adjacent_floors(2), do: [1, 3]
  defp adjacent_floors(3), do: [2, 4]
  defp adjacent_floors(4), do: [3]

  defp legal_move?(floors, elevator_floor, loading, next_floor) do
    old_floor = floor_contents(floors, elevator_floor) -- loading
    new_floor = floor_contents(floors, next_floor) ++ loading
    legal_floor?(old_floor) && legal_floor?(new_floor)
  end

  defp legal_floor?(floor) do
    {chips, generators} = Enum.split_with(floor, &chip?/1)

    cond do
      Enum.empty?(chips) -> true
      Enum.empty?(generators) -> true
      chips |> Enum.all?(fn chip -> has_generator?(floor, chip) end) -> true
      true -> false
    end
  end

  defp chip?({_, :mchip}), do: true
  defp chip?(_), do: false

  defp has_generator?(floor, {element, :mchip}) do
    Enum.member?(floor, {element, :gen})
  end

  defp make_move(floors, elevator_floor, {loading, next_floor}) do
    new_floors =
      floors
      |> Enum.with_index()
      |> Enum.map(fn {floor, floor_num} ->
        case floor_num + 1 do
          ^elevator_floor ->
            # remove loading from this floor, put on elevator
            floor -- loading

          ^next_floor ->
            # remove loading from elevator, add to this floor
            floor ++ loading

          _ ->
            floor
        end
      end)

    {new_floors, next_floor}
  end

  defp print_floors(floors, elevator_floor) do
    [4, 3, 2, 1]
    |> Enum.each(fn i ->
      floor = floor_contents(floors, i)
      e = if i == elevator_floor, do: "E ", else: "  "

      things =
        floor
        |> Enum.map(fn {elem, type} ->
          t = if type == :mchip, do: "M", else: "G"
          "#{elem |> to_string |> String.upcase()}#{t}"
        end)

      IO.puts("F#{i}: #{e} #{Enum.join(things, " ")}")
    end)
  end
end

# F4: .. .. .. .. .. .. .. .. .. ..
# F3: .. .. .. .. .. .. MG MM RG RM
# F2: .. .. .. PM .. SM .. .. .. ..
# F1: TG TM PG .. SG .. .. .. .. ..

#
# 15 is too low
# 24 is wrong
# 32 is too high

# Y2016.Day11.run1(@test_floors)
# #=>
