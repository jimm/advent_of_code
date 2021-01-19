# Like a GIF For Your Yard

defmodule Y2015.Day18 do
  use Common.File

  @input_file default_input_path()
  @on ?#
  @off ?.

  @moduledoc """
  The state a light should have next is based on its current state (on or
  off) plus the number of neighbors that are on:

  - A light which is on stays on when 2 or 3 neighbors are on, and turns off
    otherwise.

  - A light which is off turns on if exactly 3 neighbors are on, and stays
    off otherwise.

  - All of the lights update simultaneously; they all consider the same
    current state before moving to the next.

  Those are, of course, the rules for Conway's Game of Life.

  A board is a list of char lists.
  """

  def run1(steps \\ 100, input_file \\ @input_file) do
    cells = read_board(input_file)
    life(cells, steps, &life_rules/5)
  end

  @doc """
  Four corner lights are stuck.

      iex> Y2015.Day18.stuck_lights(5, "test.txt")
      17
  """
  # stuck lights
  def run2(steps \\ 100, input_file \\ @input_file) do
    cells =
      read_board(input_file)
      |> turn_on_corner_lights

    life(cells, steps, &stuck_life_rules/5)
  end

  def life(cells, steps, cell_rules) do
    board = {cells, cells |> length, cells |> hd |> length}

    {final_cells, _, _} =
      1..steps
      |> Enum.reduce(board, fn _, b -> next_board(b, cell_rules) end)

    final_cells
    |> List.flatten()
    |> Enum.filter(fn c -> c == @on end)
    |> length
  end

  defp read_board(input_file) do
    File.stream!(input_file)
    |> Enum.map(&read_board_line/1)
  end

  defp read_board_line(line) do
    line
    |> String.trim()
    |> String.to_charlist()
  end

  defp next_board({_, rows, cols} = board, cell_rules) do
    cells =
      for row <- 0..(rows - 1),
          col <- 0..(cols - 1) do
        next_state(board, row, col, cell_rules)
      end
      |> Enum.chunk_every(cols)

    {cells, rows, cols}
  end

  defp next_state(board, row, col, cell_rules) do
    state = at(board, row, col)

    number_on_neighbors =
      neighbors_of(board, row, col)
      |> Enum.filter(&(&1 == @on))
      |> length

    cell_rules.(board, row, col, state, number_on_neighbors)
  end

  defp life_rules(_, _, _, state, number_on_neighbors) do
    case {state, number_on_neighbors} do
      {@on, 2} -> @on
      {@on, 3} -> @on
      {@off, 3} -> @on
      _ -> @off
    end
  end

  defp stuck_life_rules({_, rows, cols}, row, col, _, _)
       when (row == 0 or row == rows - 1) and (col == 0 or col == cols - 1) do
    @on
  end

  defp stuck_life_rules(_, _, _, state, number_on_neighbors) do
    life_rules(nil, nil, nil, state, number_on_neighbors)
  end

  defp at({_, rows, cols}, row, col) when row < 0 or col < 0 or row >= rows or col >= cols,
    do: nil

  defp at({cells, _, _}, row, col) do
    cells
    |> Enum.drop(row)
    |> hd
    |> Enum.drop(col)
    |> hd
  end

  def neighbors_of(board, row, col) do
    [
      at(board, row - 1, col - 1),
      at(board, row - 1, col),
      at(board, row - 1, col + 1),
      at(board, row, col - 1),
      at(board, row, col + 1),
      at(board, row + 1, col - 1),
      at(board, row + 1, col),
      at(board, row + 1, col + 1)
    ]
  end

  # Not efficient, but good enough
  def turn_on_corner_lights(cells) do
    [cells |> hd |> replace_first_and_last] ++
      (cells |> all_but_first_and_last) ++
      [cells |> Enum.reverse() |> hd |> replace_first_and_last]
  end

  def replace_first_and_last(row) do
    [@on] ++ all_but_first_and_last(row) ++ [@on]
  end

  def all_but_first_and_last(l) do
    l |> Enum.drop(1) |> Enum.reverse() |> Enum.drop(1) |> Enum.reverse()
  end
end
