#! /usr/bin/env elixir

# Rooms stored as {name, sector, checksum}
defmodule Y2016.Day04 do
  use Common.File

  @input_file default_input_path()
  @room_regex ~r/^(([a-z]+-)+)(\d+)\[([a-z]+)\]$/

  def run1(file \\ @input_file) do
    read_file(file)
    |> Enum.filter(&real?/1)
    |> Enum.map(fn {_, sector, _} -> sector end)
    |> Enum.sum()
    |> IO.puts()
  end

  def run2(file \\ @input_file) do
    {_, sector, _} =
      read_file(file)
      |> Enum.map(fn {_, sector, checksum} = room ->
        {decrypt_name(room), sector, checksum}
      end)
      |> Enum.filter(fn {name, _, _} -> name == "northpole object storage" end)
      |> hd

    IO.puts(sector)
  end

  defp read_file(file) do
    input_lines(file)
    |> Enum.map(&parse_room/1)
  end

  defp parse_room(s) do
    [_, name_parts, _, sector, checksum] = Regex.run(@room_regex, s)
    {name_parts, String.to_integer(sector), checksum}
  end

  defp real?({name, _, checksum}) do
    freqs =
      name
      |> String.replace("-", "")
      |> String.split("", trim: true)
      |> Enum.reduce(%{}, fn ch, m ->
        Map.put(m, ch, Map.get(m, ch, 0) + 1)
      end)

    checksum_len = String.length(checksum)

    top_n =
      freqs
      |> Enum.sort_by(fn {ch, count} ->
        count * 1000 + (36 - String.to_integer(ch, 36))
      end)
      |> Enum.reverse()
      |> Enum.take(checksum_len)
      |> Enum.map(fn {ch, _} -> ch end)

    Enum.join(top_n, "") == checksum
  end

  defp decrypt_name({name, sector, _}) do
    name
    |> String.to_charlist()
    |> Enum.map(fn ch ->
      case ch do
        ?- -> 32
        n -> clamp(n + sector)
      end
    end)
    |> to_string
    |> String.trim()
  end

  defp clamp(n), do: Integer.mod(n - ?a, 26) + ?a
end

# Y2016.Day04.run1
# Y2016.Day04.run2
