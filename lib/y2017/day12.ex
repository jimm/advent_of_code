defmodule Y2017.Day12 do

  use Common.File

  def part1 do
    read_network()
    |> group_members(0)
    |> length
  end

  def part2 do
    read_network()
    |> groups()
    |> length
  end

  def test1 do
    test_network()
    |> group_members(0)
    |> length
  end

  # ================ part 1 ================

  # Return a list containing members of the group including k.
  # This is going to be inefficient (O(n^2)) but I don't care yet.
  defp group_members(m, k) do
    others = m[k]
    do_group_members(m, MapSet.new([k | others]))
  end

  # For all group members, add any targets we have not yet seen. If there
  # are none, we're done.
  defp do_group_members(m, group_members) do
    new_group_members =
      group_members
      |> Enum.flat_map(fn(memb) -> m[memb] end)
      |> Enum.concat(group_members)
      |> MapSet.new
    if Enum.count(new_group_members) == Enum.count(group_members) do
      MapSet.to_list(group_members)
    else
      do_group_members(m, new_group_members)
    end
  end

  # ================ part 2 ================

  defp groups(m) do
    groups(m, [group_members(m, 0)])
  end

  def groups(m, groups) do
    all_other_group_members = List.flatten(groups)
    non_group_member =
      Map.keys(m)
      |> Enum.find(fn(k) -> !Enum.member?(all_other_group_members, k) end)
    if non_group_member == nil do
      groups
    else
      groups(m, [group_members(m, non_group_member) | groups])
    end
  end

  # ================ testing ================

  defp test_network do
    one_way =
      ["0 <-> 2", "1 <-> 1", "2 <-> 0, 3, 4", "3 <-> 2, 4", "4 <-> 2, 3, 6",
       "5 <-> 6", "6 <-> 4, 5"]
      |> Enum.map(&parse_line/1)
      |> Map.new
    reverse_connections(one_way)
  end

  # ================ helpers ================

  defp read_network do
    one_way =
      input_lines()
      |> Enum.map(&parse_line/1)
      |> Map.new
    reverse_connections(one_way)
  end

  defp reverse_connections(one_way) do
    Enum.reduce(Map.keys(one_way), one_way, fn(key, m) ->
      vals = m[key]
      Enum.reduce(vals, m, fn(v, m) -> reverse_connection(m, v, key) end)
    end)
  end

  defp reverse_connection(m, v, k) do
    curr = m[v]
    if Enum.member?(curr, k) do
      m
    else
      Map.put(m, v, [k | curr])
    end
  end


  defp parse_line(line) do
    [node_str, others] = String.split(line, "<->") |> Enum.map(&String.trim/1)
    node = String.to_integer(node_str)
    other_nodes = others |> String.split(", ") |> Enum.map(&String.to_integer/1)
    {node, other_nodes}
  end
end
