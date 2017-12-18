# Recursive Circus

defmodule Y2017.Day07.Node do
  defstruct name: "", weight: 0, subtree_weight: 0, children: [], parent_name: ""
end

defmodule Y2017.Day07 do
  alias Common.File, as: CF
  alias Y2017.Day07.Node

  def part1 do
    root = read_tree()
    root.name
  end

  # 6457 is too high
  def part2 do
    root = read_tree() |> set_subtree_weights
    misfit = find_misfit(root)
    correct_weight_for(misfit, root)
  end

  # ================ part 2 ================

  defp set_subtree_weights(node) do
    %{node |
      subtree_weight: subtree_weight(node),
      children: node.children |> Enum.map(&set_subtree_weights/1)}
  end

  defp subtree_weight(node) do
    node.weight + (
      node.children
      |> Enum.map(&subtree_weight/1)
      |> Enum.sum
    )
  end

  defp find_misfit(%Node{children: []}) do
    nil
  end
  defp find_misfit(node) do
    child_weights = Enum.map(node.children, &(&1.subtree_weight))
    if balanced?(child_weights) do
      node
    else
      weight_freqs =
        node.children
        |> Enum.reduce(%{}, fn(ch, counts) ->
             Map.update(counts, ch.subtree_weight, 1, &(&1 + 1))
           end)
      different_weight =
        weight_freqs
        |> Map.keys
        |> Enum.find(&(Map.get(weight_freqs, &1) == 1))
      node.children
      |> Enum.find(&(&1.subtree_weight == different_weight))
      |> find_misfit
    end
  end

  defp correct_weight_for(node, root) do
    ideal_weight = any_sibling(node, root).subtree_weight
    sum_child_weights =
      node.children
      |> Enum.map(&(&1.subtree_weight))
      |> Enum.sum
    ideal_weight - sum_child_weights
  end

  defp any_sibling(node, root) do
    parent = find_node_named(node.parent_name, root)
    parent.children |> Enum.find(&(&1 != node))
  end

  defp find_node_named(name, node = %Node{name: name}) do
    node
  end
  defp find_node_named(name, node) do
    node.children
    |> Enum.find_value(&(find_node_named(name, &1)))
  end

  defp balanced?([]), do: true
  defp balanced?([_]), do: true
  defp balanced?(weights) do
    (weights |> Enum.sort |> Enum.uniq |> length) == 1
  end

  # ================ helpers ================

  # Returns root node
  defp read_tree do
    node_map =
      __MODULE__
      |> CF.default_input_path
      |> CF.lines
      |> Enum.map(&parse_line/1)
      |> Enum.reduce(%{}, fn(n, m) -> Map.put(m, n.name, n) end)
    root_name =
      node_map
      |> Map.keys
      |> Enum.find(fn(name) -> !has_parent?(name, Map.values(node_map)) end)
    build_tree(root_name, nil, node_map)
  end

  defp has_parent?(_, []) do
    false
  end
  defp has_parent?(name, [%Node{children: kid_names} | t]) do
    if Enum.member?(kid_names, name), do: true, else: has_parent?(name, t)
  end

  defp build_tree(name, parent_name, node_map) do
    n = node_map[name]
    %{n |
      parent_name: parent_name,
      children: n.children |> Enum.map(&(build_tree(&1, n.name, node_map)))}
  end

  defp parse_line(line) do
    [name, weight_in_parens | remainder] = String.split(line)
    weight =
      weight_in_parens
      |> String.replace(~r/[()]/, "")
      |> String.to_integer
    if length(remainder) > 0 do
      child_names =
        remainder
        |> tl
        |> Enum.map(&(String.replace_trailing(&1, ",", "")))
      %Node{name: name, weight: weight, children: child_names}
    else
      %Node{name: name, weight: weight}
    end
  end
end
