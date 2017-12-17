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
    debug_print_tree(root)
    misfit = find_misfit(root)
    correct_weight_for(misfit, root)
  end

  # ================ part 2 ================

  defp debug_print_tree(node) do
  end

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

  defp find_misfit(node = %Node{children: []}) do
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
        |> Enum.find(fn(k) -> Map.get(weight_freqs, k) == 1 end)
      node.children
      |> Enum.find(fn(ch) -> ch.subtree_weight == different_weight end)
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
    parent.children |> Enum.find(fn(ch) -> ch != node end)
  end

  defp find_node_named(name, node = %Node{name: name}) do
    node
  end
  defp find_node_named(name, node) do
    node.children
    |> Enum.find(fn(ch) -> find_node_named(name, ch) end)
  end

  defp balanced?([]), do: true
  defp balanced?([_]), do: true
  defp balanced?(weights) do
    (weights |> Enum.sort |> Enum.uniq |> length) == 1
  end

  # ================ helpers ================

  # Returns root node
  defp read_tree do
    tree_map = read_tree_map()
    root = tree_map |> Map.values |> Enum.find(fn(node) -> node.parent_name == "" end)
    install_child_nodes(root, tree_map)
  end

  # During this process, a node's children are names (map keys), not nodes.
  defp read_tree_map do
    __MODULE__
    |> CF.default_input_path
    |> CF.lines
    |> Enum.map(&parse_line/1)
    |> Enum.reduce(%{}, fn(node, tree) ->
         existing_entry = tree[node.name]
         node_to_insert = if existing_entry, do: %{node | parent_name: existing_entry}, else: node
         tree = Map.put(tree, node.name, node_to_insert)

         # For each child, add this name as the parent of that child.
         # We end up with a modified tree.
         node.children
         |> Enum.reduce(tree, fn(n, tree) -> set_parent_name(n, node.name, tree) end)
       end)
  end

  # Set parent_name of n and return modified tree
  defp set_parent_name(n, parent_name, tree) do
    child_node = tree[n]
    if child_node do
      Map.put(tree, n, %{child_node | parent_name: parent_name})
    else
      Map.put(tree, n, parent_name)
    end
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

  defp install_child_nodes(node = %Node{children: []}, _) do
    node
  end
  defp install_child_nodes(node, tree_map) do
    child_nodes =
      node.children
      |> Enum.map(fn(child_name) ->
           child_node = tree_map[child_name]
           install_child_nodes(child_node, tree_map)
         end)
    %{node | children: child_nodes }
  end
end
