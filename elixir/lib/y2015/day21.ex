# RPG Simulator 20XX

defmodule Y2015.Day21 do
  alias Common.Enum, as: CE

  @run1_player_data %{
    hp: 100,
    damage: 0,
    armor: 0
  }
  @run1_player_test_data %{
    hp: 8,
    damage: 5,
    armor: 5
  }
  @run1_boss_data %{
    hp: 109,
    damage: 8,
    armor: 2
  }
  @run1_boss_test_data %{
    hp: 12,
    damage: 7,
    armor: 2
  }

  @weapons [
    %{name: "Dagger", cost: 8, damage: 4, armor: 0},
    %{name: "Shortsword", cost: 10, damage: 5, armor: 0},
    %{name: "Warhammer", cost: 25, damage: 6, armor: 0},
    %{name: "Longsword", cost: 40, damage: 7, armor: 0},
    %{name: "Greataxe", cost: 74, damage: 8, armor: 0}
  ]

  @armor_sets [
    %{name: "Leather", cost: 13, damage: 0, armor: 1},
    %{name: "Chainmail", cost: 31, damage: 0, armor: 2},
    %{name: "Splintmail", cost: 53, damage: 0, armor: 3},
    %{name: "Bandedmail", cost: 75, damage: 0, armor: 4},
    %{name: "Platemail", cost: 102, damage: 0, armor: 5}
  ]

  @rings [
    %{name: "Damage +1", cost: 25, damage: 1, armor: 0},
    %{name: "Damage +2", cost: 50, damage: 2, armor: 0},
    %{name: "Damage +3", cost: 100, damage: 3, armor: 0},
    %{name: "Defense +1", cost: 20, damage: 0, armor: 1},
    %{name: "Defense +2", cost: 40, damage: 0, armor: 2},
    %{name: "Defense +3", cost: 80, damage: 0, armor: 3}
  ]

  def run1 do
    equipment_combinations()
    |> Enum.filter(fn equipment ->
      fight(equip(@run1_player_data, equipment), @run1_boss_data) == :player
    end)
    |> Enum.map(fn equipment ->
      equipment |> Enum.map(& &1.cost) |> Enum.sum()
    end)
    |> Enum.min()
  end

  def run1_test do
    winner = fight(@run1_player_test_data, @run1_boss_test_data)

    if winner == :player do
      IO.puts("ok")
    else
      IO.puts("error: winner should be :player, was :boss")
    end
  end

  def run2 do
    equipment_combinations()
    |> Enum.filter(fn equipment ->
      fight(equip(@run1_player_data, equipment), @run1_boss_data) == :boss
    end)
    |> Enum.map(fn equipment ->
      equipment |> Enum.map(& &1.cost) |> Enum.sum()
    end)
    |> Enum.max()
  end

  defp equipment_combinations do
    item_to_list = &[&1]
    weapon_combinations = Enum.map(@weapons, item_to_list)
    armor_combinations = [[]] ++ Enum.map(@armor_sets, item_to_list)
    ring_combinations = [[]] ++ Enum.map(@rings, item_to_list) ++ CE.combinations(@rings, 2)

    weapon_combinations
    |> Enum.map(fn ws ->
      armor_combinations
      |> Enum.map(fn as ->
        ring_combinations
        |> Enum.map(fn rs ->
          {(ws ++ as ++ rs) |> List.flatten()}
        end)
      end)
    end)
    |> List.flatten()
    |> Enum.map(&elem(&1, 0))
  end

  defp equip(player_data, equipment) do
    total_damage = equipment |> Enum.map(& &1.damage) |> Enum.sum()
    total_armor = equipment |> Enum.map(& &1.armor) |> Enum.sum()

    %{
      player_data
      | damage: player_data.damage + total_damage,
        armor: player_data.damage + total_armor
    }
  end

  defp fight(player, boss) do
    fight(:player, player, boss)
  end

  defp fight(_, %{hp: player_hp}, _) when player_hp <= 0 do
    :boss
  end

  defp fight(_, _, %{hp: boss_hp}) when boss_hp <= 0 do
    :player
  end

  defp fight(:player, player, boss) do
    damage = Enum.max([1, player.damage - boss.armor])
    fight(:boss, player, %{boss | hp: boss.hp - damage})
  end

  defp fight(:boss, player, boss) do
    damage = Enum.max([1, boss.damage - player.armor])
    fight(:player, %{player | hp: player.hp - damage}, boss)
  end
end
