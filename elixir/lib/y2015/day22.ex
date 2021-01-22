# Wizard Simulator 20XX

defmodule Y2015.Day21 do
  @run1_player_data %{
    hp: 50,
    mana: 500,
    damage: 0,
    armor: 0,
    active_spells: []
  }
  @run1_player_test_data %{
    hp: 10,
    mana: 250,
    damage: 0,
    armor: 0,
    active_spells: []
  }
  @run1_boss_data %{
    hp: 71,
    damage: 10
  }
  @run1_boss_test_data %{
    hp: 13,
    damage: 8
  }

  @spells [
    %{name: "Magic Missle", mana: 53, damage: 4, heal: 0, armor: 0, recharge: 0, len: 1},
    %{name: "Drain", mana: 73, damage: 2, heal: 2, armor: 0, recharge: 0, len: 1},
    %{name: "Shield", mana: 113, damage: 0, heal: 0, armor: 7, recharge: 0, len: 6},
    %{name: "Poison", mana: 173, damage: 3, heal: 0, armor: 0, recharge: 0, len: 6},
    %{name: "Recharge", mana: 229, damage: 0, heal: 0, armor: 0, recharge: 101, len: 5}
  ]
  @min_mana_needed 53

  def run1 do
  end

  def run1_test do
  end

  def run2 do
  end

  # Start the fight.
  defp fight(player, boss) do
    fight(:player, player, boss, 0, 0xFFFFFFFFFFFFFFFF)
  end

  # We've spent more mana than previously won, so bail.
  defp fight(_, _, _, mana_spent, min_win_mana_spent)
       when mana_spent > min_win_mana_spent do
    :boss
  end

  # Player is dead, boss wins.
  defp fight(_, %{hp: player_hp}, _, _, _) when player_hp <= 0 do
    :boss
  end

  # Boss is dead, player wins.
  defp fight(_, _, %{hp: boss_hp}, _, _) when boss_hp <= 0 do
    :player
  end

  # Boss's turn.
  defp fight(:boss, player, boss, mana_spent, min_win_mana_spent) do
    {new_player, new_boss} = run_all_spells(player, boss)
    damage = Enum.max([1, new_boss.damage - new_player.armor])

    fight(
      :player,
      %{new_player | hp: new_player.hp - damage},
      new_boss,
      mana_spent,
      min_win_mana_spent
    )
  end

  # Player's turn, but not enough mana to use any spell. Boss wins.
  defp fight(:player, %{mana: mana}, _, _, _) when mana < @min_mana_needed do
    :boss
  end

  # Player's turn.
  defp fight(:player, player, boss, mana_spent, min_win_mana_spent) do
    {new_player, new_boss} = run_all_spells(player, boss)
    # TODO pick spell and start it
    # FIXME use new_player, new_boss
    damage = Enum.max([1, new_player.damage - new_boss.armor])

    fight(
      :boss,
      new_player,
      %{new_boss | hp: new_boss.hp - damage},
      mana_spent,
      min_win_mana_spent
    )
  end

  defp run_all_spells(player, boss) do
    # FIXME
    {nil, nil}
  end

  # return spell, player, boss
  defp start_spell(spell, player, boss) do
    {
      spell,
      %{
        player
        | mana: player.mana - spell.mana,
          armor: player.armor + spell.armor,
          active_spells: [spell | player.active_spells]
      },
      boss
    }
  end

  # return spell, player, boss
  defp run_spell(spell, player, boss) do
    spell_remaining_time = spell.len - 1

    {ending_armor_mod, player_spells} =
      if spell_remaining_time == 0 do
        {spell.armor, player.active_spells.delete(spell)}
      else
        {0, player.active_spells}
      end

    {
      %{spell | len: spell_remaining_time},
      %{
        player
        | mana: player.mana + spell.recharge,
          damage: player.damage + spell.heal,
          armor: player.armor - ending_armor_mod,
          active_spells: player_spells
      },
      %{boss | hp: boss.hp - spell.damage}
    }
  end
end
