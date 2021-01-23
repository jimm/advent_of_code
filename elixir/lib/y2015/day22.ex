# Wizard Simulator 20XX

defmodule Y2015.Day22 do
  @run1_player_data %{
    hp: 50,
    mana: 500,
    damage: 0,
    armor: 0,
    active_spells: []
  }
  # @run1_player_test_data %{
  #   hp: 10,
  #   mana: 250,
  #   damage: 0,
  #   armor: 0,
  #   active_spells: []
  # }
  @run1_boss_data %{
    hp: 71,
    damage: 10
  }

  # @run1_boss_test_data %{
  #   hp: 13,
  #   damage: 8
  # }

  @spells [
    %{name: "Magic Missle", mana: 53, damage: 4, heal: 0, armor: 0, recharge: 0, len: 1},
    %{name: "Drain", mana: 73, damage: 2, heal: 2, armor: 0, recharge: 0, len: 1},
    %{name: "Shield", mana: 113, damage: 0, heal: 0, armor: 7, recharge: 0, len: 6},
    %{name: "Poison", mana: 173, damage: 3, heal: 0, armor: 0, recharge: 0, len: 6},
    %{name: "Recharge", mana: 229, damage: 0, heal: 0, armor: 0, recharge: 101, len: 5}
  ]
  @min_mana_needed 53

  def run1 do
    fight(@run1_player_data, @run1_boss_data)
  end

  def run1_test do
  end

  def run2 do
  end

  # Start the fight.
  defp fight(player, boss) do
    fight(:player, player, boss, 0)
  end

  # # We've spent more mana than previously won, so bail.
  # defp fight(_, _, _, mana_spent)
  #      when mana_spent > min_win_mana_spent do
  #   {:boss, 0}
  # end

  # Player is dead, boss wins.
  defp fight(_, %{hp: player_hp}, _, _) when player_hp <= 0 do
    {:boss, 0}
  end

  # Boss is dead, player wins.
  defp fight(_, _, %{hp: boss_hp}, mana_spent) when boss_hp <= 0 do
    {:player, mana_spent}
  end

  # Boss's turn. Returns {winner, mana_spent}
  defp fight(:boss, player, boss, mana_spent) do
    {new_player, new_boss} = run_all_spells(player, boss)
    damage = Enum.max([1, new_boss.damage - new_player.armor])

    fight(
      :player,
      %{new_player | hp: new_player.hp - damage},
      new_boss,
      mana_spent
    )
  end

  # Player's turn, but not enough mana to use any spell. Boss wins.
  defp fight(:player, %{mana: mana}, _, _) when mana < @min_mana_needed do
    {:boss, 0}
  end

  # Player's turn.
  defp fight(:player, player, boss, mana_spent) do
    {new_player, new_boss} = run_all_spells(player, boss)

    winner_tuples =
      @spells
      |> Enum.filter(&(&1.mana <= new_player.mana))
      |> Enum.map(fn spell ->
        spell_new_player = start_spell(spell, new_player)
        damage = Enum.max([1, spell_new_player.damage])

        fight(
          :boss,
          spell_new_player,
          %{boss | hp: new_boss.hp - damage},
          mana_spent + spell.mana
        )
      end)
      |> Enum.filter(fn {winner, _} -> winner == :player end)

    if winner_tuples == [] do
      {:boss, 0}
    else
      winner_tuples |> Enum.min_by(fn {:player, mana_spent} -> mana_spent end)
    end
  end

  # Run all spells and return possibly modified {player, boss}
  defp run_all_spells(player, boss) do
    {new_player, new_boss, new_active_spells} =
      player.active_spells
      |> Enum.reduce({player, boss, []}, fn spell, {p, b, spells} ->
        {modified_spell, modified_player, modified_boss} = run_spell(spell, p, b)

        if modified_spell.len == 0 do
          {modified_player, modified_boss, spells}
        else
          {modified_player, modified_boss, [modified_spell | spells]}
        end
      end)

    {%{new_player | active_spells: new_active_spells}, new_boss}
  end

  # return player
  defp start_spell(spell, player) do
    %{
      player
      | mana: player.mana - spell.mana,
        armor: player.armor + spell.armor,
        active_spells: [spell | player.active_spells]
    }
  end

  # return spell, player, boss
  defp run_spell(spell, player, boss) do
    spell_remaining_time = spell.len - 1

    {ending_armor_mod, player_spells} =
      if spell_remaining_time == 0 do
        {spell.armor, List.delete(player.active_spells, spell)}
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
