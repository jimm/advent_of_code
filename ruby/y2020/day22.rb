#!/usr/bin/env ruby
#
# Crab Combat

require 'set'
require_relative '../day'

class Player
  # The deck of cards is a simple array, with the top card at deck[0].
  attr_reader :deck

  def initialize
    @deck = []
    @deck_stack = []
  end

  def draw_card
    @deck.shift
  end

  def append_cards(cards)
    @deck += cards
  end

  def can_recurse?(n)
    @deck.length >= n
  end

  def lost?
    @deck.empty?
  end

  def push_deck(n)
    new_deck = @deck.take(n)
    @deck_stack.push(@deck)
    @deck = new_deck
  end

  def pop_deck
    @deck = @deck_stack.pop
  end

  def score
    total = 0
    @deck.reverse.each_with_index { |card, i| total += card * (i + 1) }
    total
  end
end

class Day22 < Day
  def part1
    winner = do_part1
    puts(winner.score)
  end

  def part1_tests
    run_one_test(306) { |_| do_part1.score }
  end

  def do_part1
    player1, player2 = parse
    while true
      card1 = player1.draw_card
      card2 = player2.draw_card
      winner = card1 > card2 ? player1 : player2
      cards = winner == player1 ? [card1, card2] : [card2, card1]
      winner.append_cards(cards)

      other = winner == player1 ? player2 : player1
      return winner if other.lost?
    end
  end

  def part2
    player1, player2 = parse
    winner = do_part2(player1, player2)
    puts winner.score
  end

  def part2_tests
    run_one_test(291) do |_|
      player1, player2 = parse
      do_part2(player1, player2).score
    end
  end

  def do_part2(player1, player2)
    previous_rounds = Set.new
    while true
      # If these hands have been seen before, player 1 wins.
      round_start = [player1.score, player2.score]
      return player1 if previous_rounds.include?(round_start)

      previous_rounds << round_start

      card1 = player1.draw_card
      card2 = player2.draw_card
      if player1.can_recurse?(card1) && player2.can_recurse?(card2)
        # Play a recursive game
        player1.push_deck(card1)
        player2.push_deck(card2)
        # We can take a shortcut. If player1 has the highest card, they are
        # guaranteed to win.
        max_card = (player1.deck + player2.deck).max
        winner = if player1.deck.include?(max_card)
                   player1
                 else
                   do_part2(player1, player2)
                 end
        player1.pop_deck
        player2.pop_deck
      else
        # A normal hand
        winner = card1 > card2 ? player1 : player2
      end

      # Winning card order is determined by who the winner was, not the
      # value of the cards.
      cards = winner == player1 ? [card1, card2] : [card2, card1]
      winner.append_cards(cards)

      other = winner == player1 ? player2 : player1
      return winner if other.lost?
    end
  end

  def parse
    players = []
    player = nil
    data_lines(1).each do |line|
      if line =~ /Player (\d+)/
        player = Player.new
        players << player
      else
        player.deck << line.to_i
      end
    end
    players
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
