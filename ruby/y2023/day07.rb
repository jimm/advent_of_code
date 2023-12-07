#!/usr/bin/env ruby
#
# Camel Cards

require_relative '../day'
require_relative '../enumerable'

class Day07 < Day
  CARD_ORDER_PART_1 = %w[2 3 4 5 6 7 8 9 T J Q K A]
  CARD_ORDER_PART_2 = %w[J 2 3 4 5 6 7 8 9 T Q K A] # J is now Joker

  def do_part1(lines)
    do_part(lines, CARD_ORDER_PART_1, :type_score)
  end

  def do_part2(lines)
    do_part(lines, CARD_ORDER_PART_2, :type_score_using_joker)
  end

  def do_part(lines, card_order, type_score_method_sym)
    hands = parse(lines, card_order)
    hands.each { |h| h[:type_score] = send(type_score_method_sym, h[:card_values]) }
    hands = sort_by_rank(hands)
    score = 0
    hands.each_with_index do |hand, i|
      score += hand[:bid] * (i + 1)
    end
    score
  end

  private

  def type_score(card_values)
    sorted_freqs = card_values.frequencies.values.sort.reverse
    return 8 if sorted_freqs[0] == 5            # five of a kind
    return 7 if sorted_freqs[0] == 4            # four of a kind
    return 6 if sorted_freqs == [3, 2]          # full house
    return 5 if sorted_freqs == [3, 1, 1]       # three of a kind
    return 4 if sorted_freqs == [2, 2, 1]       # two pair
    return 3 if sorted_freqs == [2, 1, 1, 1]    # one pair
    return 2 if sorted_freqs == [1, 1, 1, 1, 1] # high card

    1
  end

  def type_score_using_joker(card_values)
    idx = card_values.index(0) # find the first joker
    # If there is no joker we can use the simpler logic
    return type_score(card_values) if idx.nil?

    # Plug every other value into the slot the joker is in and find the max
    # value recursively.
    new_vals = card_values.dup
    (1..12).map do |card_val|
      new_vals[idx] = card_val
      type_score_using_joker(new_vals)
    end.max
  end

  def sort_by_rank(hands)
    hands.sort do |h0, h1|
      score0 = h0[:type_score]
      score1 = h1[:type_score]
      if score0 == score1 # compare cards to find higher
        hand0 = h0[:card_values]
        hand1 = h1[:card_values]
        val = 0
        hand0.each_with_index do |card, i|
          if hand0[i] != hand1[i]
            val = hand0[i] <=> hand1[i]
            break
          end
        end
        val
      else
        score0 <=> score1
      end
    end
  end

  # Returns an array of [hand, bid] values. The hand is an array of card
  # values (not names). That makes it easier to find the high card later.
  def parse(lines, card_order)
    lines.map do |line|
      hand, bid = line.split(' ')
      hand = hand.chars.map { card_order.index(_1) }
      bid = bid.to_i
      { card_values: hand, bid: bid }
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
