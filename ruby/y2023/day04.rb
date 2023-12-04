#!/usr/bin/env ruby
#
# Scratchcards

require_relative '../day'

class Day04 < Day
  class Card
    attr_reader :num, :winning_nums, :on_card, :num_winners

    def initialize(num, winning_nums, on_card)
      @num = num
      @winning_nums = winning_nums
      @on_card = on_card
      @num_winners = @on_card.select { @winning_nums.include?(_1) }.length
    end

    def name
      "Card #{@num}"
    end

    def to_s
      "<Card #{@num}: @num_winners = #{@num_winners}>"
    end

    def inspect
      to_s
    end
  end

  def do_part1(lines)
    cards = parse_cards(lines)
    score_part1(cards)
  end

  def do_part2(lines)
    cards = parse_cards(lines)
    score_part2(cards)
  end

  private

  def score_part1(cards)
    cards.map do |card|
      card.num_winners == 0 ? 0 : 2**(card.num_winners - 1)
    end.sum
  end

  def score_part2(cards)
    counts = Array.new(cards.length, 1)
    cards.each_with_index do |card, i|
      (1..card.num_winners).each do |j|
        counts[i + j] += counts[i]
      end
    end
    counts.sum
  end

  def num_winners(card)
    card.on_card.select { card.winning_nums.include?(_1) }.length
  end

  def parse_cards(lines)
    i = 1
    lines.map do |line|
      _, data = line.split(':')
      winning_nums, on_card = data.split('|')
      winning_nums = winning_nums.strip.split(' ').map(&:to_i)
      on_card = on_card.strip.split(' ').map(&:to_i)
      Card.new(i, winning_nums, on_card).tap { i += 1 }
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2023, 4)
end
