#!/usr/bin/env ruby
#
# Dirac Dice

require_relative '../day'

class Day21 < Day
  ROLL_LOOKUPS = []
  (1..3).each do |roll1|
    lookup1 = roll1 << 4
    (1..3).each do |roll2|
      lookup2 = lookup1 + (roll2 << 2)
      (1..3).each do |roll3|
        ROLL_LOOKUPS << lookup2 + roll3
      end
    end
  end

  class Pawn
    attr_reader :loc, :score

    def initialize(loc, win_score)
      @loc = loc - 1 # `loc` is 1-10, @loc values are 0-9
      @score = 0
      @win_score = win_score
    end

    def move(rolls)
      @loc += rolls.sum
      @loc = @loc % 10
      @score += (@loc + 1)
    end

    def won?
      @score >= win_score
    end
  end

  def part1
    pawns = if @testing
              [Pawn.new(4, 1000), Pawn.new(8, 1000)]
            else
              [Pawn.new(4, 1000), Pawn.new(2, 1000)]
            end
    rolls = (1..100).to_a

    num_rolls = 0
    turn = 0
    while true
      pawn = pawns[turn]
      rolls += (1..100).to_a if rolls.length < 3
      turn_rolls = rolls.take(3)
      pawn.move(turn_rolls)
      num_rolls += 3
      if pawn.won?
        puts num_rolls * pawns[1 - turn].score
        break
      end
      rolls = rolls.drop(3)
      turn = 1 - turn
    end
  end

  def part2
    # There are only 7 possible sums of the three three-sided dice, even
    # though there are 27 different possible dice permutations.
    #
    # For each of those sums, we precalculate the outcome (new location and
    # score increment) given the starting location.
    moves = []
    (3..9).each do |move|
      moves[move] = []
      10.times do |start_loc|
        end_loc = (start_loc + move) % 10
        moves[move] << end_loc
        # score increment = end_loc + 1 (locs 0-9, score vals 1-10)
      end
    end

    outcomes = {}
    (1..3).each do |roll1|
      lookup1 = roll1 << 4
      (1..3).each do |roll2|
        lookup2 = lookup1 + (roll2 << 2)
        (1..3).each do |roll3|
          lookup = lookup2 + roll3
          raise 'lookup build error' unless ROLL_LOOKUPS.include?(lookup)

          move = roll1 + roll2 + roll3
          outcomes[lookup] = moves[move]
        end
      end
    end

    # [current loc, score]
    pawns = if @testing
              [[4, 0], [8, 0]]
            else
              [[4, 0], [2, 0]]
            end
    p1_wins, p2_wins = *calc_wins(pawns, 0, [0, 0], outcomes)
    puts "p1 wins: #{p1_wins}"
    puts "p2 wins: #{p2_wins}"
  end

  # FIXME
  def calc_wins(pawns, turn, win_counts, outcomes)
    # DEBUG this check only valid for test mode
    if win_counts[0] + win_counts[1] > (444_356_092_776_315 + 341_960_390_180_808 + 1)
      raise 'reality error: too many universes visited'
    end

    ROLL_LOOKUPS.each do |lookup|
      loc, score = pawns[turn]
      new_loc = outcomes[lookup][loc]
      new_score = score + (new_loc + 1)

      if new_score >= 21 # pawn wins
        win_counts[turn] += 1
      else
        copy = [0, 0]
        copy[turn] = [new_loc, score + new_loc + 1]
        copy[1 - turn] = [pawns[1 - turn][0], pawns[1 - turn][1]]
        calc_wins(copy, 1 - turn, win_counts, outcomes)
      end
    end
    win_counts
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
