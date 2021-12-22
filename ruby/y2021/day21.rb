# Dirac Dice

class Day21 < Day
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
      (0..9).each do |start_loc|
        end_loc = (start_loc + move) % 10
        moves[move] << end_loc
        # score increment = end_loc + 1 (locs 0-9, score vals 1-10)
      end
    end

    outcomes = {}
    (1..3).each do |roll1|
      (1..3).each do |roll2|
        (1..3).each do |roll3|
          move = roll1 + roll2 + roll3
          outcomes[[roll1, roll2, roll3]] = moves[move]
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
    (1..3).each do |roll1|
      (1..3).each do |roll2|
        (1..3).each do |roll3|
          loc, score = pawns[turn]
          new_loc = outcomes[[roll1, roll2, roll3]][loc]
          new_score = score + (new_loc + 1)

          if new_score >= 21 # pawn wins
            win_counts[turn] += 1
          else
            copy = pawns.dup
            copy[turn] = [new_loc, score + new_loc + 1]
            calc_wins(copy, 1 - turn, win_counts, outcomes)
          end
        end
      end
    end
    win_counts
  end
end
