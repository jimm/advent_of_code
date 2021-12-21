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
  end
end
