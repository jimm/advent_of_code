#!/usr/bin/env ruby
#
# Rock Paper Scissors

require_relative '../day'

class Day02 < Day
  SHAPE_SCORE = { rock: 1, paper: 2, scissors: 3 }
  WIN_SCORE = { lose: 0, draw: 3, win: 6 }
  # [theirs, yours] => win symbol
  MOVE_RESULT = {
    %i[rock rock] => :draw,
    %i[rock paper] => :win,
    %i[rock scissors] => :lose,
    %i[paper rock] => :lose,
    %i[paper paper] => :draw,
    %i[paper scissors] => :win,
    %i[scissors rock] => :win,
    %i[scissors paper] => :lose,
    %i[scissors scissors] => :draw
  }
  CODE_TO_SYM = {
    'A' => :rock,
    'B' => :paper,
    'C' => :scissors,
    'X' => :rock,
    'Y' => :paper,
    'Z' => :scissors
  }
  CODE_TO_RESULT = {
    'X' => :lose,
    'Y' => :draw,
    'Z' => :win
  }

  def part1
    do_part(:part1_score)
  end

  def part2
    do_part(:part2_score)
  end

  def do_part(score_method)
    lines = data_lines(1)
    score = 0
    lines.each do |line|
      round = line.split
      score += send(score_method, round)
    end
    puts score
  end

  def part1_score(round)
    round = round.map { |str| CODE_TO_SYM[str] }
    shape_score = SHAPE_SCORE[round[1]]
    move_result = MOVE_RESULT[round]
    win_score = WIN_SCORE[move_result]
    shape_score + win_score
  end

  def part2_score(round)
    theirs = CODE_TO_SYM[round[0]]
    desired_result = CODE_TO_RESULT[round[1]]
    win_entry = MOVE_RESULT.detect do |k, v|
      k[0] == theirs && v == desired_result
    end
    shape_score = SHAPE_SCORE[win_entry[0][1]]
    win_score = WIN_SCORE[desired_result]
    shape_score + win_score
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 2)
end
