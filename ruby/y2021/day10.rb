#!/usr/bin/env ruby
#
# Syntax Scoring

require_relative '../day'

class Day10 < Day
  ERROR_SCORES = {
    ')' => 3,
    ']' => 57,
    '}' => 1197,
    '>' => 25_137
  }
  REPAIR_SCORES = {
    ')' => 1,
    ']' => 2,
    '}' => 3,
    '>' => 4
  }
  CLOSER_TO_OPENER = {
    ')' => '(',
    ']' => '[',
    '}' => '{',
    '>' => '<'
  }
  OPENER_TO_CLOSER = {
    '(' => ')',
    '[' => ']',
    '{' => '}',
    '<' => '>'
  }

  def part1
    lines = data_lines(1)
    score = lines.reduce(0) { |score, line| score + error_score(line) }
    puts score
  end

  def part2
    lines = data_lines(1)
    repair_scores = lines.map do |line|
      if error_score(line) == 0
        repair_score(line)
      else
        nil # ignore error lines
      end
    end
    repair_scores.compact!.sort!
    puts repair_scores[repair_scores.length / 2]
  end

  def error_score(line)
    stack = []
    line.each_char do |ch|
      if OPENER_TO_CLOSER.keys.include?(ch)
        stack.push(ch)
      else
        match = stack.pop
        return ERROR_SCORES[ch] if match != CLOSER_TO_OPENER[ch]
      end
    end
    0
  end

  def repair_score(line)
    stack = []
    line.each_char do |ch|
      if OPENER_TO_CLOSER.keys.include?(ch)
        stack.push(ch)
      else
        stack.pop
      end
    end
    score = 0
    score = score * 5 + REPAIR_SCORES[OPENER_TO_CLOSER[stack.pop]] until stack.empty?
    score
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
