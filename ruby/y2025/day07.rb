#!/usr/bin/env ruby
#
# Laboratories

require_relative '../day'
require_relative '../map'

class Day07 < Day
  START = 'S'
  SPLITTER = '^'

  def do_part1(lines)
    m = Map.new(lines)
    count_splits_from_start(m)
  end

  def do_part2(lines)
    m = Map.new(lines)
    count_timelines(m)
  end

  def count_splits_from_start(m)
    _, start_col = m.find(START)
    beam_cols = [start_col]
    total_splits = 0
    (1..m.height - 1).each do |row|
      new_beam_cols = []
      beam_cols.each do |beam_col|
        if m.at(row, beam_col) == SPLITTER
          total_splits += 1
          new_beam_cols << beam_col - 1 unless new_beam_cols.include?(beam_col - 1)
          new_beam_cols << beam_col + 1 unless new_beam_cols.include?(beam_col + 1)
        else
          new_beam_cols << beam_col
        end
      end
      beam_cols = new_beam_cols.uniq
    end
    total_splits
  end

  def count_timelines(m)
    _, start_col = m.find(START)
    # each entry is {column, number of beams that got here}
    beam_curr_cols = { start_col => 1 }
    (1..m.height - 1).each do |row|
      new_curr_cols = Hash.new { |h, k| h[k] = 0 }
      beam_curr_cols.each_pair do |beam_col, num_beams|
        if m.at(row, beam_col) == SPLITTER
          new_curr_cols[beam_col - 1] += num_beams
          new_curr_cols[beam_col + 1] += num_beams
        else
          new_curr_cols[beam_col] += num_beams
        end
      end
      beam_curr_cols = new_curr_cols
    end
    beam_curr_cols.map(&:last).sum
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
