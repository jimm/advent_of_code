#!/usr/bin/env ruby
#
# Pipe Maze

require_relative '../day'
require_relative '../map'

class Day10 < Day
  CORNER_DELTAS = {
    'F' => %i[down right],
    'L' => %i[up right],
    'J' => %i[left up],
    '7' => %i[left down]
  }

  def do_part1(lines)
    map = Map.new(lines)
    start_loc = map.find('S')
    start_char = start_char_at(start_loc, map)
    loop0 = loop_locs(start_loc, start_char, map, 0)
    loop1 = loop_locs(start_loc, start_char, map, 1)
    farthest_distance(loop0, loop1)
  end

  def do_part2(lines)
    map = Map.new(lines)
  end

  private

  def start_char_at(start_loc, map)
    dirs = []
    start_row, start_col = start_loc
    u = map.at(start_row - 1, start_col)
    d = map.at(start_row + 1, start_col)
    r = map.at(start_row, start_col + 1)
    l = map.at(start_row, start_col - 1)
    dirs << :up if ['|', 'F', '7'].include?(u)
    dirs << :down if ['|', 'L', 'J'].include?(d)
    dirs << :left if ['-', 'F', 'L'].include?(l)
    dirs << :right if ['-', 'J', '7'].include?(r)

    case dirs.map(&:to_s).sort
    when %w[down right]
      'F'
    when %w[right up]
      'L'
    when %w[left up]
      'J'
    when %w[down left]
      '7'
    else
      raise 'start char not surrounded by proper chars???'
    end
  end

  def loop_locs(start_loc, start_char, map, start_dir_idx)
    loop_locs = []
    loc = start_loc.dup
    dir = CORNER_DELTAS[start_char][start_dir_idx]
    while loop_locs.empty? || loc != start_loc
      loop_locs << loc.dup
      case dir
      when :up
        loc[0] -= 1
      when :down
        loc[0] += 1
      when :left
        loc[1] -= 1
      when :right
        loc[1] += 1
      end
      ch = map.at(*loc)
      case ch
      when '|'
        dir = dir == :up ? :up : :down
      when '-'
        dir = dir == :left ? :left : :right
      when 'F'
        dir = dir == :up ? :right : :down
      when 'L'
        dir = dir == :down ? :right : :up
      when 'J'
        dir = dir == :down ? :left : :up
      when '7'
        dir = dir == :up ? :left : :down
      end
    end
    loop_locs
  end

  def farthest_distance(loop0, loop1)
    idx = 0
    idx += 1 while idx == 0 || loop0[idx] != loop1[idx]
    idx
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
