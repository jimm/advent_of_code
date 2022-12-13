#!/usr/bin/env ruby
#
# Seating System

require_relative '../day'
require_relative '../map'

class SeatingMap < Map
  def initialize(lines, adjacent_seat_threshold, tally_func_sym)
    super(lines)
    @adjacent_seat_threshold = adjacent_seat_threshold
    @tally_func_sym = tally_func_sym
  end

  def apply_rules
    @snapshot = @cells.map(&:dup)
    (0...@height).each do |row|
      (0...@width).each do |col|
        cell = @snapshot[row][col]
        adjacent_counts = send(@tally_func_sym, row, col)
        occupied_adjacent = adjacent_counts['#']
        if cell == 'L' && occupied_adjacent == 0
          @cells[row][col] = '#'
        elsif cell == '#' && occupied_adjacent >= @adjacent_seat_threshold
          @cells[row][col] = 'L'
        end
      end
    end
  end

  def changed?
    @snapshot != @cells
  end

  def tally_adjacent(row, col)
    tally = { 'L' => 0, '.' => 0, '#' => 0 }
    (-1..1).each do |rdelta|
      (-1..1).each do |cdelta|
        next if rdelta == 0 && cdelta == 0

        r = row + rdelta
        c = col + cdelta
        tally[@snapshot[r][c]] += 1 if in_bounds?(r, c)
      end
    end
    tally
  end

  def tally_line_of_sight(row, col)
    tally = { 'L' => 0, '.' => 0, '#' => 0 }
    (-1..1).each do |rdelta|
      (-1..1).each do |cdelta|
        next if rdelta == 0 && cdelta == 0

        r = row + rdelta
        c = col + cdelta
        found = false
        while !found && in_bounds?(r, c)
          val = @snapshot[r][c]
          if val == '.'
            r += rdelta
            c += cdelta
          else
            tally[val] += 1
            found = true
          end
        end
      end
    end
    tally
  end
end

class Day11 < Day
  def part1
    do_part(4, :tally_adjacent)
  end

  def part2
    do_part(5, :tally_line_of_sight)
  end

  def do_part(adjacent_seat_threshold, tally_func_sym)
    map = SeatingMap.new(data_lines(1), adjacent_seat_threshold, tally_func_sym)
    while true
      map.apply_rules
      unless map.changed?
        puts(map.cells.flatten.tally['#'])
        exit(0)
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2020, 11)
end
