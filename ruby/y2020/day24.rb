#!/usr/bin/env ruby
#
# Lobby Layout

require 'set'
require_relative '../day'

class HexLife
  def initialize(lines)
    @map = run_map_instructions(lines)
  end

  def next_generation
    snapshot = @map.dup
    @map = Hash.new { |h, k| h[k] = :white }

    black_tile_keys = snapshot.keys.select { |k| snapshot[k] == :black }

    # process black tiles
    black_tile_keys.each do |black_loc|
      n = count_surrounding_black(snapshot, black_loc[0], black_loc[1])
      new_color = n == 0 || n > 2 ? :white : :black
      @map[black_loc] = new_color
    end

    # process white tiles that are near at least one black tile
    already_seen = Set.new
    black_tile_keys.each do |black_loc|
      neighbors = surrounding_coords(black_loc[0], black_loc[1])
      neighbors.each do |loc|
        next if snapshot[loc] == :black
        next if already_seen.include?(loc)

        # it's white and we've not already seen it
        new_color = count_surrounding_black(snapshot, loc[0], loc[1]) == 2 ? :black : :white
        @map[loc] = new_color
        already_seen << loc
      end
    end
  end

  def black_tile_count
    @map.values.select { |val| val == :black }.count
  end

  # helpers

  def count_surrounding_black(map, loc_x, loc_y)
    surrounding_coords(loc_x, loc_y)
      .map { |loc| map[loc] }
      .select { |val| val == :black }
      .count
  end

  def hex_move(x, y, sym)
    case sym
    when :e
      [x + 2, y]
    when :w
      [x - 2, y]
    when :ne
      [x + 1, y + 1]
    when :nw
      [x - 1, y + 1]
    when :se
      [x + 1, y - 1]
    when :sw
      [x - 1, y - 1]
    else
      raise "unknown symbol #{sym}"
    end
  end

  def surrounding_coords(loc_x, loc_y)
    %i[e w ne nw se sw].map do |sym|
      x, y = hex_move(loc_x, loc_y, sym)
      [x, y]
    end
  end

  def run_map_instructions(lines)
    map_instructions = parse_map_instructions(lines)

    map = Hash.new { |h, k| h[k] = :white }
    map_instructions.each do |tile_instructions|
      x = y = 0
      tile_instructions.each do |sym|
        x, y = hex_move(x, y, sym)
      end
      key = [x, y]
      map[key] = map[key] == :white ? :black : :white
    end
    map
  end

  def parse_map_instructions(lines)
    lines.map do |line|
      tile_instructions = []
      until line.empty?
        sym_len = line[0] == 'e' || line[0] == 'w' ? 1 : 2
        sym = line[0, sym_len].to_sym
        line = line[sym_len..-1]
        tile_instructions << sym
      end
      tile_instructions
    end
  end
end

# Map of hex tiles is represented as a dictionary. Keys are coordinates and
# rows are :black or :white. All tiles in the infinite map start :white, so
# that's the default if there is no entry.
class Day24 < Day
  def part1
    map = HexLife.new(data_lines(1))
    answer = map.black_tile_count
    puts(answer)
  end

  def part1_tests
    run_one_test(10) do |expected|
      expected = expected.to_i
      map = HexLife.new(data_lines(1))
      answer = map.black_tile_count
      [answer == expected, answer]
    end
  end

  def part2
    map = HexLife.new(data_lines(1))
    100.times { map.next_generation }
    answer = map.black_tile_count
    puts(answer)
  end

  def part2_tests
    run_one_test(2208) do |expected|
      map = HexLife.new(data_lines(1))
      100.times { map.next_generation }
      answer = map.black_tile_count
      [answer == expected, answer]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
