#!/usr/bin/env ruby
#
# Pyroclastic Flow

require 'set'
require_relative '../day'
require_relative '../point'

class Day17 < Day
  PART_ITERATIONS = [2022, 1_000_000_000_000]

  # x goes RIGHT, y goes UP
  class Chamber
    CHAMBER_WIDTH = 7
    EMPTY_ROW = '.' * CHAMBER_WIDTH
    ROCKS = [
      [[0, 0], [1, 0], [2, 0], [3, 0]],
      [[1, 0], [0, -1], [1, -1], [2, -1], [1, -2]],
      [[2, 0], [2, -1], [0, -2], [1, -2], [2, -2]],
      [[0, 0], [0, -1], [0, -2], [0, -3]],
      [[0, 0], [1, 0], [0, -1], [1, -1]]
    ]
    ROCK_INITIAL_LEFT = 2
    ROCK_HEIGHTS = [1, 3, 3, 4, 2]
    MAX_ROCK_HEIGHT = 4

    def initialize(wind)
      @wind = wind
      @wind_index = 0
      @rock = nil
      @rock_index = 0
      @rock_coords = Set.new
      @max_height = -1
    end

    def drop_next_rock
      @rock = ROCKS[@rock_index]
      @offset = [ROCK_INITIAL_LEFT, @max_height + 1 + ROCK_HEIGHTS[@rock_index] + 2]
      # puts "drop_next_rock @max_height = #{@max_height}, ROCK_HEIGHTS[@rock_index] = #{ROCK_HEIGHTS[@rock_index]}" # DEBUG
      # draw # DEBUG
      while true
        wind_push_rock
        break if rock_fall_down == :landed
      end
      @rock_index = (@rock_index + 1) % ROCKS.length
      remove_depths
    end

    def highest_rock_y
      @max_height + 1
    end

    def draw
      puts '****************'
      puts 'Chamber'
      puts "@rock = #{@rock}"
      puts "@offset = #{@offset}, @max_height = #{@max_height}"
      puts "@rock_coords = #{@rock_coords}"
      puts to_s
      puts '****************'
    end

    def to_s
      strs = []
      floating_coords = nil
      if @rock
        floating_coords = Set.new
        @rock.each { |p| floating_coords.add([p[0] + @offset[0], p[1] + @offset[1]]) }
      end
      start_y = [@max_height, floating_coords ? floating_coords.map(&:y).max : 0].max
      start_y.downto(0) do |y|
        s = ''
        (0...CHAMBER_WIDTH).each do |x|
          p = [x, y]
          s << if @rock_coords.include?(p)
                 '#'
               elsif floating_coords && floating_coords.include?(p)
                 '@'
               else
                 '.'
               end
        end
        strs << s
      end
      strs.join("\n")
    end

    def inspect
      to_s
    end

    private

    def wind_push_rock
      wind_direction = @wind[@wind_index]
      # puts "  wind_push_rock old @offset = #{@offset.inspect}, wind_direction = #{wind_direction}" # DEBUG
      new_offset = [@offset[0] + (wind_direction == '<' ? -1 : 1), @offset[1]]
      @offset = new_offset if rock_can_move_by?(new_offset)
      # puts "  wind_push_rock @offset = #{@offset.inspect}" # DEBUG
      @wind_index = (@wind_index + 1) % @wind.length
      # puts 'wind_push_rock'     # DEBUG
      # draw                      # DEBUG
    end

    def rock_fall_down
      # puts 'rock_fall_down'     # DEBUG
      new_offset = [@offset[0], @offset[1] - 1]
      if rock_can_move_by?(new_offset)
        @offset = new_offset
        # puts "  rock_fall_down @offset = #{@offset.inspect}" # DEBUG
        # draw                                                 # DEBUG
        nil
      else
        # draw rock
        @rock.each do |p|
          x = p[0] + @offset[0]
          y = p[1] + @offset[1]
          @rock_coords.add([x, y])
          @max_height = y if y > @max_height
        end
        # puts '  rock_fall_down landed' # DEBUG
        # debug_rock = @rock             # DEBUG
        # @rock = nil                    # DEBUG
        # draw                           # DEBUG
        # @rock = debug_rock             # DEBUG
        :landed
      end
    end

    def rock_can_move_by?(offset)
      @rock.all? do |rock_p|
        p = [rock_p[0] + offset[0], rock_p[1] + offset[1]]
        p[0] >= 0 && p[0] < CHAMBER_WIDTH && p[1] >= 0 && !@rock_coords.include?(p)
      end
    end

    # We can ignore anything that's much farther down then the current @max_height
    def remove_depths
      max_keep_y = @max_height - 50
      remove = @rock_coords.select { |p| p[1] < max_keep_y }
      @rock_coords -= remove
    end
  end

  # ================ Parts ================

  def part1
    puts do_part(data_lines(1))
  end

  def part1_tests
    do_tests
  end

  def part2
    puts do_part(data_lines(1))
  end

  def part2_tests
    do_tests
  end

  private

  def do_tests
    run_chunk_tests(1) do |expected, lines|
      expected = expected.split(',')[@part_number - 1].to_i
      answer = do_part(lines)
      [answer == expected, answer, expected]
    end
  end

  def do_part(lines)
    wind = parse(lines)
    chamber = Chamber.new(parse(lines))
    PART_ITERATIONS[@part_number - 1].times do |i|
      chamber.drop_next_rock
      # chamber.draw              # DEBUG
      # break if i > 5            # DEBUG
    end
    chamber.highest_rock_y
  end

  def parse(lines)
    lines[0].split('')
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 17)
end
