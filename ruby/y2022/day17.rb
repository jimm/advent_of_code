#!/usr/bin/env ruby
#
# Pyroclastic Flow

require_relative '../day'

class Day17 < Day
  PART_ITERATIONS = [2022, 1_000_000_000_000]

  class Chamber
    CHAMBER_WIDTH = 7
    EMPTY_ROW = '.' * CHAMBER_WIDTH
    ROCKS = [
      [[0, 0], [0, 1], [0, 2], [0, 3]],
      [[0, 1], [1, 0], [1, 1], [1, 2], [2, 1]],
      [[0, 2], [1, 2], [2, 0], [2, 1], [2, 2]],
      [[0, 0], [1, 0], [2, 0], [3, 0]],
      [[0, 0], [0, 1], [1, 0], [1, 1]]
    ]
    ROCK_INITIAL_LEFT = 2
    ROCK_HEIGHTS = [1, 3, 3, 4, 2]
    MAX_ROCK_HEIGHT = 4

    def initialize(wind)
      @wind = wind
      @wind_index = 0
      @rock = nil
      @rock_index = 0
      @rows = []
    end

    def drop_next_rock
      @rock = ROCKS[@rock_index]
      @offset = [0, ROCK_INITIAL_LEFT]
      adjust_height
      # draw # DEBUG
      while true
        wind_push_rock
        break if rock_fall_down == :landed
      end
      @rock_index = (@rock_index + 1) % ROCKS.length
    end

    def rock_height
      @rows.length - top_index
    end

    def to_s
      if @rock
        @rock.each do |x, y|
          @rows[x + @offset[0]][y + @offset[1]] = '@'
        end
      end
      str = @rows.map(&:join).join("\n")
      if @rock
        @rock.each do |x, y|
          @rows[x + @offset[0]][y + @offset[1]] = '.'
        end
      end
      str
    end

    def inspect
      to_s
    end

    private

    def wind_push_rock
      wind_direction = @wind[@wind_index]
      # puts "  wind_push_rock old @offset = #{@offset.inspect}, wind_direction = #{wind_direction}" # DEBUG
      new_offset = @offset.dup
      new_offset[1] += (wind_direction == '<' ? -1 : 1)
      @offset = new_offset if rock_can_move_to?(new_offset)
      # puts "  wind_push_rock @offset = #{@offset.inspect}" # DEBUG
      @wind_index = (@wind_index + 1) % @wind.length
      # puts 'wind_push_rock'     # DEBUG
      # draw                      # DEBUG
    end

    def rock_fall_down
      # puts 'rock_fall_down'     # DEBUG
      new_offset = @offset.dup
      new_offset[0] += 1
      if rock_can_move_to?(new_offset)
        @offset = new_offset
        # puts "  rock_fall_down @offset = #{@offset.inspect}" # DEBUG
        # draw                                                 # DEBUG
      else
        # draw rock
        @rock.each do |x, y|
          @rows[x + @offset[0]][y + @offset[1]] = '#'
        end
        # puts '  rock_fall_down landed' # DEBUG
        # debug_rock = @rock             # DEBUG
        # @rock = nil                    # DEBUG
        # draw                           # DEBUG
        # @rock = debug_rock             # DEBUG
        :landed
      end
    end

    def adjust_height
      # wasteful and non-optimized
      blank_rows = (0..(ROCK_HEIGHTS[@rock_index] + 2)).to_a.map { EMPTY_ROW.dup }
      @rows = blank_rows + @rows[top_index..]
    end

    def top_index
      top_index = 0
      top_index += 1 while @rows[top_index] == EMPTY_ROW
      top_index
    end

    def rock_can_move_to?(offset)
      @rock.all? do |x, y|
        x += offset[0]
        y += offset[1]
        x >= 0 && x < @rows.length && y >= 0 && y < CHAMBER_WIDTH && @rows[x][y] != '#'
      end
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
    chamber.rock_height
  end

  def parse(lines)
    lines[0].split('')
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 17)
end
