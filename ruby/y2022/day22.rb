#!/usr/bin/env ruby
#
# Monkey Map

require_relative '../day'
require_relative '../map'

class Day22 < Day
  DIR_TO_DELTAS = {
    n: [-1, 0],
    e: [0, 1],
    s: [1, 0],
    w: [0, -1]
  }
  ROTATIONS = {
    'L' => {n: :w, w: :s, s: :e, e: :n},
    'R' => {n: :e, e: :s, s: :w, w: :n}
  }
  FACING_SCORE = {
    e: 0, s: 1, w: 2, n: 3
  }

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

  # part 1 166102 too high
  def do_part(lines)
    map = Map.new(lines[..-2], :both)
    directions = lines[-1].strip
    set_starting_loc(map)
    facing = :e
    distance = 0

    until directions.empty?
      ch = directions[0]
      if ch == 'L' || ch == 'R'
        facing = ROTATIONS[ch][facing]
        directions = directions[1..]
      else
        directions =~ /^(\d+)/
        directions = directions[$1.length..]
        distance = $1.to_i
        move(map, facing, distance)
      end
    end

    (map.row + 1) * 1000 + (map.col + 1) * 4 + FACING_SCORE[facing]
  end

  def move(map, facing, distance)
    delta = DIR_TO_DELTAS[facing]
    distance.times do
      ch, row, col = next_char_and_loc(map, delta)
      break unless ch == '.'
      map.move_to(row, col)
    end
  end

  def next_char_and_loc(map, delta)
    row = map.row + delta[0]
    col = map.col + delta[1]
    ch = map.at(row, col)
    while ch == ' '
      row += delta[0]
      col += delta[1]
      ch = map.at(row, col)
    end
    [map.at(row, col), row, col]
  end

  def set_starting_loc(map)
    map.move_by(0, 1) while map.here != '.'
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 22)
end
