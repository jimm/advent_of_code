#!/usr/bin/env ruby
#
# Unstable Diffusion

require 'set'
require_relative '../day'
require_relative '../point'

class Day23 < Day
  SURROUNDING_OFFSETS = [
    # Starting in NW corner, clockwise, plus one extra wrapping around
    [-1, 1], [0, 1], [1, 1], [1, 0], [1, -1], [0, -1], [-1, -1], [-1, 0], [-1, 1]

  ]
  DIR_OFFSETS = {
    n: 0, e: 2, s: 4, w: 6
  }

  def part1
    puts do_part1(data_lines(1))
  end

  def part1_tests
    run_one_test(110) { |_| do_part1(data_lines(1)) }
  end

  def part2
    puts do_part2(data_lines(1))
  end

  def part2_tests
    run_one_test(20) { |_| do_part2(data_lines(1)) }
  end

  private

  def do_part1(lines)
    elves = parse(data_lines(1))
    dirs = %i[n s w e n s w]
    # debug_print(elves) # DEBUG
    10.times do |i|
      elves = move(elves, dirs[i % 4, 4])
      # puts "round #{i + 1}" # DEBUG
      # debug_print(elves) # DEBUG
    end
    empty_tile_count(elves)
  end

  def do_part2(lines)
    elves = parse(data_lines(1))
    prev_elves = nil
    dirs = %i[n s w e n s w]
    # debug_print(elves) # DEBUG
    i = 0
    while elves != prev_elves
      prev_elves = elves
      elves = move(elves, dirs[i % 4, 4])
      i += 1
    end
    i
  end

  def move(elves, dirs)
    maybes = proposed_moves(elves, dirs)
    return elves if maybes.empty?

    new_elves = elves - maybes.keys # start with elves that are not moving
    maybes.each do |elf, target|
      # Inefficient (checks same coord more than once if elves share a
      # target)
      new_elves.add(only_elf_targeting(elf, target, maybes) ? target : elf)
    end
    new_elves
  end

  def proposed_moves(elves, dirs)
    # Inefficient (may check same coord more than once when elves near each
    # other)
    proposed_moves = {}
    elves.each do |elf|
      surrounding_points = SURROUNDING_OFFSETS.map { |x, y| Point.new(elf.x + x, elf.y + y) }
      # if nobody around, do not move
      next if elves.disjoint?(surrounding_points)

      dirs.each do |dir|
        pts = surrounding_points[DIR_OFFSETS[dir], 3]
        next unless elves.disjoint?(pts)

        proposed_moves[elf] = pts[1]
        break
      end
    end
    proposed_moves
  end

  def only_elf_targeting(elf, target, maybes)
    maybes.each do |other_elf, other_target|
      next if elf == other_elf
      return false if other_target == target
    end
    true
  end

  def empty_tile_count(elves)
    min_x, max_x = elves.map { |e| e.x }.minmax
    min_y, max_y = elves.map { |e| e.y }.minmax
    (max_x - min_x + 1) * (max_y - min_y + 1) - elves.size
  end

  def parse(lines)
    elves = Set.new
    num_lines = lines.length
    lines.each_with_index do |line, y|
      line.split('').each_with_index do |ch, x|
        elves.add(Point.new(x, num_lines - y)) if ch == '#'
      end
    end
    elves
  end

  def debug_print(elves)
    min_x, max_x = elves.map { |e| e.x }.minmax
    min_y, max_y = elves.map { |e| e.y }.minmax
    max_y.downto(min_y).each do |y|
      (min_x..max_x).each do |x|
        print(elves.include?(Point.new(x, y)) ? '#' : '.')
      end
      puts
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 23)
end
