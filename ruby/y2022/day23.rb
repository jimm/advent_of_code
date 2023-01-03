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
    clear_screen                # DEBUG
    debug_print(elves, true)    # DEBUG
    i = 0
    while elves != prev_elves
      prev_elves = elves
      elves = move(elves, dirs[i % 4, 4])
      debug_print(elves, true) # DEBUG
      i += 1
    end
    i
  end

  def move(elves, dirs)
    maybes, unmoving = proposed_moves(elves, dirs)
    return elves if maybes.empty?

    moved_elves = unmoving

    maybes.each do |target, elves|
      puts "target = #{target}, elves = #{elves}" # DEBUG
      if elves.size == 1
        moved_elves.add(target)
      else
        moved_elves += elves
      end
    end

    # maybes.each do |elf, target|
    #   # Inefficient (checks same coord more than once if elves share a
    #   # target)
    #   moved_elves.add(only_elf_targeting(elf, target, maybes) ? target : elf)
    # end

    moved_elves
  end

  # Returns hashes with key = target, val = array of elves moving to that target
  def proposed_moves(elves, dirs)
    # Inefficient (may check same coord more than once when elves near each
    # other)
    proposed_moves = Hash.new { |h, k| h[k] = [] }
    unmoving = Set.new
    elves.each do |elf|
      surrounding_points = SURROUNDING_OFFSETS.map { |dx, dy| [elf[0] + dx, elf[1] + dy] }
      # if nobody around, do not move
      if elves.disjoint?(surrounding_points)
        unmoving.add(elf)
        next
      end

      dirs.each do |dir|
        pts = surrounding_points[DIR_OFFSETS[dir], 3]
        next unless elves.disjoint?(pts)

        proposed_moves[pts[1]] << elf
        break
      end
    end
    [proposed_moves, unmoving]
  end

  def only_elf_targeting(elf, target, maybes)
    maybes.each do |other_elf, other_target|
      next if elf == other_elf
      return false if other_target == target
    end
    true
  end

  def empty_tile_count(elves)
    min_x, max_x = elves.map { |e| e[0] }.minmax
    min_y, max_y = elves.map { |e| e[1] }.minmax
    (max_x - min_x + 1) * (max_y - min_y + 1) - elves.size
  end

  def parse(lines)
    elves = Set.new
    num_lines = lines.length
    lines.each_with_index do |line, y|
      line.split('').each_with_index do |ch, x|
        elves.add([x, num_lines - y]) if ch == '#'
      end
    end
    elves
  end

  def debug_print(elves, clear_screen_first = false)
    print("\e[0;0H") if clear_screen_first
    min_x, max_x = elves.map { |e| e[0] }.minmax
    min_y, max_y = elves.map { |e| e[1] }.minmax
    max_y.downto(min_y).each do |y|
      (min_x..max_x).each do |x|
        print(elves.include?([x, y]) ? '#' : '.')
      end
      puts
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2022, 23)
end
