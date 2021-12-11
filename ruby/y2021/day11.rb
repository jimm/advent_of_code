# Dumbo Octopus

require 'set'
require_relative '../map'

class Day11 < Day
  def part1
    lines = data_lines(1)
    map = Map.new(lines).tap(&:cells_to_ints!)
    num_flashes = 0
    100.times do |_|
      num_flashes += step(map)
    end
    puts num_flashes
  end

  def part1_tests
    lines = data_lines(1)
    map = Map.new(lines).tap(&:cells_to_ints!)
    num_flashes = 0
    puts
    puts 'before step 1'
    puts map
    10.times do |i|
      num_flashes += step(map)
      puts
      puts "after step #{i + 1}"
      puts map
    end
    puts "num flashes: #{num_flashes}"
  end

  def part2
    lines = data_lines(1)
    map = Map.new(lines).tap(&:cells_to_ints!)
    num_cells = map.height * map.width
    step_num = 0
    while true
      step_num += 1
      break if step(map) == num_cells
    end
    puts step_num
  end

  # Run one step and return the number of flashes.
  def step(map)
    all_flashed_cells = Set.new

    # increase all cells by 1, flashing when appropriate
    flashed = Set.new
    map.height.times do |row|
      map.width.times do |col|
        loc = [row, col]
        map.update(row, col) do |val|
          new_val = val + 1
          if new_val > 9
            flashed.add(loc)
            all_flashed_cells.add(loc)
          end
          new_val
        end
      end
    end

    # process flashes
    while true
      new_flashes = Set.new
      flashed.each do |row, col|
        # increase neighbors and see if they flash
        (row - 1..row + 1).each do |r|
          (col - 1..col + 1).each do |c|
            next if row == r && col == c
            next unless map.in_bounds?(r, c)

            loc = [r, c]
            next if all_flashed_cells.include?(loc) # already flashed

            map.update(r, c) do |val|
              new_val = val + 1
              if new_val > 9
                new_flashes.add(loc)
                all_flashed_cells.add(loc)
              end
              new_val
            end
          end
        end
      end
      break if new_flashes.empty?

      flashed = new_flashes
    end

    # set all flashed cells' values to 0
    all_flashed_cells.each do |row, col|
      map.update(row, col) { |_| 0 }
    end

    all_flashed_cells.size
  end
end
