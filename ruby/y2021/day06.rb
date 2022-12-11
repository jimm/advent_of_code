#!/usr/bin/env ruby
#
# Lanternfish

require_relative '../day'

class Day06 < Day
  def part1
    do_part(80)
  end

  def part2
    do_part(256)
  end

  def do_part(num_generations)
    lines = data_lines(1)
    fish = lines.first.split(',').map(&:to_i)
    phases = [0] * 9
    fish.each { |f| phases[f] += 1 }
    phases = simulate_generations(phases, num_generations)
    puts phases.sum
  end

  def simulate_generations(phases, n)
    n.times do
      phases = simulate_generation(phases)
    end
    phases
  end

  def simulate_generation(phases)
    new_phases = phases[1..-1]
    new_phases[8] = phases[0]       # num births
    new_phases[6] += phases[0]      # 0 => 6
    new_phases
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc
end
