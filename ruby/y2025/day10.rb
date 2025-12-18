#!/usr/bin/env ruby
#
# Factory

require_relative '../day'

class Day10 < Day
  class Machine
    attr_accessor :lights, :wiring, :joltage

    def initialize(lights, wiring, joltage)
      @lights = lights          # goal state; array of bools, true == light on
      @wiring = wiring          # array of arrays of numbers
      @joltage = joltage        # array of numbers
    end

    # Returns the min number of presses needed to configure the lights.
    def min_config_press_count
      (0..nil).each do |n|
        return n if configurable_with_n_presses?(n)
      end
      0 # will never reach
    end

    # Returns the min number of schematics presses needed to achieve
    # `@joltage`. Returns nil if it's not possible.
    def min_joltage_press_count
      # Calculate the max number of times each schematic can be pressed
      # before it surpasses the desired joltage for any position.
      @max_presses = {}
      @wiring.each do |buttons|
        @max_presses[buttons] = buttons.map { |button| @joltage[button] || 0 }.min
      end

      min_presses(@wiring, 0, [0] * @joltage.length)
    end

    def to_s
      "Machine(l=#{@lights.inspect}, w=#{@wiring.inspect}, j=#{@joltage.inspect})"
    end

    # ================ configuration ================

    def configurable_with_n_presses?(n)
      on_indexes = (0...@lights.length).select { |i| @lights[i] }
      @wiring.combination(n).any? { |schematic| configures?(on_indexes, schematic) }
    end

    # Returns true of the button presses in `schematic` can result in the
    # same `on_indexes` as @lights.
    def configures?(on_indexes, schematic)
      on_after_presses_indexes = schematic.flatten.tally.select { |_, v| v.odd? }.keys
      on_indexes.length == on_after_presses_indexes.length && on_indexes == on_after_presses_indexes.sort
    end

    # ================ joltage settings ================

    # Returns the total number of schematics presses needed to achieve
    # `@joltage`, recursively. Returns nil if it's not possible.
    #
    # `@max_presses` is an optimization: a pre-calculated map from wiring
    # schematics (e.g. [3, 5]) to max number of times it can be pressed
    # before one of the joltages will be exceeded.
    def min_presses(schematics, num_presses, curr_joltage)
      return num_presses if @joltage == curr_joltage
      return nil if schematics.empty?
      return nil if (0...@joltage.length).any? { |i| curr_joltage[i] > @joltage[i] }

      # If we can simply multiply each value in curr_joltage by the same
      # value to get @joltage, we win!
      mult = find_common_mult(num_presses, curr_joltage)
      puts 'OMG IT WORKED!' if mult
      return mult * num_presses if mult

      rest = schematics[1..]
      num_presses_needed = []

      # zero presses of the first set of buttons
      n = min_presses(rest, num_presses, curr_joltage)
      num_presses_needed << n unless n.nil?

      # Try pressing the first schematic 1..max times and see if the
      # remaining schematics will get us to our solution.
      buttons = schematics[0]
      @max_presses[buttons].times do
        # build new joltage obtained after pressing buttons
        new_joltage = press(buttons, curr_joltage)
        num_presses += 1
        n = min_presses(rest, num_presses, new_joltage)
        num_presses_needed << n unless n.nil?

        curr_joltage = new_joltage
      end
      num_presses_needed.min
    end

    # Presses `buttons` with `curr_joltage` and returns a new joltage array
    def press(buttons, curr_joltage)
      new_joltage = curr_joltage.dup
      buttons.each { |button| new_joltage[button] += 1 }
      new_joltage
    end

    # If each of curr_joltage's values can be multipled by the same value to
    # get @joltage, return that multiplier. Else return nil.
    def find_common_mult(num_presses, curr_joltage)
      mults =
        @joltage
        .zip(curr_joltage)
        .map { |vals| vals[1] == 0 ? 0 : vals[0].to_f / vals[1].to_f }
        .uniq
      return nil unless mults.length == 1

      mult = mults[0] * num_presses
      return nil if mult == 0

      mult.to_int == mult ? mult.to_int : nil
    end

    # Returns true if all of the buttons needed by @joltage are included in
    # `schematic`.
    def covers_joltage_slots?(schematic)
      buttons_in_schematic = schematic.flatten.uniq.sort
      nonzero_schematic_indexes = []
      @joltage.each_with_index { |j, i| nonzero_schematic_indexes << i if j != 0 }
      nonzero_schematic_indexes.sort!
      buttons_in_schematic == nonzero_schematic_indexes
    end
  end

  def do_part1(lines)
    machines_from_lines(lines)
      .map(&:min_config_press_count)
      .sum
  end

  def do_part2(lines)
    machines_from_lines(lines)
      .map(&:min_joltage_press_count)
      .sum
  end

  def machines_from_lines(lines)
    lines.map do |line|
      lights = ''
      wiring = []
      joltage = []
      line.split(' ').each do |part|
        if part[0] == '['
          lights = part[1..-2].chars.map { |ch| ch == '#' }
        elsif part[0] == '('
          wiring << part[1..-2].split(',').map(&:to_i)
        elsif part[0] == '{'
          joltage = part[1..-2].split(',').map(&:to_i)
        end
      end
      Machine.new(lights, wiring, joltage)
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
