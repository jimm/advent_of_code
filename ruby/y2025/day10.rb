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

    def min_config_press_count
      (0..nil).each do |n|
        return n if configurable_with_n_presses?(n)
      end
      0 # will never reach
    end

    def min_joltage_press_count
      pdebug
      (0..nil).each do |n|
        return n if joltage_set_with_n_presses?(n)
        return 0 if $DEBUG && n >= 13
      end
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

    def joltage_set_with_n_presses?(n)
      max_presses = {}
      @wiring.each do |buttons|
        max_presses[buttons] = buttons.map { |button| @joltage[button] || 0 }.min
      end
      sets_joltage?(@wiring, max_presses)
      @wiring.combination(n).any? { |schematic| sets_joltage?(schematic, max_presses) }
    end

    # Returns true if the button presses in `schematic` can result in our
    # desired @joltages.
    #
    # `max_presses` is an optimization: a pre-calculated map from wiring
    # schematics (e.g. [3, 5]) to max number of times it can be pressed
    # before one of the joltages will be exceeded.
    def sets_joltage?(schematic, max_presses, curr_joltage = nil)
      return false unless covers_joltage_slots?(schematic)

      do_sets_joltage?(schematic, max_presses, [0] * @joltage.length)
    end

    def do_sets_joltage?(schematic, max_presses, curr_joltage)
      return @joltage == curr_joltage if schematic.empty?
      return false if (0...@joltage.length).any? { |i| curr_joltage[i] > @joltage[i] }

      warn "sets_joltage? schematic = #{schematic.inspect}, curr_joltage = #{curr_joltage.inspect}" # DEBUG
      return true if curr_joltage == @joltage

      # TODO: shortcut: at top level (so need to split this func) return
      # false if schematic does not include all buttons needed for non-zero
      # joltage values.

      # make sure we've not exceeded any required @joltage value
      curr_joltage.each_with_index { |j, i| return false if @joltage[i] < j }

      # For each of the buttons try pressing it 1..max times and applying
      # the rest of the buttons.
      schematic.each_with_index do |buttons, schematic_idx|
        max_for_buttons = max_presses[buttons]
        0.upto(max_for_buttons).each do |press_num|
          # build new joltage obtained after pressing buttons
          new_joltage = press(buttons, curr_joltage)
          # warn "  new_joltage = #{new_joltage.inspect}" # DEBUG
          return true if do_sets_joltage?(schematic[schematic_idx + 1..], max_presses, new_joltage)

          curr_joltage = new_joltage
        end
      end
      false
    end

    def covers_joltage_slots?(schematic)
      buttons_in_schematic = schematic.flatten.uniq.sort
      nonzero_schematic_indexes = []
      @joltage.each_with_index { |j, i| nonzero_schematic_indexes << i if j != 0 }
      nonzero_schematic_indexes.sort!
      buttons_in_schematic == nonzero_schematic_indexes
    end

    def press(buttons, curr_joltage)
      new_joltage = curr_joltage.dup
      buttons.each { |button| new_joltage[button] += 1 }
      new_joltage
    end

    # ================ misc ================
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
