#!/usr/bin/env ruby
#
# Passage Pathing

require_relative '../day'

class Day12 < Day
  class Cave
    attr_reader :name, :size
    attr_accessor :connections

    def initialize(name)
      @name = name
      @size = name.downcase == name ? :small : :big
      @connections = []
    end

    def start?
      @name == 'start'
    end

    def end?
      @name == 'end'
    end

    def add(cave)
      # Don't add outgoing caves to end or incoming caves to start
      return if end? || cave.start?

      @connections << cave
    end

    def remove(cave)
      @connections.delete(cave)
    end

    def remove_self
      @connections.each do |c|
        c.remove(self)
      end
      @connections = []
    end

    def dead_end?
      @size == :small && @connections.all? { |c| c.size == :small }
    end

    def to_s
      @name
    end

    def inspect
      to_s
    end
  end

  class CaveSystem
    attr_reader :caves, :start, :end

    def initialize(lines)
      @caves = {}
      lines.each do |line|
        names = line.split('-')
        c0 = caves[names[0]] || build_cave(names[0])
        c1 = caves[names[1]] || build_cave(names[1])
        c0.add(c1)
        c1.add(c0)
      end
    end

    def num_paths(max_small_cave_revisits = 0)
      paths = build_paths([
                            [[@start], max_small_cave_revisits]
                          ])
      paths.size
    end

    private

    # `paths` is an array of [path, max_small_cave_revisits] arrays. For
    # each path, return all possible paths leading to @end from the last
    # node in the path.
    def build_paths(paths)
      return paths.map(&:first) if paths.all? { |p| p[0].last == @end }

      new_paths = []
      paths.map do |path_and_mscr|
        path, max_small_cave_revisits = *path_and_mscr
        curr_cave = path.last
        if curr_cave.end?
          new_paths << path_and_mscr
          next
        end
        curr_cave.connections.each do |c|
          if c.size == :small && path.include?(c)
            if max_small_cave_revisits > 0
              # We have already visited this small cave, but we're allowed
              # to revisit it.
              new_paths << ([path + [c], max_small_cave_revisits - 1])
            end
          else
            new_paths << ([path + [c], max_small_cave_revisits])
          end
        end
      end
      build_paths(new_paths)
    end

    def build_cave(name)
      Cave.new(name).tap do |c|
        @caves[name] = c
        @start = c if name == 'start'
        @end = c if name == 'end'
      end
    end

    def find_dead_ends
      @caves.values
            .select { |c| c.dead_end? }
            .reject { |c| %w[start end].include?(c.name) }
    end
  end

  def part1
    cave_system = CaveSystem.new(data_lines(1))
    puts cave_system.num_paths
  end

  def part1_tests
    tests(0, :first)
  end

  def part2
    cave_system = CaveSystem.new(data_lines(1))
    puts cave_system.num_paths(1)
  end

  def part2_tests
    tests(1, :last)
  end

  def tests(max_small_cave_revisits, expected_extractor_sym)
    run_chunk_tests(1) do |expected_str, lines|
      expected = expected_str.split(',').send(expected_extractor_sym).to_i
      cave_system = CaveSystem.new(lines)
      num_paths = cave_system.num_paths(max_small_cave_revisits)
      [num_paths == expected, num_paths]
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
