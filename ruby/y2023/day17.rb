#!/usr/bin/env ruby
#
# Clumsy Crucible

require_relative '../day'
require_relative '../map'

class Day17 < Day
  def do_part1(lines)
    map = Map.new(lines)
    map.cells_to_ints!
    path = a_star(map, [0, 0], [map.height - 1, map.width - 1])
    pp path # DEBUG
    path.map { |cell| map.at(*cell) }.sum
  end

  def do_part2(lines)
    # TODO
  end

  private

  def naive_crawl(map, start, goal)
    until r == goal_r && c = goal_c

    end
  end

  def heuristic(map, cell)
    map.at(*cell)
  end

  def a_star(map, start, goal)
    neighbors = [[0, 1], [1, 0], [-1, 0], [0, -1]]
    close_set = Set.new
    came_from = {}
    gscore = Hash.new { |h, k| h[k] = 0 }
    gscore[start] = 0
    fscore = Hash.new { |h, k| h[k] = 0 }
    fscore[start] = heuristic(map, start)
    oheap = []

    oheap.push([fscore[start], start])
    until oheap.empty?
      # find oheap entry with min fscore
      min_fscore = oheap.map(&:first).min
      fscore_and_cell = oheap.detect { |val| val[0] == min_fscore }
      oheap.delete(fscore_and_cell)
      current = fscore_and_cell[1]

      if current == goal
        data = []
        while came_from.include?(current)
          data << current
          current = came_from[current]
        end
        return data
      end

      close_set.add(current)
      neighbors.each do |delta|
        neighbor = [current[0] + delta[0], current[1] + delta[1]]
        next unless map.in_bounds?(*neighbor)
        next if too_far_in_straight_line?(came_from, current, neighbor)
        next if neighbor == came_from[current] # can't reverse direction

        tentative_g_score = gscore[current] + heuristic(map, neighbor)
        next if close_set.include?(neighbor) && tentative_g_score >= gscore[neighbor]

        next unless tentative_g_score < gscore[neighbor] || !oheap.map(&:last).include?(neighbor)

        came_from[neighbor] = current
        gscore[neighbor] = tentative_g_score
        fscore[neighbor] = tentative_g_score + heuristic(map, neighbor)
        oheap.push([fscore[neighbor], neighbor])
      end
    end
    nil
  end

  def too_far_in_straight_line?(came_from, current, next_loc)
    prevs = []
    while came_from.include?(current) && prevs.length < 3
      prevs << current
      current = came_from[current]
    end
    return false unless prevs.length == 2 || prevs.length == 3 # starting block counts
    return true if prevs.all? { _1[0] == next_loc[0] }
    return true if prevs.all? { _1[1] == next_loc[1] }

    false
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
