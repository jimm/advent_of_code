#!/usr/bin/env ruby
#
# Wait For It

require_relative '../day'

class Day06 < Day
  def do_part1(lines)
    data = parse1(lines)
    data.map { brute_force_num_wins(_1) }.inject(1, &:*)
  end

  def do_part2(lines)
    data = parse2(lines)
    brute_force_num_wins(data)
  end

  private

  # Reduce the search space by starting in the middle and going down and up
  # in time. As soon as we hit a loss in both directions, we know we're
  # done.
  #
  # This could be made faster by using a binary search to find the two end
  # points of the set of winning games and finding the delta, but that's not
  # worth it. This works and is fast enough for now.
  def brute_force_num_wins(race)
    time, distance = race
    num_wins = 0
    midpoint = time / 2

    i = midpoint
    while i > 0
      distance_at_speed = (time - i) * i
      num_wins += 1 if distance_at_speed > distance
      break if num_wins > 0 && distance_at_speed <= distance

      i -= 1
    end

    i = midpoint + 1
    while true
      distance_at_speed = (time - i) * i
      num_wins += 1 if distance_at_speed > distance
      break if num_wins > 0 && distance_at_speed <= distance

      i += 1
    end

    num_wins
  end

  # Returns an array of time and distance pairs.
  def parse1(lines)
    times = lines[0].split(':')[1].split(' ').map(&:to_i)
    distances = lines[1].split(':')[1].split(' ').map(&:to_i)
    times.zip(distances)
  end

  # Returns an array containing a single time and distance pair.
  def parse2(lines)
    time = lines[0].split(':')[1].gsub(/\s+/, '').to_i
    distance = lines[1].split(':')[1].gsub(/\s+/, '').to_i
    [time, distance]
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
