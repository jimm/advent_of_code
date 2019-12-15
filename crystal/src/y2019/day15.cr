require "../day"

module Year2019
  class Day15 < Day
    enum DroidMove
      None
      North
      South
      West
      East
    end

    enum DroidStatus
      HitWall
      Moved
      MovedAndFoundOxygen
    end

    # Go on a random walk until we stumble across the oxygen tank. Then
    # compute the A* path length given the new map we've created.
    def part1
    end

    def part2
    end
  end
end

AoC.register(Year2019::Day15)
