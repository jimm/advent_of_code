require "../day"
require "../types"

module Year2019
  enum Color
    Black
    White
  end

  enum Direction
    Up
    Left
    Down
    Right
  end

  class Hull
    @painted_locs : Hash({Int32, Int32}, Color)

    def initialize
      @painted_locs = {} of {Int32, Int32} => Color
    end

    def paint(loc, color)
      @painted_locs[loc.to_tuple] = color
    end

    def color_at(loc)
      if @painted_locs.keys.includes?(loc)
        @painted_locs[loc]
      else
        Color::Black
      end
    end

    def locs_painted
      @painted_locs.keys
    end

    def print
      min_x, max_x = @painted_locs.keys.map(&.first).minmax
      min_y, max_y = @painted_locs.keys.map(&.last).minmax
      (min_y..max_y).each do |y|
        (min_x..max_x).each do |x|
          print(color_at({x, y}) == Color::White ? "*" : " ")
        end
        puts()
      end
    end
  end

  class PaintingRobot
    HALT = -1_i64

    def initialize(program : Array(Int64), @hull : Hull)
      @loc = Point2.new(0, 0)
      @dir = Direction::Up
      @computer = IntcodeComputer.new
      @computer.load(program)
      @running = false
    end

    def run
      output_stream = Channel(Int64).new
      @computer.direct_output_to(output_stream)
      spawn do
        @computer.run
        output_stream.send(HALT) # signal end of program to robot
      end

      @running = true
      while @running
        cycle(output_stream)
      end
    end

    def cycle(output_stream)
      input_current_color()
      color_int = output_stream.receive
      if color_int == HALT
        @running = false
        return
      end
      turn_int = output_stream.receive
      paint(color_int)
      turn(turn_int)
      move_forward()
    end

    def input_current_color
      @computer.append_input(@hull.color_at(@loc.to_tuple).to_i64)
    end

    def paint(color_int)
      @hull.paint(@loc, Color.new(color_int.to_i))
    end

    def turn(turn_int)
      if turn_int == 0
        @dir = Direction.new((@dir.to_i + 1) % 4)
      else
        @dir = Direction.new((@dir.to_i + 3) % 4)
      end
    end

    def move_forward
      case @dir
      when Direction::Up
        @loc += {0, -1}
      when Direction::Left
        @loc += {-1, 0}
      when Direction::Down
        @loc += {0, 1}
      when Direction::Right
        @loc += {1, 0}
      end
    end
  end

  class Day11 < Day
    def part1
      no_tests
      hull = Hull.new
      program = data_lines()[0].split(",").map(&.to_i64)
      robot = PaintingRobot.new(program, hull)
      robot.run
      puts(hull.locs_painted.size)
    end

    def part2
      no_tests
      hull = Hull.new
      hull.paint(Point2.new(0, 0), Color::White)
      program = data_lines(part_number: 1)[0].split(",").map(&.to_i64)
      robot = PaintingRobot.new(program, hull)
      robot.run
      hull.print
    end
  end
end

AoC.register(Year2019::Day11)
