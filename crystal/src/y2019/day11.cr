require "../day"

module Year2019
  alias Loc = {Int32, Int32}

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
    @painted_locs : Hash(Loc, Array(Color))

    def initialize
      pl_init = ->(h : Hash(Loc, Array(Color)), k : Loc) { h[k] = [] of Color }
      @painted_locs = Hash(Loc, Array(Color)).new(pl_init)
    end

    def paint(loc, color)
      @painted_locs[loc] << color
    end

    def color_at(loc)
      if @painted_locs.keys.includes?(loc)
        @painted_locs[loc].last
      else
        Color::Black
      end
    end

    def locs_painted
      @painted_locs.keys
    end
  end

  class PaintingRobot
    def initialize(program : Array(Int64), @hull : Hull)
      @loc = {0, 0}
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
        output_stream.send(-1) # signal end of program to robot
        output_stream.send(-1)
      end

      @running = true
      while @running
        cycle(output_stream)
      end
    end

    def cycle(output_stream)
      input_current_color()
      color_int = output_stream.receive
      turn_int = output_stream.receive
      if color_int == -1
        @running = false
      else
        paint(color_int)
        turn(turn_int)
        move_forward()
      end
    end

    def input_current_color
      @computer.append_input(@hull.color_at(@loc).to_i64)
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
        @loc = {@loc[0], @loc[1] - 1}
      when Direction::Left
        @loc = {@loc[0] - 1, @loc[1]}
      when Direction::Down
        @loc = {@loc[0], @loc[1] + 1}
      when Direction::Right
        @loc = {@loc[0] + 1, @loc[1]}
      end
    end
  end

  class Day11 < Day
    def part1
      hull = Hull.new
      program = data_lines()[0].split(",").map(&.to_i64)
      robot = PaintingRobot.new(program, hull)
      robot.run
      puts(hull.locs_painted.size)
    end
  end
end

AoC.register(Year2019::Day11)
