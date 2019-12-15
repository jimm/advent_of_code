require "../day"

module Year2019
  enum ArcadeTile
    Empty
    Wall
    Block
    Horizontal
    Ball
  end

  class Day13 < Day
    def initialize(@part_number : Int32, @testing : Bool)
      super
      program = data_lines(part_number: 1)[0].split(',').map(&.to_i64)
      @chan = Channel(Int64).new

      @computer = IntcodeComputer.new
      @computer.load(program)
      @computer.direct_output_to(@chan)
    end

    def part1
      raise "no test for part 1" if @testing
      spawn do
        @computer.run
        # signal end of program run
        3.times { |_| @chan.send(-1) }
      end

      tiles = {} of Point2 => ArcadeTile
      while true
        x = @chan.receive
        y = @chan.receive
        tile_type = @chan.receive
        break if tile_type == -1
        tiles[Point2.new(x.to_i, y.to_i)] = ArcadeTile.new(tile_type.to_i)
      end
      puts tiles.values.select(&.block?).size
    end

    def part2
      raise "no test for part 1" if @testing
      @computer.dump_memory
      @computer.set(0_i64, 2_i64) # insert two quarters
      spawn do
        @computer.run
        # FIXME
        # signal end of program run
        3.times { |_| @chan.send(-1) }
      end

      tiles = {} of Point2 => ArcadeTile
      while true
        x = @chan.receive.to_i
        y = @chan.receive.to_i
        val = @chan.receive.to_i
        loc = Point2.new(x, y)
        if loc.x == -1 && loc.y == 0
          print_tiles(tiles)
          puts("score: #{val}")
          print("(l,r,.,q)> ")
          str = gets.as(String).strip
          case str
          when "l"
            @computer.append_input(-1_i64)
            Fiber.yield
          when "r"
            @computer.append_input(1_i64)
            Fiber.yield
          when "."
            @computer.append_input(0_i64)
            Fiber.yield
          when "q"
            break
          end
        else
          break if val == -1
          tiles[loc] = ArcadeTile.new(val)
        end
      end
    end

    def print_tiles(tiles)
      min_x, max_x = tiles.keys.map(&.x).minmax
      min_y, max_y = tiles.keys.map(&.y).minmax
      (min_y..max_y).each do |y|
        (min_x..max_x).each do |x|
          tile = tiles[{x, y}]
          case
          when tile.empty?
            print(" ")
          when tile.wall?
            print("#")
          when tile.block?
            print("*")
          when tile.horizontal?
            print("_")
          when tile.ball?
            print("O")
          end
        end
        puts()
      end
    end
  end
end

AoC.register(Year2019::Day13)
