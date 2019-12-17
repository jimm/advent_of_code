require "../day"

module Year2019
  class Map
    include Enumerable({Int32, Int32})

    getter min_x, min_y, max_x, max_y : Int32

    def initialize
      @map = Hash({Int32, Int32}, Int32).new
      @min_x = @min_y = Int32::MAX
      @max_x = @max_y = Int32::MIN
    end

    def set(xy : {Int32, Int32}, val)
      @map[xy] = val
      x, y = xy
      @min_x = x if x < @min_x
      @min_y = y if y < @min_y
      @max_x = x if x > @max_x
      @max_y = y if y > @max_y
    end

    def set(x : Int32, y : Int32, val)
      set({x, y}, val)
    end

    def get(xy : {Int32, Int32}, default = 0)
      if @map.has_key?(xy)
        @map[xy]
      else
        default
      end
    end

    def get(x : Int32, y : Int32, default = 0)
      get({x, y}, default)
    end

    def width
      @max_x - @min_x + 1
    end

    def height
      @max_y - @min_y + 1
    end

    # Yields to a block with each key ({x, y} coord) in y, x order (row,
    # column).
    def each
      @map.keys
        .sort_by { |xy| xy[1] * @max_y + xy[0] }
        .each { |xy| yield xy }
    end

    # Assumes map is rectangular and that values are character codes.
    def to_s
      String.build do |str|
        (@min_y..@max_y).each do |y|
          (@min_x..@max_x).each do |x|
            str << get(x, y).chr
          end
          str << "\n"
        end
      end
    end
  end

  class Day17 < Day
    def part1
      no_tests

      map = generate_map()
      align_param_sum = map.reduce(0) do |acc, xy|
        if intersection?(map, xy)
          acc + xy[0] * xy[1]
        else
          acc
        end
      end
      puts(align_param_sum)
    end

    def part2
      if @testing
        map = generate_map()
        puts(map.to_s)
        return
      end

      robot_input = (
        "A,B,A,B,C,B,A,C,B,C\n" +
        "L,12,L,8,R,10,R,10\n" +
        "L,6,L,4,L,12\n" +
        "R,10,L,8,L,4,R,10\n"
      ).chars.map { |ch| ch.ord.to_i64 }

      computer = IntcodeComputer.new
      computer.load(data_lines(part_number: 1, testing: false)[0]
        .split(",").map(&.to_i64)
      )
      computer.set(0, 2) # wake up the 'bot
      robot_input.each { |i| computer.append_input(i) }

      computer.append_input('n'.ord.to_i64) # answer "no" to feed prompt
      computer.append_input(10_i64)

      computer.direct_output_to(nil)
      computer.run
      puts(computer.last_output)
    end

    def generate_map
      computer = IntcodeComputer.new
      computer.load(data_lines(part_number: 1, testing: false)[0]
        .split(",").map(&.to_i64)
      )
      output = [] of Int64
      computer.direct_output_to(output)
      computer.run

      newline_loc = output.index(10_i64).as(Int32)
      rows = output[..-2] # double newline in output, rm 2nd
        .map(&.to_i)
        .in_groups_of(newline_loc + 1, 0)
        .map { |row| row[..-2] }

      map = Map.new
      rows.each_with_index do |row, y|
        row.each_with_index do |val, x|
          map.set(x, y, val)
        end
      end
      map
    end

    def intersection?(map, xy)
      x, y = xy
      map.get(x, y) === '#' &&
        map.get(x - 1, y) === '#' &&
        map.get(x + 1, y) === '#' &&
        map.get(x, y - 1) === '#' &&
        map.get(x, y + 1) === '#'
    end

    def print_chars(chars : Array(Int64))
      chars.each { |ch| print(ch.chr) }
    end
  end
end

AoC.register(Year2019::Day17)
