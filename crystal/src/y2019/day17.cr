require "../day"

module Year2019
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

      map = GridMap(Int32).new
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
