require "../day"

module Year2019
  class Day02 < Day
    @initial_memory : Array(Int64)

    def initialize(part_number : Int32, testing : Bool)
      super
      lines = data_lines(
        part_number: 1,
        testing: @part_number == 1 ? @testing : false
      )
      @initial_memory = lines[0].split(",").map(&.to_i64)
      @computer = IntcodeComputer.new
      @computer.load(@initial_memory)
    end

    def part1
      if !@testing
        @computer.set(1_i64, 12_i64)
        @computer.set(2_i64, 2_i64)
      end
      @computer.run
      puts(@computer.get(0))
    end

    def part2
      (0...99).each do |noun|
        (0..99).each do |verb|
          @computer.load(@initial_memory)
          @computer.set(1_i64, noun.to_i64)
          @computer.set(2_i64, verb.to_i64)
          @computer.run
          if @computer.get(0) == 19690720_i64
            puts(100 * noun + verb)
            return
          end
        end
      end
      puts("failed: no noun/verb found that produces magic number")
    end
  end
end

AoC.register(Year2019::Day02)
