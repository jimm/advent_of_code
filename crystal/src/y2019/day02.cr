require "../day"

module Year2019
  class Day02 < Day
    @initial_memory : Array(Int32)

    def initialize(part_number : Int32, testing : Bool)
      super
      lines = data_lines(
        part_number: 1,
        testing: @part_number == 1 ? @testing : false
      )
      @initial_memory = lines[0].split(",").map { |s| s.to_i }
      @computer = IntcodeComputer.new
      @computer.load(@initial_memory)
    end

    def part1
      if !@testing
        @computer.set(1, 12)
        @computer.set(2, 2)
      end
      @computer.run
      puts(@computer.get(0))
    end

    def part2
      (0...99).each do |noun|
        (0..99).each do |verb|
          @computer.load(@initial_memory)
          @computer.set(1, noun)
          @computer.set(2, verb)
          @computer.run
          if @computer.get(0) == 19690720
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
