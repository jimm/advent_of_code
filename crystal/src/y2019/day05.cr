require "../day"

module Year2019
  class Day05 < Day
    @initial_memory : Array(Int32)

    def initialize(part_number : Int32, testing : Bool)
      super
      lines = data_lines(
        part_number: @part_number == 1 ? 1 : (@testing ? 2 : 1),
        testing: @part_number == 1 ? false : @testing
      )
      @initial_memory = lines[0].split(",").map { |s| s.to_i }
      @computer = IntcodeComputer.new
      @computer.load(@initial_memory)
    end

    def part1
      @computer.trace(true) if @testing
      @computer.enqueue_input(1)
      @computer.run
    end

    def part2
      if @testing
        {5 => 999, 8 => 1000, 10 => 1001}.each do |input, expected|
          @computer.load(@initial_memory)
          @computer.enqueue_input(input)
          print("expect to see #{expected}: ")
          @computer.run
        end
      else
        @computer.enqueue_input(5)
        @computer.run
      end
    end
  end
end

AoC.register(Year2019::Day05)
