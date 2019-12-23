require "../day"

module Year2019
  class Day23 < Day
    def initialize(@part_number : Int32, @testing : Bool)
      super
      @computers = [] of IntcodeComputer
      @outputs = [] of Channel(Int64)
      @answer_chan = Channel(Int64).new
    end

    def part1
      no_tests

      computers_and_outputs = create_computers_and_outputs()
      @computers = computers_and_outputs.map(&.first)
      @outputs = computers_and_outputs.map(&.last)
      @computers.each_with_index do |computer, i|
        spawn do
          computer.run
        end
        spawn do
          listen(computer.output_io.as(Channel(Int64)), i)
        end
      end
      Fiber.yield
      puts(@answer_chan.receive)
    end

    def part2
    end

    def listen(output, i)
      while true
        dest = output.receive.to_i
        x = output.receive
        y = output.receive
        if dest == 255
          @answer_chan.send(y)
          return
        end
        computer = @computers[dest]
        computer.append_input(x)
        Fiber.yield
        computer.append_input(y)
        Fiber.yield
      end
    end

    # Creates and returns an array of {computer, output channel} tuples.
    def create_computers_and_outputs
      program = data_lines()[0].split(",").map(&.to_i64)
      (0...50).map do |i|
        nic = IntcodeComputer.new("NIC #{i}")
        nic.load(program)
        nic.append_input(i.to_i64)
        output = Channel(Int64).new
        nic.direct_output_to(output)
        {nic, output}
      end
    end
  end
end

AoC.register(Year2019::Day23)
