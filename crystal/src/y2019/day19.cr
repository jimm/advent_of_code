require "../day"

module Year2019
  class Day19 < Day
    def part1
      no_tests

      computer = IntcodeComputer.new
      computer.load(data_lines()[0].split(",").map(&.to_i64))
      output = Channel(Int64).new
      computer.direct_output_to(output)

      spawn do
        computer.run
        puts("computer has halted") # DEBUG
      end

      num_points = 0
      (0...50).each do |y|
        (0..50).each do |x|
          puts("(#{x}, #{y})") # DEBUG
          val = get_tractor_val_at(computer, output, x, y)
          num_points += 1 if val == 1
        end
      end
      puts(num_points)
      return

      # start_x = 0
      # (0...50).each do |y|
      #   in_beam = false
      #   (start_x...50).each do |x|
      #     val = get_tractor_val_at(computer, output, x, y)
      #     if val == 0
      #       break if in_beam
      #       if !in_beam
      #         in_beam = true
      #         start_x = x
      #       end
      #       num_points += 1
      #     end
      #   end
      # end
      puts num_points
    end

    def part2
    end

    def get_tractor_val_at(computer, output, x, y)
      puts("get_tractor_val_at(#{x}, #{y})") # DEBUG
      computer.append_input(x.to_i64)
      computer.append_input(y.to_i64)
      output.receive.to_i.tap { |val| puts("val received - #{val}") }
    end
  end
end

AoC.register(Year2019::Day19)
