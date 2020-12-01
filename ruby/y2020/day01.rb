# Report Repair

class Day01 < Day
  def part1
    entries = data_lines(1).map(&:to_i)
    entries.each_with_index do |e1, i|
      entries[i+1..-1].each do |e2|
        if e1 + e2 == 2020
          puts(e1 * e2)
          exit(0)
        end
      end
    end
  end

  def part2
    entries = data_lines(1).map(&:to_i)
    entries.each_with_index do |e1, i|
      entries[i+1..-1].each do |e2|
        entries[i+2..-1].each do |e3|
          if e1 + e2 + e3 == 2020
            puts(e1 * e2 * e3)
            exit(0)
          end
        end
      end
    end
  end
end
