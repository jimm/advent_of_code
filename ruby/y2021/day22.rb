# Reactor Reboot

require 'set'

class Day22 < Day
  def part1
    init_instructions = parse(data_lines(1), -50, 50)
    on_set = run_init(init_instructions)
    puts on_set.size
  end

  def part1_tests
    tests(1, -50, 50)
  end

  def part2
    init_instructions = parse(data_lines(1), nil, nil)
    on_set = run_init(init_instructions)
    puts on_set.size
    lines = data_lines(1)
  end

  def part2_tests
    tests(2, nil, nil)
  end

  def tests(part, min_val, max_val)
    run_chunk_tests(part) do |expected, lines|
      expected = eval(expected)[:on]
      init_instructions = parse(lines, min_val, max_val)
      on_set = run_init(init_instructions)
      [expected == on_set.size, on_set.size]
    end
  end

  def run_init(init_instructions)
    s = Set.new
    init_instructions.each do |command, xyz_ranges|
      xyz_ranges[0].each do |x|
        xyz_ranges[1].each do |y|
          xyz_ranges[2].each do |z|
            if command == :on
              s.add([x, y, z])
            else
              s.delete([x, y, z])
            end
          end
        end
      end
    end
    s
  end

  def parse(lines, min_val, max_val)
    lines.map do |line|
      line =~ /(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)/
      [
        Regexp.last_match(1).to_sym,
        xyz_ranges(min_val, max_val, *(2..7).map { |i| Regexp.last_match(i).to_i })
      ]
    end
         .reject { |init| init[1].nil? }
  end

  def xyz_ranges(min_val, max_val, xmin, xmax, ymin, ymax, zmin, zmax)
    return [(xmin..xmax), (ymin..ymax), (zmin..zmax)] if min_val.nil? && max_val.nil?

    return nil if xmin > max_val || ymin > max_val || zmin > max_val ||
                  xmax < min_val || ymax < min_val || zmax < min_val

    [
      ([xmin, min_val].max..[xmax, max_val].min),
      ([ymin, min_val].max..[ymax, max_val].min),
      ([zmin, min_val].max..[zmax, max_val].min)
    ]
  end
end
