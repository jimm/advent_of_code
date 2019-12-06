require "../util"

# Orbits are stored as a hash where key = orbiting body and val = inner
# body.
class Day06
  def initialize(@part_number : Int32, @testing : Bool)
    @orbits = {} of String => String
    lines = Util.data_file_lines(
      2019, 6, @testing ? @part_number : 1, @testing
    )
    @orbits = create_orbit_map(lines)
  end

  def run
    if @part_number == 1
      part1()
    else
      # 1137 is too high
      part2()
    end
  end

  def part1
    num_orbits = @orbits.keys.sum { |k| path_to_com_from(k).size }

    if @testing
      if num_orbits == 42
        puts("ok")
      else
        puts("error: expected 42, saw #{num_orbits}")
      end
    else
      puts(num_orbits)
    end
  end

  def part2
    start = @orbits["YOU"]
    target = @orbits["SAN"]
    my_path_to_com = path_to_com_from("YOU")
    santa_path_to_com = path_to_com_from("SAN")
    innermost = common_ancestor(my_path_to_com, santa_path_to_com)
    path = moves_in(my_path_to_com, to: innermost)
    path += moves_out(santa_path_to_com, from: innermost, to: target)

    if @testing
      path_str = path.join("")
      if path.size == 4 && path_str == "JEDI"
        puts("ok")
      else
        puts(%(error: expected "JEDI", saw "#{path_str}"))
      end
    else
      puts(path.size)
    end
  end

  # Returns path from `obj` to COM, not including `obj`.
  def path_to_com_from(obj)
    path = [] of String
    until obj.nil?
      obj = @orbits[obj]?
      path << obj if obj
    end
    path
  end

  def common_ancestor(path1, path2)
    # O(n^2) but we don't care
    path1.each do |obj|
      if path2.includes?(obj)
        return obj
      end
    end
    "COM"
  end

  # Returns objs in between start (second item in path, first is "YOU") and
  # `to` exclusive.
  def moves_in(path, to)
    moves = path[1..].take_while { |obj| obj != to }
  end

  # Returns objs from `from` to `to` inclusive.
  def moves_out(path, from, to)
    moves = path
      .reverse
      .skip_while { |obj| obj != from }
      .take_while { |obj| obj != to }
    moves << to
  end

  def create_orbit_map(lines)
    lines.each_with_object({} of String => String) do |line, h|
      inner, outer = line.split(")")
      h[outer] = inner
    end
  end
end

AoC.register("2019.6", ->(part_number : Int32, testing : Bool) do
  Day06.new(part_number, testing).run
end)
