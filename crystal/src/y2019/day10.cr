require "../day"

module Year2019
  alias Asteroid = Tuple(Int32, Int32)
  alias Angle = Float64

  class Map
    getter rows : Int32
    getter cols : Int32
    getter asteroids : Array(Asteroid)
    getter map_strings : Array(String)

    def initialize(@map_strings)
      @rows = map_strings.size
      @cols = map_strings[0].size
      @asteroids = [] of Asteroid
      @map_strings.each_with_index do |row_str, y|
        row_str.each_char_with_index do |char, x|
          if char != '.'
            @asteroids << {x, y}
          end
        end
      end
    end

    def asteroid_char_at(a : Asteroid)
      @map_strings[a[1]].char_at(a[0])
    end
  end

  class Test
    getter best : Asteroid
    getter other_count : Int32
    getter map : Map

    def initialize(@best, @other_count, @map)
    end
  end

  class Day10 < Day
    def part1
      if @testing
        ok = true
        tests = parse_test1_input()
        tests.each do |tg|
          ok &&= run_test1(tg)
        end
        puts("ok") if ok # errors already printed
      else
        map = Map.new(data_lines())
        winner = part1_find_max_seen(map)
        puts(winner[1])
      end
    end

    def part2
      if @testing
        ok = true
        laser_at = {11, 13}
        expected, map = parse_test2_input()

        all_angles, distances = calc_angles_and_distances(map.asteroids)
        angles_to_others = all_angles[laser_at] # Hash(Float64, Asteroid)
        sorted_angles = angles_to_others.keys.sort
        max_index = expected.map { |e| e[0] }.max

        index = 0
        first_angle_found = false
        sorted_angles.cycle do |angle|
          if !first_angle_found
            next if angle < 0
            first_angle_found = true
          end
          if index > max_index
            break
          end

          if angles_to_others[angle].empty?
            index += 1
            next
          end

          # find nearest and, if thre is one, vaporize it
          a = angles_to_others[angle].min_by { |a| distances[{laser_at, a}] }
          next if a.nil?

          angles_to_others[angle].delete(a)
          # compare with expected
          if expected.has_key?(index)
            expected_vaporized = expected[index]
            if expected_vaporized.as(Asteroid) != a
              puts("error: at index #{index} expected #{expected_vaporized} but saw #{a}")
              ok = false
            end
          end

          index += 1
        end
        if ok
          puts("ok")
        end
      else
        map = Map.new(data_lines(part_number: 1))
      end
    end

    def angle_between(p1, p2)
      x_len = (p2[0] - p1[0]).to_f
      y_len = (p2[1] - p1[1]).to_f
      Math.atan2(y_len, x_len)
    end

    def square_of_distance_between(p1, p2)
      (p2[0] - p1[0]).abs.to_f * (p2[1] - p1[1]).abs.to_f
    end

    def calc_angles_and_distances(asteroids) : Tuple(Hash(Asteroid, Hash(Angle, Array(Asteroid))), Hash(Tuple(Asteroid, Asteroid), Float64))
      inner_proc = ->(h : Hash(Angle, Array(Asteroid)), k : Angle) do
        h[k] = [] of Asteroid
      end
      outer_proc = ->(h : Hash(Asteroid, Hash(Angle, Array(Asteroid))), k : Asteroid) do
        h[k] = Hash(Angle, Array(Asteroid)).new(inner_proc)
      end
      angles_between = Hash(Asteroid, Hash(Angle, Array(Asteroid))).new(outer_proc)
      distances_between = {} of Tuple(Asteroid, Asteroid) => Float64

      asteroids.combinations(2).each do |cons|
        a1, a2 = cons

        angles_between[a1][angle_between(a1, a2)] << a2
        angles_between[a2][angle_between(a2, a1)] << a1

        dist = square_of_distance_between(a1, a2)
        distances_between[{a1, a2}] = dist
        distances_between[{a2, a1}] = dist
      end
      {angles_between, distances_between}
    end

    # Returns the `{Asteroid, others_seen}` tuple of the asteroid in *map*
    # that has max `others_seen`.
    def part1_find_max_seen(map)
      asteroids = map.asteroids

      # Calculate angles and distances between asteroids
      angles_between, distances_between = calc_angles_and_distances(asteroids)

      winner = asteroids.map do |a|
        # The simple fact that there is a angle between this asteroid and
        # one or more others means that it can see one of them (the
        # closest). We don't care which is closest right now.
        count_seen = angles_between[a].size
        {a, count_seen}
      end
        .max_by { |a_and_count| a_and_count[1] }
    end

    def run_test1(test)
      winner = part1_find_max_seen(test.map)

      expected = {test.best, test.other_count}
      if winner != expected
        puts("error: expected #{expected} but saw #{winner}")
        return false
      end
      true
    end

    # Reads the test1 input file and returns an array of `Test`s.
    def parse_test1_input
      best_regex = /Best is ([\d,]+) with (\d+) /
      data_lines()
        .chunks { |line| line.starts_with?("Best") }
        .map(&.last)
        .in_groups_of(2)
        .map do |meta_and_map|
          best_regex.match(meta_and_map[0].as(Array(String))[0])
          best = $1.split(",").map(&.to_i)
          other_count = $2.to_i
          Test.new(
            {best[0], best[1]},
            other_count,
            Map.new(meta_and_map[1].as(Array(String)))
          )
        end
    end

    # Reads the test2 input file and returns a `Hash(Int32, Asteroid)`
    # and a `Map`, together as a `Tuple`.
    def parse_test2_input
      expected_regex = /The (\d+).*is at (\d+),(\d+)/
      expected_chunk, map_chunk = data_lines().chunks { |line| line.starts_with?(";") }
      expected = expected_chunk[1].each_with_object(Hash(Int32, Asteroid).new) do |line, h|
        expected_regex.match(line)
        h[$1.to_i - 1] = {$2.to_i, $3.to_i} # "1" => index 0
      end
      map = Map.new(map_chunk[1])
      {expected, map}
    end
  end
end

AoC.register(Year2019::Day10)
