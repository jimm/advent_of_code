require "../util"

class Day03
  def run(part_number : Int32, testing : Bool)
    lines = Util.data_file_lines(2019, 3, 1, testing)
    wire_directions = lines.map { |line| line.split(",") }
    proc = if part_number == 1
             ->{ part1(wire_directions, testing) }
           else
             ->{ part2(wire_directions, testing) }
           end
    proc.call
  end

  def part1(wire_directions, _testing)
    locs, common_locs = crossovers(wire_directions)
    min_dist = common_locs.map { |loc| manhattan_dist_from_origin(loc) }.min
    puts(min_dist)
  end

  def part2(wire_directions, _testing)
    locs, common_locs = crossovers(wire_directions)
    # This is inefficient (O(n^2), looks up indexes multiple times) but it
    # doesn't take that long to run.
    fastest_dist = common_locs.min_by do |loc|
      dist0 = locs[0].index(loc)
      dist1 = locs[1].index(loc)
      dist0.as(Int32) + dist1.as(Int32)
    end
    combined_steps =
      locs[0].index(fastest_dist).as(Int32) +
        locs[1].index(fastest_dist).as(Int32)
    puts(combined_steps)
  end

  def manhattan_dist_from_origin(loc)
    loc[0].abs + loc[1].abs
  end

  # Returns {locs, common_locs}
  def crossovers(wire_directions)
    locs = wire_directions.map { |moves| locs_from_moves(moves) }
    common_locs = locs[0] & locs[1]
    common_locs.delete(locs[0][0])
    {locs, common_locs}
  end

  def locs_from_moves(moves)
    prev_loc = {0, 0}
    locs = [prev_loc]
    moves.map do |move|
      dir = move[0]
      amt = move[(1..)].to_i
      case dir
      when 'L'
        (0...amt).each do |_|
          prev_loc = {prev_loc[0] - 1, prev_loc[1]}
          locs << prev_loc
        end
      when 'R'
        (0...amt).each do |_|
          prev_loc = {prev_loc[0] + 1, prev_loc[1]}
          locs << prev_loc
        end
      when 'U'
        (0...amt).each do |_|
          prev_loc = {prev_loc[0], prev_loc[1] + 1}
          locs << prev_loc
        end
      when 'D'
        (0...amt).each do |_|
          prev_loc = {prev_loc[0], prev_loc[1] - 1}
          locs << prev_loc
        end
      else
        puts("error: unknown dir #{dir}")
        exit(1)
      end
    end
    locs
  end
end

AoC.register("2019.3", ->(part_number : Int32, testing : Bool) do
  Day03.new.run(part_number, testing)
end)
