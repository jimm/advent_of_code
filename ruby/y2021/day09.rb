# Smoke Basin

require 'set'
require_relative '../map'

class Day09 < Day
  def part1
    map = map_from_data(data_lines(1))
    low_point_vals = []
    map.height.times do |row|
      map.width.times do |col|
        val = map.at(row, col)
        other_vals = [
          map.at(row - 1, col),
          map.at(row + 1, col),
          map.at(row, col - 1),
          map.at(row, col + 1)
        ].compact
        low_point_vals << val if val < other_vals.min
      end
    end
    puts low_point_vals.sum + low_point_vals.length
  end

  def part2
    map = map_from_data(data_lines(1))
    basin_cells = Set.new
    basin_sizes = []
    map.height.times do |row|
      map.width.times do |col|
        val = map.at(row, col)
        next if val == 9
        next if basin_cells.include?([row, col])

        basin = find_basin_at(map, row, col)
        basin.each { |cell| basin_cells << cell }
        basin_sizes << basin.size
      end
    end
    puts basin_sizes.sort.reverse[0, 3].reduce(&:*)
  end

  def map_from_data(lines)
    map = Map.new(lines)
    map.height.times do |row|
      map.width.times do |col|
        map.set(row, col, map.at(row, col).to_i)
      end
    end
    map
  end

  # Returns an array of [row, col] cell coordinates that make up the basin.
  def find_basin_at(map, row, col)
    basin = Set.new
    flood_fill_basin(map, basin, row, col)
    basin
  end

  def flood_fill_basin(map, basin, row, col)
    return unless map.in_bounds?(row, col)

    val = map.at(row, col)
    return if val.nil? || val == 9
    return if basin.include?([row, col])

    basin.add([row, col])
    flood_fill_basin(map, basin, row - 1, col)
    flood_fill_basin(map, basin, row + 1, col)
    flood_fill_basin(map, basin, row, col - 1)
    flood_fill_basin(map, basin, row, col + 1)
  end
end
