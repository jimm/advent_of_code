# An infinite N-dimensional Conway's Life cube. Cells are on or off. On
# cells are represented as an N-element array of [x, y, z, ...] coordinates
# and are kept in a Set.

require 'set'

class ConwayCube
  attr_reader :num_dimensions, :cells

  # Given `lines` of text representing a 2D slice, initializes cells on the
  # [x, y] plane.
  def initialize(num_dimensions, lines, wrap_type=nil)
    @num_dimensions = num_dimensions
    @cells = Set.new
    @snapshot = nil

    lines.each_with_index do |line, x|
      y = 0
      line.each_char do |ch|
        if ch == '#'
          coords = [x, y]
          while coords.length < @num_dimensions
            coords << 0
          end
          @cells << coords
        end
        y += 1
      end
    end
  end

  def alive?(coords, cells=@cells)
    cells.include?(coords)
  end

  def alive_count
    @cells.length
  end

  # Returns all neighbors of [x, y, z] that are "alive".
  def live_neighbor_count(coords, cells=@cells)
    num_alive_neighbors = 0
    minmax = coords.map { |dim_coord| [dim_coord-1, dim_coord+1] }
    all_coords_in(minmax).each do |neighbor_coord|
      if coords != neighbor_coord && cells.include?(neighbor_coord)
        num_alive_neighbors += 1 
      end
    end
    num_alive_neighbors
  end

  def next_generation
    @snapshot = @cells
    @cells = Set.new
    minmax = snapshot_boundaries()
    # Expand dimensions by 1 in each direction so we can check empty cells
    # surrounding all live ones.
    minmax.each do |dim_minmax|
      dim_minmax[0] -= 1
      dim_minmax[1] += 1
    end
    all_coords_in(minmax).each do |coord|
      alive_nbr_count = live_neighbor_count(coord, @snapshot)
      if alive?(coord, @snapshot)
        @cells << coord if alive_nbr_count == 2 || alive_nbr_count == 3
      else
        @cells << coord if alive_nbr_count == 3
      end
    end
  end

  # Returns an array of all coordinates inside `minmax`. `minmax` is an
  # array of [min, max] pairs, one pair per dimension.
  def all_coords_in(minmax, coord=[], coords=[])
    if minmax.empty?
      coords << coord
    else
      min, max = minmax[0]
      (min..max).each do |dim_coord|
        all_coords_in(minmax[1..-1], coord + [dim_coord], coords)
      end
    end
    coords
  end

  # Returns an array of [min, max] values, one pair for each dimension.
  def snapshot_boundaries
    minmax = @snapshot.first.map { |dim_coord| [dim_coord, dim_coord] }
    @snapshot.each do |coord|
      coord.each_with_index do |dim_coord, i|
        minmax[i][0] = dim_coord if dim_coord < minmax[i][0]
        minmax[i][1] = dim_coord if dim_coord > minmax[i][1]
      end
    end
    minmax
  end
end
