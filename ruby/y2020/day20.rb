# Jurassic Jigsaw

require_relative '../map'
require_relative '../utils'     # for debug{i,}


class Tile
  NUM_ORIENTATIONS = 12

  attr_reader :id
  attr_accessor :orientation_index, :top_tile, :right_tile, :bottom_tile, :left_tile

  # Given an `id` number and `lines` of text, creates a Map and initializes
  # the numerical representations of the four sides' characters.
  def initialize(id, lines)
    @id = id
    @map = Map.new(lines)
    @orientation_index = 0
    @orientations = generate_orientations(
      row_to_number(0), column_to_number(@map.width - 1),
      row_to_number(@map.height - 1), column_to_number(0)
    )
  end

  def top; @orientations[@orientation_index][0]; end
  def right; @orientations[@orientation_index][1]; end
  def bottom; @orientations[@orientation_index][2]; end
  def left; @orientations[@orientation_index][3]; end
  def borders; @orientations[@orientation_index]; end

  # Returns true if this tile is attached to any other tile.
  def attached?
    [top_tile, right_tile, bottom_tile, left_tile].any?
  end

  # Returns the number of border matches given our current orientation. OK
  # if any tile is nil.
  def num_matches(top_tile, right_tile, bottom_tile, left_tile)
    num = 0
    num += 1 if top_tile && top_tile.bottom == top
    num += 1 if right_tile && right_tile.left == right
    num += 1 if bottom_tile && bottom_tile.top == bottom
    num += 1 if left_tile && left_tile.right == left
    num
  end

  # ================ helpers ================

  # Generates and returns an array of TLBR arrays, one for each possible
  # combination of rotation and flip.
  #
  # Note that we don't have to return both a single T/B and single L/R flip,
  # because those are just reorderings of each other.
  def generate_orientations(top, right, bottom, left)
    r0 = [top, right, bottom, left]
    r1 = rotate(*r0)
    r2 = rotate(*r1)
    r3 = rotate(*r2)
    f0 = top_bottom_flip(top, right, bottom, left)
    f1 = rotate(*f0)
    f2 = rotate(*f1)
    f3 = rotate(*f2)
    ff0 = left_right_flip(*f0)
    ff1 = rotate(*ff0)
    ff2 = rotate(*ff1)
    ff3 = rotate(*ff2)
    [r0, r1, r2, r3, f0, f1, f2, f3, ff0, ff1, ff2, ff3]
  end

  # Does a 90-degree clockwise rotation and returns new [top, right, bottom,
  # left].
  def rotate(top=@top, right=@right, bottom=@bottom, left=@left)
    [reverse_number(left), top, reverse_number(right), bottom]
  end

  # Does a top/bottom flip and returns new [top, right, bottom, left].
  def top_bottom_flip(top=@top, right=@right, bottom=@bottom, left=@left)
    [bottom, reverse_number(right), top, reverse_number(left)]
  end

  # Does a left/right flip and returns new [top, right, bottom, left].
  def left_right_flip(top=@top, right=@right, bottom=@bottom, left=@left)
    [reverse_number(top), left, reverse_number(bottom), right]
  end

  def row_to_number(n)
    chars_to_number(@map.cells[n])
  end

  def column_to_number(n)
    chars_to_number(@map.cells.map { |row| row[n] })
  end

  def chars_to_number(chars)
    chars.join().gsub('.', '0').gsub('#', '1').to_i(2)
  end

  def reverse_number(n)
    ("%0#{@map.width}b" % n).reverse.to_i(2)
  end
end


class Day20 < Day
  def part1
    answer = do_part1()
    puts(answer)
  end

  def part1_tests
    expected = 20899048083289
    answer = do_part1()
    if answer == expected
      puts('.')
      puts('ok')
    else
      puts('F')
      puts("error: expected #{expected}, got #{answer}")
    end
  end

  def do_part1
    tiles = parse()
    attach_tiles(tiles)

    top_left = tiles.first
    $stderr.puts "starting at tile #{top_left.id}" # DEBUG
    top_left = top_left.top_tile while top_left.top_tile
    top_left = top_left.left_tile while top_left.left_tile

    top_right = top_left
    while top_right.right_tile
      top_right = top_right.right_tile
    end

    bottom_left = top_left
    while bottom_left.bottom_tile
      bottom_left = bottom_left.bottom_tile
    end

    bottom_right = bottom_left
    while bottom_right.right_tile
      bottom_right = bottom_right.right_tile
    end

    $stderr.puts [top_left, top_right, bottom_left, bottom_right].map(&:id) # DEBUG
    [top_left, top_right, bottom_left, bottom_right].map(&:id).reduce(&:*)
  end

  def part2
    lines = data_lines(1)
  end

  def attach_tiles(tiles)
    # FIXME after this pass some tiles next to each other might not be attached
    #
    # for example x -- y
    #             |
    #             z -- w
    #
    # but y and w are not attached
    tiles.each do |unattached|
      unattached.orientation_index = -1
      while !unattached.attached? && unattached.orientation_index < Tile::NUM_ORIENTATIONS
        unattached.orientation_index += 1
        tiles.each do |tile|
          next if unattached == tile
          if unattached.top_tile.nil? && tile.bottom_tile.nil? && unattached.top == tile.bottom
            unattached.top_tile = tile
            tile.bottom_tile = unattached
          elsif unattached.right_tile.nil? && tile.left_tile.nil? && unattached.right == tile.left
            unattached.right_tile = tile
            tile.left_tile = unattached
          elsif unattached.bottom_tile.nil? && tile.top_tile.nil? && unattached.bottom == tile.top
            unattached.bottom_tile = tile
            tile.top_tile = unattached
          elsif unattached.left_tile.nil? && tile.right_tile.nil? && unattached.left == tile.right
            unattached.left_tile = tile
            tile.right_tile = unattached
          end
        end
      end
      unless unattached.attached?
        puts "error: tile #{unattached.id} doesn't attach with any rotation"
        next
      end
    end
  end

  def parse
    tiles = []
    tile_id = nil
    lines = []
    data_lines(1).each do |line|
      if line =~ /Tile (\d+)/
        if tile_id
          tiles << Tile.new(tile_id, lines)
        end
        tile_id = $1.to_i
        lines = []
      else
        lines << line
      end
    end
    tiles << Tile.new(tile_id, lines)
  end
end
