#!/usr/bin/env ruby
#
# Jurassic Jigsaw

require_relative '../day'
require_relative '../map'

class Image < Map
  def top
    @cells[0].join
  end

  def right
    @cells.map { |row| row[@width - 1] }.join
  end

  def bottom
    @cells[@height - 1].join
  end

  def left
    @cells.map { |row| row[0] }.join
  end

  # Given a two-dimensional array of `images`, creates and returns a new
  # image comprised of `images`. Assumes borders have been removed.
  def self.from_images(images)
    image_height = images[0][0].height
    lines = Array.new(image_height * images.length) { String.new }
    row_index = 0
    images.each do |images_row|
      images_row.each do |image|
        cells = image.instance_variable_get(:@cells)
        cells.each_with_index do |row, row_offset|
          lines[row_index + row_offset] << row.join
        end
      end
      row_index += image_height
    end

    new(lines)
  end

  # Does a 90-degree clockwise rotation.
  def rotate
    return if @frozen

    n = @width
    (0...n / 2).each do |i|
      (i...n - i - 1).each do |j|
        ni = n - 1 - i
        nj = n - 1 - j
        tmp = @cells[i][j]
        @cells[i][j] = @cells[nj][i]
        @cells[nj][i] = @cells[ni][nj]
        @cells[ni][nj] = @cells[j][ni]
        @cells[j][ni] = tmp
      end
    end
  end

  # Does a top/bottom flip.
  def flip
    @cells.reverse! unless @frozen
  end

  def remove_borders
    @cells = @cells[1..-2].map { |row| row[1..-2] }
    @width -= 2
    @height -= 2
  end

  def count_pixels(ch)
    @cells.flatten.select { |x| x == ch }.count
  end
end

class Tile
  @@reversals = {}

  attr_reader :id, :image
  attr_accessor :top_tile, :right_tile, :bottom_tile, :left_tile

  # Given an `id` number and `lines` of text, creates a Map and initializes
  # the numerical representations of the four sides' characters.
  def initialize(id, lines)
    @id = id
    @image = Image.new(lines)
    @frozen = false
  end

  # Define *_border methods
  %i[top right bottom left].each do |sym|
    define_method("#{sym}_border".to_sym) { @image.send(sym) }
  end

  def remove_borders
    @image.remove_borders
  end

  def freeze_orientation
    @frozen = true
  end

  def maybe_attach(other)
    return true if maybe_attach_top(other)
    return true if maybe_attach_right(other)
    return true if maybe_attach_bottom(other)
    return true if maybe_attach_left(other)

    false
  end

  def maybe_attach_top(other)
    return false unless top_tile.nil? && other.bottom_tile.nil? && top_border == other.bottom_border

    @top_tile = other
    other.bottom_tile = self

    if left_tile && left_tile.top_tile
      other.left_tile = left_tile.top_tile
      left_tile.top_tile.right_tile = other
    end
    if right_tile && right_tile.top_tile
      other.right_tile = right_tile.top_tile
      right_tile.top_tile.left_tile = other
    end
    if other.left_tile && other.left_tile.bottom_tile
      left_tile = other.left_tile.bottom_tile
      other.left_tile.bottom_tile.right_tile = self
    end
    if other.right_tile && other.right_tile.bottom_tile
      right_tile = other.right_tile.bottom_tile
      other.right_tile.bottom_tile.left_tile = self
    end

    true
  end

  def maybe_attach_right(other)
    return false unless right_tile.nil? && other.left_tile.nil? && right_border == other.left_border

    @right_tile = other
    other.left_tile = self

    if top_tile && top_tile.right_tile
      other.top_tile = top_tile.right_tile
      top_tile.right_tile.bottom_tile = other
    end
    if bottom_tile && bottom_tile.right_tile
      other.bottom_tile = bottom_tile.right_tile
      bottom_tile.right_tile.top_tile = other
    end
    if other.top_tile && other.top_tile.left_tile
      top_tile = other.top_tile.left_tile
      other.top_tile.left_tile.bottom_tile = self
    end
    if other.bottom_tile && other.bottom_tile.left_tile
      bottom_tile = other.bottom_tile.left_tile
      other.bottom_tile.left_tile.top_tile = self
    end

    true
  end

  def maybe_attach_bottom(other)
    return false unless bottom_tile.nil? && other.top_tile.nil? && bottom_border == other.top_border

    @bottom_tile = other
    other.top_tile = self

    if left_tile && left_tile.bottom_tile
      other.left_tile = left_tile.bottom_tile
      left_tile.bottom_tile.right_tile = other
    end
    if right_tile && right_tile.bottom_tile
      other.right_tile = right_tile.bottom_tile
      right_tile.bottom_tile.left_tile = other
    end
    if other.left_tile && other.left_tile.top_tile
      left_tile = other.left_tile.top_tile
      other.left_tile.top_tile.right_tile = self
    end
    if other.right_tile && other.right_tile.top_tile
      right_tile = other.right_tile.top_tile
      other.right_tile.top_tile.left_tile = self
    end

    true
  end

  def maybe_attach_left(other)
    return false unless left_tile.nil? && other.right_tile.nil? && left_border == other.right_border

    @left_tile = other
    other.right_tile = self

    if top_tile && top_tile.left_tile
      other.top_tile = top_tile.left_tile
      top_tile.left_tile.bottom_tile = other
    end
    if bottom_tile && bottom_tile.left_tile
      other.bottom_tile = bottom_tile.left_tile
      bottom_tile.left_tile.top_tile = other
    end
    if other.top_tile && other.top_tile.right_tile
      top_tile = other.top_tile.right_tile
      other.top_tile.right_tile.bottom_tile = self
    end
    if other.bottom_tile && other.bottom_tile.right_tile
      bottom_tile = other.bottom_tile.right_tile
      other.bottom_tile.right_tile.top_tile = self
    end
    true
  end

  def corner?
    attached_count == 2
  end

  def attached_count
    [top_tile, right_tile, bottom_tile, left_tile].compact.length
  end

  def to_s
    puts "Tile #{@id}:"
    puts @image
  end

  # ================ helpers ================

  def rotate
    @image.rotate unless @frozen
  end

  def flip
    @image.flip unless @frozen
  end
end

class Day20 < Day
  MONSTER = [
    '                  # ',
    '#    ##    ##    ###',
    ' #  #  #  #  #  #   '
  ]
  MONSTER_HEIGHT = MONSTER.length
  MONSTER_WIDTH = MONSTER[0].length

  def part1
    answer = do_part1
    puts(answer)
  end

  def part1_tests
    run_one_test(20_899_048_083_289) { |_| do_part1 }
  end

  def do_part1
    tiles = parse
    attach_tiles(tiles)

    corners = tiles.select(&:corner?)

    top_lefts = tiles.select { |t| t.top_tile.nil? && t.left_tile.nil? && t.bottom_tile && t.right_tile }
    top_left = top_lefts.first

    corners.map(&:id).reduce(&:*)
  end

  def part2
    answer = do_part2
    # dammit 2787 is too high
    puts(answer)
  end

  def part2_tests
    run_one_test(273) { |_| do_part2 }
  end

  def do_part2
    tiles = parse
    attach_tiles(tiles)
    tiles.each(&:remove_borders)

    # Create one image from many
    image_matrix = []
    top_left_corner = tiles.detect { |t| t.top_tile.nil? && t.left_tile.nil? }
    t = top_left_corner
    while t
      row = []
      row_start = t
      while t
        row << t.image
        t = t.right_tile
      end
      image_matrix << row
      t = row_start.bottom_tile
    end
    image = Image.from_images(image_matrix)

    # Detect monsters
    offsets = monster_segment_offsets
    locs = find_two_monsters(image, offsets)
    locs.each do |row, col|
      offsets.each do |row_offset, col_offset|
        image.set(row + row_offset, col + col_offset, 'O')
      end
    end

    # Return number of remaining waves
    image.count_pixels('#')
  end

  def attach_tiles(tiles)
    loose = tiles.dup
    queue = [loose.shift]
    until queue.empty?
      current = queue.shift
      current.freeze_orientation
      loose.each do |t|
        if maybe_attach(current, t)
          queue << loose.delete(t)
          t.freeze_orientation
        end
      end
    end
  end

  def maybe_attach(t1, t2)
    2.times do
      4.times do
        return true if t1.maybe_attach(t2)

        t2.rotate
      end
      t2.flip
    end
    false
  end

  def monster_segment_offsets
    offsets = []
    MONSTER.each_with_index do |chars, row_offset|
      chars.split('').each_with_index do |ch, col_offset|
        offsets << [row_offset, col_offset] if ch == '#'
      end
    end
    offsets
  end

  def find_two_monsters(image, offsets)
    2.times do
      4.times do
        locs = find_monsters_in_image(image, offsets)
        return locs if locs

        image.rotate
      end
      image.flip
    end
    nil
  end

  def find_monsters_in_image(image, offsets)
    locs = []
    (0..image.height - MONSTER_HEIGHT).each do |row|
      (0..image.width - MONSTER_WIDTH).each do |col|
        locs << [row, col] if monster_at(image, offsets, row, col)
      end
    end
    locs.empty? ? nil : locs
  end

  def monster_at(image, offsets, row, col)
    offsets.all? do |offset|
      image.at(row + offset[0], col + offset[1]) == '#'
    end
  end

  def parse
    tiles = []
    tile_id = nil
    lines = []
    data_lines(1).each do |line|
      if line =~ /Tile (\d+)/
        tiles << Tile.new(tile_id, lines) if tile_id
        tile_id = ::Regexp.last_match(1).to_i
        lines = []
      else
        lines << line
      end
    end
    tiles << Tile.new(tile_id, lines)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
