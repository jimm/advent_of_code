# Jurassic Jigsaw

require_relative '../map'
require_relative '../utils'     # for debug{i,}


class Integer
  def to_b(len=10)
    "%0#{len}b" % self
  end
end


class Tile
  @@reversals = {}

  attr_reader :id, :top, :right, :bottom, :left
  attr_accessor :top_tile, :right_tile, :bottom_tile, :left_tile

  # Given an `id` number and `lines` of text, creates a Map and initializes
  # the numerical representations of the four sides' characters.
  def initialize(id, lines)
    @id = id
    @map = Map.new(lines)
    @top = row_to_number(0)
    @right = column_to_number(@map.width - 1)
    @bottom = row_to_number(@map.height - 1)
    @left = column_to_number(0)
    @rotation_cache = {}
    @flip_cache = {}
    @frozen = false
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
    return false unless @top_tile.nil? && other.bottom_tile.nil? && @top == other.bottom

    @top_tile = other
    other.bottom_tile = self

    if @left_tile && @left_tile.top_tile
      other.left_tile = @left_tile.top_tile
      @left_tile.top_tile.right_tile = other
    end
    if @right_tile && @right_tile.top_tile
      other.right_tile = @right_tile.top_tile
      @right_tile.top_tile.left_tile = other
    end
    if other.left_tile && other.left_tile.bottom_tile
      @left_tile = other.left_tile.bottom_tile
      other.left_tile.bottom_tile.right_tile = self
    end
    if other.right_tile && other.right_tile.bottom_tile
      @right_tile = other.right_tile.bottom_tile
      other.right_tile.bottom_tile.left_tile = self
    end

    true
  end

  def maybe_attach_right(other)
    return false unless @right_tile.nil? && other.left_tile.nil? && @right == other.left

    @right_tile = other
    other.left_tile = self

    if @top_tile && @top_tile.right_tile
      other.top_tile = @top_tile.right_tile
      @top_tile.right_tile.bottom_tile = other
    end

    if @bottom_tile && @bottom_tile.right_tile
      other.bottom_tile = @bottom_tile.right_tile
      @bottom_tile.right_tile.top_tile = other
    end

    if other.top_tile && other.top_tile.left_tile
      @top_tile = other.top_tile.left_tile
      other.top_tile.left_tile.bottom_tile = self
    end

    if other.bottom_tile && other.bottom_tile.left_tile
      @bottom_tile = other.bottom_tile.left_tile
      other.bottom_tile.left_tile.top_tile = self
    end

    return true
  end

  def maybe_attach_bottom(other)
    return false unless @bottom_tile.nil? && other.top_tile.nil? && @bottom == other.top

    @bottom_tile = other
    other.top_tile = self

    if @left_tile && @left_tile.bottom_tile
      other.left_tile = @left_tile.bottom_tile
      @left_tile.bottom_tile.right_tile = other
    end
    if @right_tile && @right_tile.bottom_tile
      other.right_tile = @right_tile.bottom_tile
      @right_tile.bottom_tile.left_tile = other
    end
    if other.left_tile && other.left_tile.top_tile
      @left_tile = other.left_tile.top_tile
      other.left_tile.top_tile.right_tile = self
    end
    if other.right_tile && other.right_tile.top_tile
      @right_tile = other.right_tile.top_tile
      other.right_tile.top_tile.left_tile = self
    end

    return true
  end

  def maybe_attach_left(other)
    return false unless @left_tile.nil? && other.right_tile.nil? && @left == other.right

    @left_tile = other
    other.right_tile = self

    if @top_tile && @top_tile.left_tile
      other.top_tile = @top_tile.left_tile
      @top_tile.left_tile.bottom_tile = other
    end
    if @bottom_tile && @bottom_tile.left_tile
      other.bottom_tile = @bottom_tile.left_tile
      @bottom_tile.left_tile.top_tile = other
    end
    if other.top_tile && other.top_tile.right_tile
      @top_tile = other.top_tile.right_tile
      other.top_tile.right_tile.bottom_tile = self
    end
    if other.bottom_tile && other.bottom_tile.right_tile
      @bottom_tile = other.bottom_tile.right_tile
      other.bottom_tile.right_tile.top_tile = self
    end
    return true
  end

  def corner?
    attached_count == 2
  end

  def attached_count
    [@top_tile, @right_tile, @bottom_tile, @left_tile].compact.length
  end

  # Does a 90-degree clockwise rotation.
  def rotate
    return if @frozen

    cache_key = [@top, @right, @bottom, @left]
    answer = @rotation_cache[cache_key]
    if answer.nil?
      answer = [reverse_number(@left), @top, reverse_number(@right), @bottom]
      @rotation_cache[cache_key] = answer
    end
    @top, @right, @bottom, @left = *answer
  end

  # Does a top/bottom flip.
  def flip
    return if @frozen

    cache_key = [@top, @right, @bottom, @left]
    answer = @flip_cache[cache_key]
    if answer.nil?
      answer = [@bottom, reverse_number(@right), @top, reverse_number(@left)]
      @flip_cache[cache_key] = answer
    end
    @top, @right, @bottom, @left = *answer
  end

  # ================ helpers ================

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
    answer = @@reversals[n]
    if answer.nil?
      answer = ("%0#{@map.width}b" % n).reverse.to_i(2)
      @@reversals[n] = answer
    end
    answer
  end

  def p_sides
  end
end


class Day20 < Day
  def part1
    answer = do_part1()
    puts(answer)
  end

  def part1_tests
    run_one_test(20899048083289) do |expected|
      answer = do_part1()
      [answer == expected, answer]
    end
  end

  def do_part1
    tiles = parse()
    attach_tiles(tiles, Math.sqrt(tiles.length).to_i)

    corners = tiles.select(&:corner?)

    top_lefts = tiles.select { |t| t.top_tile.nil? && t.left_tile.nil? && t.bottom_tile && t.right_tile }
    top_left = top_lefts.first

    return corners.map(&:id).reduce(&:*)
  end

  def part2
    lines = data_lines(1)
  end

  def attach_tiles(tiles, square_size)
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
