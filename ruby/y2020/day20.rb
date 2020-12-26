# Jurassic Jigsaw

require_relative '../map'
require_relative '../utils'     # for debug{i,}


class Tile
  NUM_ORIENTATIONS = 8
  # OPPOSITES = {top: :bottom, bottom: :top, right: :left, left: :right}
  # ORTHOGONAL = {
  #   top: [:left, :right], bottom: [:left, :right],
  #   left: [:top, :bottom], right: [:top, :bottom]
  # }

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
    @bottom = reverse_number(row_to_number(@map.height - 1))
    @left = reverse_number(column_to_number(0))
    @rotation_cache = {}
    @flip_cache = {}
  end

  def l; @left_tile; end
  def r; @right_tile; end
  def t; @top_tile; end
  def b; @bottom_tile; end
  def l=(t); @left_tile = t; end
  def r=(t); @right_tile = t; end
  def t=(t); @top_tile = t; end
  def b=(t); @bottom_tile = t; end

  def maybe_attach(other)
    if t.nil? && other.b.nil? && top == other.bottom
      t = other
      other.b = self

      if l && l.t
        o.l = l.t
        l.t.r = other
      end
      if r && r.t
        other.r = r.t
        r.t.l = other
      end
      if other.l && other.l.b
        l = other.l.b
        other.l.b.r = self
      end
      if other.r && other.r.b
        r = other.r.b
        other.r.b.l = self
      end

      return true
    end

    if r.nil? && other.l.nil? && right == other.left
      r = other
      other.l = self

      if t && t.r
        other.t = t.r
        t.r.b = other
      end

      if b && b.r
        other.b = b.r
        b.r.t = other
      end

      if other.t && other.t.l
        t = other.t.l
        other.t.l.b = self
      end

      if other.b && other.b.l
        b = other.b.l
        other.b.l.t = self
      end

      return true
    end

    if b.nil? && other.t.nil? && bottom == other.top
      b = other
      other.t = self

      if l && l.b
        other.l = l.b
        l.b.r = other
      end
      if r && r.b
        other.r = r.b
        r.b.l = other
      end
      if other.l && other.l.t
        l = other.l.t
        other.l.t.r = self
      end
      if other.r && other.r.t
        r = other.r.t
        other.r.t.l = self
      end

      return true
    end

    if l.nil? && other.r.nil? && left == other.right
      l = other
      other.r = self

      if t && t.l
        other.t = t.l
        t.l.b = other
      end
      if b && b.l
        other.b = b.l
        b.l.t = other
      end
      if other.t && other.t.r
        t = other.t.r
        other.t.r.b = self
      end
      if other.b && other.b.r
        b = other.b.r
        other.b.r.t = self
      end
      return true
    end

    false
  end

  def clear_connections
    @top_tile = @right_tile = @bottom_tile = @left_tile = nil
  end

  # Returns true if this tile is attached to any other tile.
  def attached?
    attached_count > 0
  end

  def attached_count
    [top_tile, right_tile, bottom_tile, left_tile].compact.length
  end

  # Does a 90-degree clockwise rotation.
  def rotate
    cache_key = [@top, @right, @bottom, @left]
    answer = @rotation_cache[cache_key]
    if answer.nil?
      answer = reverse_number(@left), @top, reverse_number(@right), @bottom
      @rotation_cache[cache_key] = answer
    end
    @top, @right, @bottom, @left = *answer
  end

  # Does a top/bottom flip.
  def flip
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
    return answer if answer

    answer = ("%0#{@map.width}b" % n).reverse.to_i(2)
    @@reversals[n] = answer
    answer
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

    corners = tiles.select { |t| t.attached_count == 2 }
    pp tiles.length                                # DEBUG
    pp tiles.map(&:attached_count).tally           # DEBUG
    $stderr.puts "found #{corners.length} corners" # DEBUG
    return corners.map(&:id).reduce(&:*)
  end

  def part2
    lines = data_lines(1)
  end

  def attach_tiles(tiles, square_size)
    queue = []
    loose = tiles.dup
    queue << loose.pop
    until queue.empty?
      current = queue.pop
      found = false
      loose.each do |t|
        2.times do
          break if found
          t.flip
          4.times do
            if current.maybe_attach(t)
              queue << loose.delete(t)
              found = true
              break
            end
            t.rotate
          end
        end
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
