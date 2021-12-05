# Hydrothermal Venture

require_relative '../map'
require_relative '../utils'

class Point
  attr_accessor :x, :y

  def initialize(x = 0, y = 0)
    @x = x
    @y = y
  end

  def ==(other)
    @x == other.x && @y == other.y
  end

  def to_s
    "Point(x=#{x}, y=#{y})"
  end
end

class SegmentMap < Map
  def initialize(segments, rook_moves_only: false)
    max_x = max_y = 0
    segments.each do |p0, p1|
      next if rook_moves_only && !rook_move?(p0, p1)

      max_x = p0.x if max_x < p0.x
      max_x = p1.x if max_x < p1.x
      max_y = p0.y if max_y < p0.y
      max_y = p1.y if max_y < p1.y
    end

    @cells = []
    (max_x + 1).times { |row| @cells << ([0] * (max_y + 1)) }
    @wrap_type = nil
    @height = @cells.length
    @width = @cells[0].length
    @row = @col = 0

    segments.each do |p0, p1|
      next if rook_moves_only && !rook_move?(p0, p1)

      add_line_between(p0, p1)
    end
  end

  def overlap_cell_count
    num_overlaps = 0
    @cells.each do |row|
      num_overlaps += row.select { |val| val >= 2 }.count
    end
    num_overlaps
  end

  private

  def add_line_between(p0, p1)
    dx = if p0.x == p1.x
           0
         elsif p0.x < p1.x
           1
         else
           -1
         end
    dy = if p0.y == p1.y
           0
         elsif p0.y < p1.y
           1
         else
           -1
         end
    while p0 != p1
      move_to(p0.y, p0.x)
      incr(@row, @col)
      p0.x += dx
      p0.y += dy
    end
    move_to(p1.y, p1.x)
    incr(@row, @col)
  end

  def incr(row, col)
    @cells[row][col] = (at(row, col) || 0) + 1
  end

  def rook_move?(p0, p1)
    p0.x == p1.x || p0.y == p1.y
  end

  def minmax(a, b)
    a < b ? a.upto(b) : b.upto(a)
  end
end

class Day05 < Day
  def part1
    do_part(rook_moves_only: true)
  end

  def part2
    do_part(rook_moves_only: false)
  end

  def do_part(rook_moves_only: false)
    lines = data_lines(1)
    segments = lines.map do |line|
      line =~ /(\d+),(\d+) -> (\d+),(\d+)/
      [Point.new(Regexp.last_match(1).to_i, Regexp.last_match(2).to_i),
       Point.new(Regexp.last_match(3).to_i, Regexp.last_match(4).to_i)]
    end
    map = SegmentMap.new(segments, rook_moves_only: rook_moves_only)
    puts if @testing
    puts map if @testing
    puts map.overlap_cell_count
  end
end
