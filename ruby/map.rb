# A two-dimensional map where each cell is a character. There is also a
# single convenience "cursor" consisting of writable row and column indexes.
#
# [0, 0] is at the top left of the map. @row increases down, @col increases
# to the right.
class Map
  attr_reader :cell, :height, :width
  attr_accessor :row, :col

  # Given lines of text, initializes a two-dimensional map of characters.
  # Assumes all lines are the same length.
  def initialize(lines)
    @cell = lines.map { |line| line.split('') }
    @height = @cell.length
    @width = @cell[0].length
    @row = @col = 0
  end

  # Returns the character at [row][col].
  def at(row=@row, col=@col)
    @cell[row][col]
  end

  # A convenience method that returns `at(@row, @col)`
  def here
    at(@row, @col)
  end

  # Move to row and col. wrap_type may be nil, :both, :row, or :col.
  def move_to(row, col, wrap_type=nil)
    @row = row
    @col = col
    wrap(wrap_type)
  end

  # Move by deltas. wrap_type may be nil, :both, :row, or :col.
  def move_by(row_delta, col_delta, wrap_type=nil)
    @row += row_delta
    @col += col_delta
    wrap(wrap_type)
  end

  # wrap_type may be nil, :both, :row, or :col.
  def wrap(wrap_type)
    return if wrap_type.nil?
    case wrap_type
    when :both
      @row = @row % @height
      @col = @col % @width
    when :row
      @row = @row % @height
    when :col
      @col = @col % @width
    end
  end
end
