# A two-dimensional map where each cell is a single value. There is also a
# single convenience "cursor" consisting of writable row and column indexes.
#
# [0, 0] is at the top left of the map. @row increases down, @col increases
# to the right.
class Map
  attr_reader :cells, :height, :width, :wrap_type
  attr_accessor :row, :col

  # Given `lines` of text, initializes a two-dimensional map of characters.
  # Assumes all lines are the same length.
  #
  # `wrap_type` may be nil, :both, :row, or :col.
  def initialize(lines, wrap_type = nil)
    @cells = lines.map { |line| line.split('') }
    @wrap_type = wrap_type
    @height = @cells.length
    @width = @cells[0].length
    @row = @col = 0
  end

  # Converts each cell's character, presumably a single digit char, to an
  # integer.
  def cells_to_ints!
    @height.times do |row|
      @cells[row].map!(&:to_i)
    end
  end

  # Returns the value at [row][col].
  def at(row = @row, col = @col)
    row, col = wrap(row, col)
    return nil unless in_bounds?(row, col)

    @cells[row][col]
  end

  # A convenience method that returns `at(@row, @col)`
  def here
    at(@row, @col)
  end

  def row(row = @row)
    @cells[row]
  end

  def col(col = @col)
    @cells.map { |row| row[col] }
  end

  # For each row and column, yields the row number, column number, and
  # value.
  def each
    @cells.each_with_index do |row, ir|
      row.each_with_index do |val, ic|
        yield ir, ic, val
      end
    end
  end

  # Sets value at `row`, `col` to `val`.
  def set(row = @row, col = @col, val)
    row, col = wrap(row, col)
    @cells[row][col] = val
  end

  # Updates value at `row`, `col` by yielding its value and storing the
  # result.
  def update(row = @row, col = @col)
    @cells[row][col] = yield at(row, col)
  end

  # Move to row and col.
  def move_to(row, col)
    @row, @col = wrap(row, col)
  end

  # Move by deltas. wrap_type may be nil, :both, :row, or :col.
  def move_by(row_delta, col_delta)
    @row, @col = wrap(row + row_delta, col + col_delta)
  end

  def in_bounds?(row = @row, col = @col)
    row >= 0 && row < @height && col >= 0 && col < @width
  end

  # wrap_type may be nil, :both, :row, or :col.
  def wrap(row, col)
    case wrap_type
    when :both
      row = row % @height
      col = col % @width
    when :row
      row = row % @height
    when :col
      col = col % @width
    end
    [row, col]
  end

  def to_s
    @cells.map(&:join).join("\n")
  end
end
