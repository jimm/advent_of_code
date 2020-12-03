# A two-dimensional map where each cell is a character.
class Map
  attr_reader :map, :height, :width

  # Given lines of text, initializes a two-dimensional map of characters.
  def initialize(lines)
    @map = lines.map { |line| line.split('') }
    @height = @map.length
    @width = @map[0].length
  end

  # Returns the character at [row_idx][col_idx]. If args[:wrap] is true, will
  # wrap those coords using modular arithmetic.
  def at(row_idx, col_idx, args)
    if args[:wrap]
      row_idx = row_idx % @height
      col_idx = col_idx % @width
    end
    @map[row_idx][col_idx]
  end
end
