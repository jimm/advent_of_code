# Giant Squid

class BingoGame
  def initialize(input_lines)
    @numbers = input_lines.shift.split(',').map(&:to_i)
    @boards = []
    board_lines = input_lines.shift(5)
    until board_lines.empty?
      @boards << BingoBoard.new(board_lines)
      board_lines = input_lines.shift(5)
    end
  end

  def play
    @numbers.each do |n|
      @boards.each do |b|
        b.play_number(n)
        return b if b.won?
      end
    end
  end

  def play_to_lose
    @numbers.each_with_index do |n, i|
      @boards.each { |b| b.play_number(n) }
      winners, losers = @boards.partition(&:won?)
      next unless losers.length == 1

      loser = losers[0]
      loser.play_number(@numbers[i + 1])
      return loser
    end
  end

  def to_s
    "BingoGame with #{@boards.length} boards and #{@numbers.length} numbers left"
  end
end

class BingoCell
  attr_reader :number
  attr_accessor :marked

  def initialize(n)
    @number = n
    @marked = false
  end

  def marked?
    @marked
  end

  def to_s
    " #{@marked ? '*' : ' '}#{format('%2d', @number)}"
  end
end

class BingoBoard
  attr_reader :rows, :cols, :last_played_number

  def initialize(lines)
    @rows = lines.map { |line| line.split.map { |str| BingoCell.new(str.to_i) } }
    @cols = []
    @rows.each_with_index do |row, row_index|
      row.each_with_index do |cell, cell_index|
        @cols[cell_index] ||= []
        @cols[cell_index][row_index] = cell
      end
    end
    @last_played_number = nil
  end

  def play_number(n)
    @last_played_number = n
    c = @rows.flatten.detect { |cell| cell.number == n }
    c.marked = true if c
  end

  def won?
    @rows.any? { |row| all_marked?(row) } ||
      @cols.any? { |col| all_marked?(col) }
  end

  def score
    @last_played_number * @rows.flatten.select { |cell| !cell.marked? }.map(&:number).sum
  end

  def all_marked?(cells)
    cells.all? { |cell| cell.marked? }
  end

  def to_s
    str = ''
    @rows.each do |row|
      row.each { |cell| str << cell.to_s }
      str << "\n"
    end
    str
  end
end

class Day04 < Day
  def part1
    game = BingoGame.new(data_lines(1))
    winning_board = game.play
    puts winning_board.score
  end

  def part2
    game = BingoGame.new(data_lines(1))
    last_winning_board = game.play_to_lose
    puts last_winning_board.score
  end
end
