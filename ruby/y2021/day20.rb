#!/usr/bin/env ruby
#
# Trench Map

require_relative '../day'

class Day20 < Day
  class Image
    def self.parse(lines)
      algorithm = []
      lines[0].each_char { |ch| algorithm << (ch == '#' ? 1 : 0) }
      pixels = lines[1..-1].map do |line|
        line.split('').map { |ch| ch == '#' ? 1 : 0 }
      end
      new(pixels, algorithm)
    end

    attr_reader :pixels, :algorithm

    def initialize(pixels, algorithm)
      @pixels = pixels
      @algorithm = algorithm
    end

    def height
      @pixels.length
    end

    def width
      @pixels[0].length
    end

    def sharpen!
      old_height = height
      old_width = width
      need_to_reverse = @algorithm[0] != 0

      # grow existing pixels outwards
      reverse_everything! if need_to_reverse
      @pixels = @pixels.map { |row| [0, 0, 0, 0] + row + [0, 0, 0, 0] }
      @pixels = [Array.new(old_width + 8, 0), Array.new(old_width + 8, 0)] +
                @pixels +
                [Array.new(old_width + 8, 0), Array.new(old_width + 8, 0)]

      # clone
      new_pixels = Array.new(height) { Array.new(width, 0) }

      # sharpen
      (1..height - 2).each do |row_index|
        (1..width - 2).each do |col_index|
          new_pixels[row_index][col_index] = sharpened_pixel(row_index, col_index)
        end
      end

      @pixels = new_pixels
      reverse_everything! if need_to_reverse
    end

    def sharpened_pixel(row_index, col_index)
      algorithm_index = [
        @pixels[row_index - 1][col_index - 1],
        @pixels[row_index - 1][col_index],
        @pixels[row_index - 1][col_index + 1],
        @pixels[row_index][col_index - 1],
        @pixels[row_index][col_index],
        @pixels[row_index][col_index + 1],
        @pixels[row_index + 1][col_index - 1],
        @pixels[row_index + 1][col_index],
        @pixels[row_index + 1][col_index + 1]
      ].join.to_i(2)
      @algorithm[algorithm_index]
    end

    def num_on_pixels
      @pixels.map do |row|
        row.select { |val| val == 1 }.count
      end.sum
    end

    def reverse_everything!
      warn 'reversing everything' # DEBUG
      @pixels.each do |row|
        width.times do |i|
          row[i] = 1 - row[i]
        end
      end
      # @algorithm.length.times do |i|
      #   @algorithm[i] = 1 - @algorithm[i]
      # end
    end

    def to_s
      @pixels.map(&:join).map { |row| row.gsub(/0/, '.').gsub(/1/, '#') }.join("\n")
    end
  end

  def part1
    image = Image.parse(data_lines(1))
    puts
    puts image
    2.times do
      image.sharpen!
      puts
      puts image
    end
    # test ok but 4697 too low; 5133 too high (as is 5940)
    puts image.num_on_pixels
  end

  def part2
    lines = data_lines(1)
  end

  def parse(lines)
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
