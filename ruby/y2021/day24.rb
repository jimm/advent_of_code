#!/usr/bin/env ruby
#
# Arithmetic Logic Unit

require_relative '../day'
#
# w = x = y = z = 0
# loop
#   read digit
#   x = z
#   x mod 26 (alphabet?)
#   z = z / 1 OR 26 (rounding here)
#   x += NNN
#   x = x == digit ? 1 : 0
#   x = not x (that is, x != digit here)
#   y = 25 * x + 1 (1 OR 26)
#   z = z * y
#   y = digit + MMM
#   y = y * x
#   z += y

class Day24 < Day
  # An instruction is an array of symbols. The first is the opcode. The
  # remaining symbols are either names of registers or constants.
  class ALU
    DIVS = [1, 1, 1, 26, 1, 26, 1, 26, 26, 26, 1, 1, 26, 26]
    X_ADDS = [12, 11, 12, -3, 10, -9, 10, -7, -11, -4, 14, 11, -8, -10]
    Y_ADDS = [7, 15, 2, 15, 14, 2, 15, 1, 15, 15, 12, 2, 13, 13]

    attr_accessor :registers

    def initialize
      @cache = []
      14.times do |place|
        @cache[place] = []
        (1..9).each do |digit|
          @cache[place][digit] = {}
        end
      end
    end

    def load(instructions)
      @instructions = instructions
    end

    def run(inputs)
      @z = 0
      inputs.each_with_index do |digit, i|
        val = get_cache(digit, i, @z)
        if val
          @z = val
        else
          z_val = @z
          x = (@z & 0xff) + X_ADDS[i]
          @z >>= 8 if DIVS[i] != 1
          @z = (@z << 8) + digit + Y_ADDS[i] if x != digit
          set_cache(digit, i, @z)
        end
      end
    end

    def valid?
      @z == 0
    end

    def get_cache(digit, place, z_val)
      @cache[place][digit][z_val]
    end

    def set_cache(digit, place, z_val)
      @cache[place][digit][z_val] = @z
    end
  end

  class DescendingModelNumberGenerator
    def each
      9.downto(1).each do |a|
        9.downto(1).each do |b|
          9.downto(1).each do |c|
            9.downto(1).each do |d|
              9.downto(1).each do |e|
                9.downto(1).each do |f|
                  9.downto(1).each do |g|
                    9.downto(1).each do |h|
                      9.downto(1).each do |i|
                        9.downto(1).each do |j|
                          9.downto(1).each do |k|
                            9.downto(1).each do |l|
                              9.downto(1).each do |m|
                                9.downto(1).each do |n|
                                  yield [a, b, c, d, e, f, g, h, i, j, k, l, m, n]
                                end
                              end
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end

  def part1
    lines = data_lines(1)
    alu = ALU.new
    alu.load(parse(lines))
    dmng = DescendingModelNumberGenerator.new
    dmng.each do |inputs|
      alu.run(inputs)
      if alu.valid?
        puts "valid model number: #{inputs.join}"
        break
      end
    end
  end

  def part2
    lines = data_lines(1)
  end

  def parse(lines)
    lines.map do |line|
      words = line.split
      instruction = [words[0].to_sym, words[1].to_sym]
      if words[2]
        instruction << case words[2]
                       when /[wxyz]/
                         words[2].to_sym
                       else
                         words[2].to_i
                       end
      end
      instruction
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(2021, 24)
end
