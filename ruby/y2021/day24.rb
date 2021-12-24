# Arithmetic Logic Unit
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

    def load(instructions)
      @instructions = instructions
    end

    def run(inputs)
      x = 0
      y = 0
      @z = 0
      inputs.each_with_index do |digit, i|
        x = @z % 26 + X_ADDS[i]
        @z /= DIVS[i] if DIVS[i] != 1
        x = x == digit ? 0 : 1
        y = (25 * x) + 1 # 1 if x == digit, else 26
        @z *= y
        @z += (digit + Y_ADDS[i]) if x == 1 # that is, if x != digit
      end

      # @inputs = inputs
      # @instructions.each { |i| run_instruction(i) }
    end

    # def run_instruction(instruction)
    #   opcode = instruction[0]
    #   dest_reg = instruction[1]
    #   operand = instruction[2]
    #   operand = @registers[operand] if operand.instance_of?(Symbol)
    #   @registers[dest_reg] = case opcode
    #                          when :inp
    #                            @inputs.shift
    #                          when :add
    #                            @registers[dest_reg] + operand
    #                          when :mul
    #                            @registers[dest_reg] * operand
    #                          when :div
    #                            val = @registers[dest_reg].abs / operand.abs
    #                            val = -val if @registers[dest_reg].negative? != operand.negative?
    #                            val
    #                          when :mod
    #                            @registers[dest_reg] % operand
    #                          when :eql
    #                            @registers[dest_reg] == operand ? 1 : 0
    #                          end
    # end

    def valid?
      @z == 0
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
