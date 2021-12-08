# Seven Segment Search

class Day08 < Day
  # Digits 0-9 encoded as seven-segment displays as described in the puzzle
  CORRECT = %w[
    abcdfg cf acdeg acdfg bcdf
    abdfg abdefg acf abcdefg abcdfg
  ]

  class Digit
    def initialize(value, segments)
    end
  end

  def part1
    lines = data_lines(1)
    num_digits_unique_segment_counts = 0
    lines.each do |line|
      inputs, output_four_digits = line.split('|').map(&:strip).map(&:split)
      # All we need to check are the lengths of the scrambled outputs. We
      # don't need to unscramble anything.
      output_four_digits.each do |s|
        num_digits_unique_segment_counts += 1 if [2, 4, 3, 7].include?(s.length)
      end
    end
    puts num_digits_unique_segment_counts
  end

  def part2
    lines = data_lines(1)
    total = 0
    lines.each do |line|
      inputs, output_four_digits = line.split('|').map(&:strip).map(&:split)
      total += unscramble(inputs, output_four_digits)
    end
    puts total
  end

  def unscramble(inputs, output_four_digits)
    warn '' # DEBUG
    warn '' # DEBUG
    warn "inputs = #{inputs}" # DEBUG
    warn "output_four_digits = #{output_four_digits}" # DEBUG

    # unique digits are
    # - 1 (c, f)
    # - 4 (b, c, d, f)
    # - 7 (a, c, f)
    # - 8 (a, b, c, d, e, f, g)
    #
    # c, f are the two right segments

    # key = real letter, value = possible mixed inputs that match
    constrained = {}
    'a'.upto('g') { |ch| constrained[ch] = %w[a b c d e f g] }

    # unique segment lengths
    inputs.sort_by { |s| s.length }.each do |input|
      segments = input.split('')
      case input.length
      when 2 # digit 1
        %w[c f].each { |ch| constrained[ch] &= segments }
        %w[a b d e g].each { |ch| constrained[ch] -= segments }
      when 3 # digit 7
        if constrained['c'].length == 2 # I've already seen a 1
          # we now know that the third segment must be 'a'
          constrained['a'] = segments - constrained['c']
        end
        %w[a c f].each { |ch| constrained[ch] &= segments }
        %w[b d e g].each { |ch| constrained[ch] -= segments }
      when 4 # digit 4
        %w[b c d f].each { |ch| constrained[ch] &= segments }
        %w[a e g].each { |ch| constrained[ch] -= segments }
      end
    end
    warn "after unique seg lengths, constrained = #{constrained}" # DEBUG
    reserve_uniques(constrained)
    warn "after reserve_uniques, constrained = #{constrained}" # DEBUG
    # DEBUG
    return 1234

    # if all keys have one value, we are done
    until segments_unscrambled?(constrained)
      inputs

      unscrambled = output_four_digits.each do |s|
        str = ''
        s.each_char do |ch|
          str << constrained[ch][0]
        end
        CORRECT.index(str)
      end
    end

    unscrambled[0] * 1000 + unscrambled[1] * 100 + unscrambled[2] * 10 + unscrambled[3]
  end

  def segments_unscrambled?(constrained)
    constrained.keys.all? { |k| constrained[k].length == 1 }
  end

  # Whenever any key has one value, remove it from all other values
  def reserve_uniques(constrained)
    num_changed = 1
    while num_changed > 0
      num_changed = 0
      constrained.keys.each do |k|
        val = constrained[k]
        next unless val.length == 1

        val_to_remove = val[0]
        constrained.keys.each do |k2|
          next if k2 == k

          num_changed += 1 if constrained[k2].delete(val_to_remove)
        end
      end
    end
  end
end
