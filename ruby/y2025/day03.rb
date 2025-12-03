#!/usr/bin/env ruby
#
# Lobby

require_relative '../day'

class Day03 < Day
  def do_part1(lines)
    lines.map { |line| _max_joltage_2(line) }.sum
  end

  def do_part2(lines)
    lines.map { |line| _max_joltage_12(line) }.sum
  end

  def _max_joltage_2(line)
    charges = line.chars.map(&:to_i)
    l = charges.length
    # brute force
    max_charge = 0
    l.times do |i|
      (i + 1...l).each do |j|
        charge = charges[i] * 10 + charges[j]
        max_charge = charge if charge > max_charge
      end
    end
    max_charge
  end

  # This works, but takes around eight minutes to run. If I have time I'll
  # come back and optimize it.
  def _max_joltage_12(line)
    charges = line.chars.map(&:to_i)
    l = charges.length

    max_charge = 0
    (0...l - 11).each do |a|
      ca = charges[a] * 100_000_000_000
      next if (ca + 100_000_000_000) < max_charge

      (a + 1...l - 10).each do |b|
        cb = charges[b] * 10_000_000_000
        next if ca + (cb + 10_000_000_000) < max_charge

        (b + 1...l - 9).each do |c|
          cc = charges[c] * 1_000_000_000
          next if ca + cb + (cc + 1_000_000_000) < max_charge

          (c + 1...l - 8).each do |d|
            cd = charges[d] * 100_000_000
            next if ca + cb + cc + (cd + 100_000_000) < max_charge

            (d + 1...l - 7).each do |e|
              ce = charges[e] * 10_000_000
              next if ca + cb + cc + cd + (ce + 10_000_000) < max_charge

              (e + 1...l - 6).each do |f|
                cf = charges[f] * 1_000_000
                next if ca + cb + cc + cd + ce + (cf + 1_000_000) < max_charge

                (f + 1...l - 5).each do |g|
                  cg = charges[g] * 100_000
                  next if ca + cb + cc + cd + ce + cf + (cg + 100_000) < max_charge

                  (g + 1...l - 4).each do |h|
                    ch = charges[h] * 10_000
                    next if ca + cb + cc + cd + ce + cf + cg + (ch + 10_000) < max_charge

                    (h + 1...l - 3).each do |i|
                      ci = charges[i] * 1000
                      next if ca + cb + cc + cd + ce + cf + cg + ch + (ci + 1000) < max_charge

                      (i + 1...l - 2).each do |j|
                        cj = charges[j] * 100
                        next if ca + cb + cc + cd + ce + cf + cg + ch + ci + (cj + 100) < max_charge

                        (j + 1...l - 1).each do |k|
                          ck = charges[k] * 10
                          next if ca + cb + cc + cd + ce + cf + cg + ch + ci + cj + (ck + 10) < max_charge

                          (k + 1...l).each do |l|
                            cl = charges[l]
                            charge = ca + cb + cc + cd + ce + cf +
                                     cg + ch + ci + cj + ck + cl
                            max_charge = charge if charge > max_charge
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
    max_charge
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
