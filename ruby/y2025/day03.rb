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

  # This version gets the runtime down from just over eight minutes to just
  # over three. We do this by skipping digits that are lower than
  # previously-seen digits in the same place in the charge.
  #
  # Next step, probably: make this recursive or at least loop over the
  # digits place index.
  def _max_joltage_12(line)
    warn "line = #{line}" # DEBUG
    charges = line.chars.map(&:to_i)
    l = charges.length

    max_charge = 0
    max_charges_seen = [0] * 12
    (0...l - 11).each do |a|
      next if charges[a] <= max_charges_seen[0]

      max_charges_seen[0] = charges[a]
      max_charges_seen[1] = 0

      (a + 1...l - 10).each do |b|
        next if charges[b] <= max_charges_seen[1]

        max_charges_seen[1] = charges[b]
        max_charges_seen[2] = 0

        (b + 1...l - 9).each do |c|
          next if charges[c] <= max_charges_seen[2]

          max_charges_seen[2] = charges[c]
          max_charges_seen[3] = 0

          (c + 1...l - 8).each do |d|
            next if charges[d] <= max_charges_seen[3]

            max_charges_seen[3] = charges[d]
            max_charges_seen[4] = 0

            (d + 1...l - 7).each do |e|
              next if charges[e] <= max_charges_seen[4]

              max_charges_seen[4] = charges[e]
              max_charges_seen[5] = 0

              (e + 1...l - 6).each do |f|
                next if charges[f] <= max_charges_seen[5]

                max_charges_seen[5] = charges[f]
                max_charges_seen[6] = 0

                (f + 1...l - 5).each do |g|
                  next if charges[g] <= max_charges_seen[6]

                  max_charges_seen[6] = charges[g]
                  max_charges_seen[7] = 0

                  (g + 1...l - 4).each do |h|
                    next if charges[h] <= max_charges_seen[7]

                    max_charges_seen[7] = charges[h]
                    max_charges_seen[8] = 0

                    (h + 1...l - 3).each do |i|
                      next if charges[i] <= max_charges_seen[8]

                      max_charges_seen[8] = charges[i]
                      max_charges_seen[9] = 0

                      (i + 1...l - 2).each do |j|
                        next if charges[j] <= max_charges_seen[9]

                        max_charges_seen[9] = charges[j]
                        max_charges_seen[10] = 0

                        (j + 1...l - 1).each do |k|
                          next if charges[k] <= max_charges_seen[10]

                          max_charges_seen[10] = charges[k]
                          max_charges_seen[11] = 0

                          (k + 1...l).each do |l|
                            next if charges[l] <= max_charges_seen[11]

                            max_charges_seen[11] = charges[l]
                            charge = max_charges_seen.reduce(0) { |acc, charge| acc * 10 + charge }
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
    warn "max_charge = #{max_charge}" # DEBUG
    max_charge
  end

  # # This works, but takes around eight minutes to run. If I have time I'll
  # # come back and optimize it.
  # def _max_joltage_12(line)
  #   charges = line.chars.map(&:to_i)
  #   l = charges.length

  #   max_charge = 0
  #   (0...l - 11).each do |a|
  #     ca = charges[a] * 100_000_000_000
  #     next if (ca + 100_000_000_000) < max_charge

  #     (a + 1...l - 10).each do |b|
  #       cb = charges[b] * 10_000_000_000
  #       next if ca + (cb + 10_000_000_000) < max_charge

  #       (b + 1...l - 9).each do |c|
  #         cc = charges[c] * 1_000_000_000
  #         next if ca + cb + (cc + 1_000_000_000) < max_charge

  #         (c + 1...l - 8).each do |d|
  #           cd = charges[d] * 100_000_000
  #           next if ca + cb + cc + (cd + 100_000_000) < max_charge

  #           (d + 1...l - 7).each do |e|
  #             ce = charges[e] * 10_000_000
  #             next if ca + cb + cc + cd + (ce + 10_000_000) < max_charge

  #             (e + 1...l - 6).each do |f|
  #               cf = charges[f] * 1_000_000
  #               next if ca + cb + cc + cd + ce + (cf + 1_000_000) < max_charge

  #               (f + 1...l - 5).each do |g|
  #                 cg = charges[g] * 100_000
  #                 next if ca + cb + cc + cd + ce + cf + (cg + 100_000) < max_charge

  #                 (g + 1...l - 4).each do |h|
  #                   ch = charges[h] * 10_000
  #                   next if ca + cb + cc + cd + ce + cf + cg + (ch + 10_000) < max_charge

  #                   (h + 1...l - 3).each do |i|
  #                     ci = charges[i] * 1000
  #                     next if ca + cb + cc + cd + ce + cf + cg + ch + (ci + 1000) < max_charge

  #                     (i + 1...l - 2).each do |j|
  #                       cj = charges[j] * 100
  #                       next if ca + cb + cc + cd + ce + cf + cg + ch + ci + (cj + 100) < max_charge

  #                       (j + 1...l - 1).each do |k|
  #                         ck = charges[k] * 10
  #                         next if ca + cb + cc + cd + ce + cf + cg + ch + ci + cj + (ck + 10) < max_charge

  #                         (k + 1...l).each do |l|
  #                           cl = charges[l]
  #                           charge = ca + cb + cc + cd + ce + cf +
  #                                    cg + ch + ci + cj + ck + cl
  #                           max_charge = charge if charge > max_charge
  #                         end
  #                       end
  #                     end
  #                   end
  #                 end
  #               end
  #             end
  #           end
  #         end
  #       end
  #     end
  #   end
  #   max_charge
  # end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
