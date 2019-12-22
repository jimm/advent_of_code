require "../day"

module Year2019
  class Day22 < Day
    def part1
      if @testing
        ok = true
        data_chunks(data_lines()).each do |data_chunk|
          retval = run_test1(data_chunk[0].split(" ").map(&.to_i), data_chunk[1])
          ok &&= retval
        end
        puts("ok") if ok
        return
      end
      deck = Array.new(10007, &.itself)
      rules = data_lines()
      run_rules(deck, rules)
      puts(deck.index(2019))
    end

    # Integer overflow with array indexes!
    def part2
      # no_tests

      # deck = Array.new(119315717514047_u64, &.itself)
      # rules = data_lines()
      # 101741582076661.times { |_| run_rules(deck, rules) }
      # puts(deck[2020]) # 2021? 1-based?
    end

    def run_rules(deck : Array(UInt64), rules)
      size = deck.size
      rules.each do |rule|
        case rule
        when /deal with increment (\d+)/
          incr = $1.to_i
          new_deck = deck.dup
          i = 0
          size.times do |j|
            new_deck[i] = deck[j]
            i += incr
            i %= size
          end
          deck[0..-1] = new_deck
        when /deal into new stack/
          deck.reverse!
        when /cut (-?\d+)/
          cut = $1.to_i
          deck.rotate!(cut)
        else
          raise "error: unknown rule #{rule}"
        end
      end
    end

    def run_test1(expected_order, rules)
      deck = Array.new(expected_order.size, &.itself)
      run_rules(deck, rules)
      expected_order.each_with_index do |val, i|
        if val != deck[i]
          puts("error: expected #{expected_order}, saw #{deck}")
          return false
        end
      end
      true
    end
  end
end

AoC.register(Year2019::Day22)
