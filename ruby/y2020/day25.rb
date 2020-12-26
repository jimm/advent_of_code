# Combo Breaker

require 'bigdecimal/util'

class Day25 < Day
  SEED = 7

  def part1
    card_public_key = 8987316
    door_public_key = 14681524

    # one way
    card_loop_size = find_loop_size(SEED, card_public_key)
    puts(transform(door_public_key, card_loop_size))

    # # the other way
    # door_loop_size = find_loop_size(7, doorcard_public_key)
    # $stderr.puts "door_loop_size = #{door_loop_size}" # DEBUG
    # puts(transform(card_public_key, door_loop_size))
  end

  def part1_tests
    card_public_key = 5764801
    door_public_key = 17807724
    card_loop_size = 8
    door_loop_size = 11
    expected_encryption_key = 14897079

    errors = []
    test(card_public_key, transform(SEED, card_loop_size),
         "transform card", errors)
    test(door_public_key, transform(SEED, door_loop_size),
         "transform door", errors)
    test(card_loop_size, find_loop_size(SEED, card_public_key),
         "find card loop", errors)
    test(door_loop_size, find_loop_size(SEED, door_public_key), "
find door loop", errors)

    card_loop_size = find_loop_size(SEED, card_public_key)
    test(expected_encryption_key, transform(door_public_key, card_loop_size),
         "find key w/door", errors)
    test(expected_encryption_key, transform(card_public_key, door_loop_size),
         "find key w/card", errors)

    puts
    if errors.empty?
      puts "ok"
    else
      errors.each do |err|
        puts "error: #{err[2]} expected #{err[0]}, got #{err[1]}"
      end
    end
  end

  def test(expected, val, msg, errors)
    if expected == val
      print('.')
    else
      print('F')
      errors << [expected, val, msg]
    end
  end

  def transform(num, loop_size)
    # (num ** loop_size).remainder(20201227)

    # Efficient computation with integer exponents (from Wikipedia's
    # Exponentiation page)
    powers = {}                 # key = power, val = num ** power
    val = num
    powers[1] = val

    max_exp_needed = 2 ** Math.log(loop_size, 2).ceil

    exp = 2
    while exp <= max_exp_needed
      squared = val * val
      powers[exp] = squared
      val = squared
      exp *= 2
    end

    answer = 1
    ('%b' % loop_size).reverse.split('').map(&:to_i).each_with_index do |bit, i|
      if bit == 1
        answer *= powers[2**i]
      end
    end

    answer.remainder(20201227)
  end

  def find_loop_size(num, public_key)
    val = 1
    (1...).each do |i|
      val *= 7
      # return i if val.remainder(20201227) == public_key
      val -= 20201227 while val > 20201227
      return i if val == public_key
    end
  end

  def part2
    lines = data_lines(1)
  end
end
