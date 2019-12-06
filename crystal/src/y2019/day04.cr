require "../util"

class Day04
  @@min_password_input = 271973
  @@max_password_input = 785961

  def initialize(@part_number : Int32, @testing : Bool)
  end

  def run
    password_range = @@min_password_input..@@max_password_input
    if @part_number == 1
      legit_predicate = ->(pwd : Int32) { legit_password_part_one?(pwd) }
      tests = {111111 => true, 223450 => false, 123789 => false}
    else
      legit_predicate = ->(pwd : Int32) { legit_password_part_two?(pwd) }
      tests = {112233 => true, 123444 => false, 111122 => true}
    end
    if @testing
      run_tests(tests, legit_predicate)
    else
      legit_pwds = legit_passwords_in_range(password_range, legit_predicate)
      puts(legit_pwds.size)
    end
  end

  def legit_passwords_in_range(password_range, legit_predicate)
    password_range.select { |pwd| legit_predicate.call(pwd) }
  end

  def no_digits_decreasing?(digit_chars)
    prev_char = digit_chars[0]
    digit_chars[1..].each do |ch|
      return false if ch < prev_char
      prev_char = ch
    end
    true
  end

  def legit_password_part_one?(pwd)
    digit_chars = pwd.to_s.chars
    return false unless no_digits_decreasing?(digit_chars)

    runs = collect_runs_of_digits(digit_chars)
    runs.any? { |run| run.size >= 2 }
  end

  def legit_password_part_two?(pwd)
    digit_chars = pwd.to_s.chars
    return false unless no_digits_decreasing?(digit_chars)

    runs = collect_runs_of_digits(digit_chars)
    runs.any? { |run| run.size == 2 }
  end

  def collect_runs_of_digits(digit_chars)
    digit_chars.chunk_while { |i, j| i == j }.to_a
  end

  def run_tests(expected, legit_predicate)
    ok = true
    expected.each do |key, val|
      if legit_predicate.call(key) != val
        puts("error: #{key} should be #{val} but it's not")
        ok = false
      end
    end
    puts("all tests passed") if ok
  end
end

AoC.register("2019.4", ->(part_number : Int32, testing : Bool) do
  Day04.new(part_number, testing).run
end)
