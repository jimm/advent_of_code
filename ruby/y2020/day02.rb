# Password Philosophy

class PasswordPolicy
  attr_reader :n1, :n2, :letter

  def self.parse(text)
    text =~ /(\d+)-(\d+) (.)/
    new($1.to_i, $2.to_i, $3)
  end

  def initialize(n1, n2, letter)
    @n1 = n1
    @n2 = n2
    @letter = letter
  end

  def valid_password?(password)
    raise "subclasses must implement"
  end
end


class OldPasswordPolicy < PasswordPolicy
  def valid_password?(password)
    found_letters = password.chars.select { |ch| ch == @letter }
    len = found_letters.length
    len >= @n1 && len <= @n2
  end
end


class NewPasswordPolicy < PasswordPolicy
  def valid_password?(password)
    [password[@n1-1], password[@n2-1]].one? { |ch| ch == @letter }
  end
end


class Day02 < Day
  def part1
    analyze_with_policy(OldPasswordPolicy)
  end

  def part2
    analyze_with_policy(NewPasswordPolicy)
  end

  def analyze_with_policy(policy_class)
    entries = data_lines(1).map do |line|
      policy_text, password = line.split(':').map(&:strip)
      [policy_class.parse(policy_text), password]
    end
    print(entries.select { |e| e[0].valid_password?(e[1]) }.length)
  end
end
