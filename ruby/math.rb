module Integer
  # divisors of self
  # assumes all non-negative numbers
  def divisors
    divisors = [1, self]
    (2..self/2).each do |m|
      divisors << m if self % m == 0
    end
    divisors.sort
  end

  # greatest common divisor between self and `n`
  # assumes all non-negative numbers
  def gcd(n)
    self_divs = self.divisors()
    n_divs = n.divisors()
    (self_divs & n_divs).max
  end

  # true if self and `n` are coprime
  def coprime?(n)
    gcd(self, n) == 1
  end

  # least common multiple of self and `n`
  # assumes all non-negative numbers
  def lcm(n)
    (self * n) / gcd(n)
  end
end
