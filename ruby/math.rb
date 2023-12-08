class Integer
  # divisors of self
  # assumes all non-negative numbers
  def divisors
    divisors = [1, self]
    (2..self / 2).each do |m|
      divisors << m if self % m == 0
    end
    divisors.sort
  end

  # Returns true if self is prime.
  #
  # See also
  # http://www.noulakaz.net/weblog/2007/03/18/a-regular-expression-to-check-for-prime-numbers/
  # ("1" * n) !~ %r{^1?$|^(11+?)\1+$}
  # It's a lot slower, but cool.
  def prime?
    h = Math.sqrt(self).to_i + 1
    i = 2
    j = h
    while i != j
      if self % i == 0
        j = i
      else
        i += 1
      end
    end
    i == h
  end

  def next_prime
    return 2 if self < 2

    i = succ
    i += 1 if i.even?
    i += 2 until i.prime?
    i
  end

  # Returns the prime factors of self as a hash whose keys are the primes
  # and values are the number of times the prime appears.
  def prime_factors
    h = Hash.new { |h, k| h[k] = 0 }
    divs = divisors[1..].select(&:prime?)
    divs.each do |p|
      n = self
      num = -1
      div = n
      mod = 0
      while mod == 0
        num += 1
        div, mod = div.divmod(p)
      end
      h[p] = num
    end
    h
  end

  # Returns the greatest common divisor between self and `n`. Assumes both
  # self and n are non-negative numbers.
  def gcd(n)
    self_divs = divisors
    n_divs = n.divisors
    (self_divs & n_divs).max
  end

  # Returns true if self and `n` are coprime.
  def coprime?(n)
    gcd(self, n) == 1
  end

  # Returns the least common multiple of self and `n`. Assumes both self and
  # n are non-negative integers.
  def lcm(n)
    (self * n) / gcd(n)
  end
end

# Returns the least common multiple of an array of integers. Assumes all
# elements of arr are non-negative integers.
def lcm(arr)
  pfs = arr.map(&:prime_factors)
  all_primes = pfs.map(&:keys).flatten.uniq
  all_primes.inject(1) do |acc, p|
    n = pfs.map { |prime_factors| prime_factors[p] || 0 }.max
    acc * (p**n)
  end
end
