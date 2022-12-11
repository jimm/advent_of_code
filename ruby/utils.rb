class Object
  # Prints self.to_s to stdout and returns self, so that this method can be
  # chained.
  def debug(msg = nil)
    puts("#{msg}#{msg ? ' ' : ''}#{self}")
    self
  end

  # Prints self.inspect to stdout and returns self, so that this method can
  # be chained.
  def debugi(msg = nil)
    puts("#{msg}#{msg ? ' ' : ''}#{inspect}")
    self
  end
end

module Enumerable
  # Divides `self` into arrays based on a delimiter, returning an array of
  # arrays of elements. Must either specfiy `func_sym` or pass in a block to
  # be applied to each element.
  def split(func_sym = nil)
    chunk { |entry| func_sym ? entry.send(func_sym) : yield(entry) }
      .reject { |delim_true, _| delim_true }
      .map { |_, vals| vals }
  end
end
