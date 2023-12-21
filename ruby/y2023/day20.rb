#!/usr/bin/env ruby
#
# Pulse Propagation

require 'observer'
require_relative '../day'

class Day20 < Day

  class Module
    include Observable

    attr_reader :name, :state, :lows_sent, :highs_sent, :inputs

    def initialize(name)
      @name = name
      @state = :low
      @inputs = []
      @lows_sent = 0
      @highs_sent = 0
    end

    def add_observer(observer, func=:update)
      super
      observer.inputs << self
    end

    def notify_observers(sender, pulse)
      if pulse == :low
        @lows_sent += @observer_peers.keys.size
      else
        @highs_sent += @observer_peers.keys.size
      end
      super
    end

    def update(sender, pulse)
      # nop
    end

    def run
      # nop
    end

    def to_s = "#{self.class.name}(name=#{@name}, state=#{@state})"
    def inspect = "#{self.class.name}(name=#{@name}, state=#{@state}, inputs=#{@inputs.map(&:name)})"
  end

  class FlipFlop < Module
    def initialize(name)
      super
    end

    def update(sender, pulse)
      super
      return unless pulse == :low

      @state = @state == :low ? :high : :low
      changed
    end

    def run
      notify_observers(self, @state)
    end
  end

  class Conjunction < Module
    def initialize(name)
      super
      @received = {}
    end

    def update(sender, pulse)
      super
      @received[sender.name] = pulse
      @state = @inputs.map(&:name).all? { @received[_1] == :high } ? :low : :high
      changed
      notify_observers(self, @state)
    end
  end

  class Broadcaster < Module
    def initialize = super('broadcaster')

    def update(sender, pulse)
      super
      @state = pulse
      changed
    end

    def run
      notify_observers(self, @state)
    end
  end

  class Button < Module
    def initialize = super('button')

    def push
      changed
      notify_observers(self, :low)
    end
  end

  def do_part1(lines)
    modules = parse(lines)
    button = modules.detect { _1.name == 'button' }
    1000.times do
      button.push
      changed = modules.select(&:changed?)
      until changed.empty?
        changed.each(&:run)
        changed = modules.select(&:changed?)
      end
    end
    modules.map(&:highs_sent).sum * modules.map(&:lows_sent).sum
  end

  def do_part2(lines)
    no_tests

    modules = parse(lines)
    button = modules.detect { _1.name == 'button' }
    rx = modules.detect { _1.name == 'rx' }
    num_button_presses = 0
    while true
      num_button_presses += 1
      button.push
      changed = modules.select(&:changed?)
      until changed.empty?
        changed.each(&:run)
        changed = modules.select(&:changed?)
      end
      break if rx.lows_sent >= 1
    end
    rx.lows_sent
  end

  private

  def parse(lines)
    modules = {} # name -> module
    to_link = {} # remember what needs to be linked after being built

    m = Button.new
    modules[m.name] = m
    to_link[m] = ['broadcaster']

    lines.map do |line|
      input_name, output_names = line.split(' -> ')
      output_names = output_names.split(', ')
      m = if input_name == 'broadcaster'
            Broadcaster.new
          elsif input_name[0] == '%'
            FlipFlop.new(input_name[1..])
          elsif input_name[0] == '&'
            Conjunction.new(input_name[1..])
          end
      modules[m.name] = m
      to_link[m] = output_names
    end

    to_link.each do |m, output_names|
      output_names.each do |output_name|
        observer = modules[output_name]
        unless observer
          # as-yet undefined, such as "output"
          observer = Module.new(output_name)
          modules[output_name] = observer
        end
        m.add_observer(observer)
      end
    end

    modules.values
  end
end

if __FILE__ == $PROGRAM_NAME
  require_relative '../aoc'

  aoc(__FILE__)
end
