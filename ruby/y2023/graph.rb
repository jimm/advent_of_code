require 'set'

class Graph
  attr_reader :vertices

  def initialize
    @vertices = []
  end

  def add_vertex(v)
    @vertices << v
  end

  def remove_vertex(v)
    v.inputs.each { remove_edge(_1) }
    v.outputs.each { remove_edge(_1) }
    @vertices.delete(v)
  end

  def add_edge(v_src, v_dest)
    e = Edge.new(v_src, v_dest)
  end

  def remove_edge(e)
    e.source.outputs.delete(e)
    e.destination.inputs.delete(e)
  end

  def edges
    es = Set.new
    vertices.each do |v|
      es += v.inputs
      es += v.outputs
    end
    es
  end
end

class Vertex
  attr_reader :name, :inputs, :outputs
  def initialize(name)
    @name = name
    @inputs = []
    @outputs = []
  end

  def to_s = "Vertex(name=#{name})"
  def inspect = to_s
end

class Edge
  attr_reader :source, :dest
  def initialize(source, dest)
    @source = source
    @source.outputs << self
    @dest = dest
    @dest.inputs << self
  end

  def to_s = "Edge(#{source.name} -> #{dest.name})"
  def inspect = to_s
end
