# Memory Maneuver

import itertools

from utils import *


class Node:
    id_generator = itertools.count()

    def __init__(self, parent):
        self.name = chr(ord("A") + next(Node.id_generator))
        self.parent = parent
        self.children = []
        self.metadata = []

    def metadata_sum(self):
        return sum(self.metadata) + sum([c.metadata_sum() for c in self.children])

    def value(self):
        if not self.children:
            return sum(self.metadata)
        num_children = len(self.children)
        return sum(
            [self.children[m - 1].value() for m in self.metadata if m <= num_children]
        )

    def print_tree(self):
        print(str(self))
        for c in self.children:
            c.print_tree()

    def __str__(self):
        return f"{self.name} {[c.name for c in self.children]} {self.metadata}"


def part1(testing=False):
    root = _read_tree(1, testing)
    if testing:
        root.print_tree()
    print(root.metadata_sum())


def part2(testing=False):
    root = _read_tree(2, testing)
    print(root.value())


def _read_tree(part_num, testing):
    data = [int(n) for n in data_file_lines(2018, 8, 1, testing)[0].split()]
    root, _ = _read_node(None, data)
    return root


def _read_node(parent, data):
    node = Node(parent)
    header, data = data[:2], data[2:]
    num_children, metadata_len = header
    for _ in range(num_children):
        child, data = _read_node(node, data)
        node.children.append(child)
    metadata, data = data[:metadata_len], data[metadata_len:]
    node.metadata = metadata[:]
    return node, data
