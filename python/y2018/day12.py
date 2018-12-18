# Subterranean Sustainability

from utils import *


def part1(testing=False):
    pots, rules = _read_data(1, testing)
    print(_run_for_n_generations(pots, rules, 20))


def part2(testing=False):
    pots, rules = _read_data(2, testing)
    # n = 50000000000
    n = 50000000000
    print(_run_for_n_generations(pots, rules, n))


def _run_for_n_generations(pots, rules, n):
    return _run_generations(pots, rules, n)


def _run_generations(pots, rules, gens_left):
    prev_gen = None
    while gens_left > 0:
        if prev_gen and _offset_from(prev_gen, pots):
            return sum([i + gens_left for i in pots])
        prev_gen = pots
        pots = _apply_rules(pots, rules)
        gens_left -= 1
    return sum(pots)


def _apply_rules(pots, rules):
    new_pots = set()
    min_idx, max_idx = minmax(pots)
    for i in range(min_idx - 2, max_idx + 3):
        if _matches_rule(pots, i, rules):
            new_pots.add(i)
    return new_pots


def _matches_rule(pots, i, rules):
    neighborhood = tuple([1 if idx in pots else 0 for idx in range(i - 2, i + 3)])
    if neighborhood in rules:
        return 1
    return 0


def _offset_from(prev_gen, pots):
    if len(prev_gen) != len(pots):
        return False
    for p in pots:
        if p-1 not in prev_gen:
            return False
    return True

# rule is a tuple like ([1,1,0,1,1], 1)
def _read_data(part_num, testing):
    lines = data_file_lines(12, 1, testing)
    pots = set([idx for idx, ch in enumerate(lines[0][15:]) if ch == "#"])
    rules = set(
        [
            tuple(nbhd)
            for nbhd, res in [_parse_rule(line) for line in lines[1:]]
            if res == 1
        ]
    )
    return (pots, rules)


def _parse_rule(line):
    neighborhood = [1 if ch == "#" else 0 for ch in line[0:5]]
    result = 1 if line[9] == "#" else 0
    return (neighborhood, result)
