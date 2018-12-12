# Subterranean Sustainability

from utils import *


def part1(testing=False):
    pots, rules = _read_data(1, testing)
    pots = _run_for_n_generations(pots, rules, 20)
    answer = sum([(i - 20) for i in range(len(pots)) if pots[i]])
    print(answer)


def part2(testing=False):
    pots, rules = _read_data(1, testing)
    # n = 50000000000
    n = 50000
    pots = _run_for_n_generations(pots, rules, n)
    answer = sum([(i - n) for i in range(len(pots)) if pots[i]])
    print(answer)


def _run_for_n_generations(pots, rules, n):
    expanded = ([0] * n) + pots + ([0] * (n * 2))
    return _run_generations(expanded, rules, n)


def _run_generations(pots, rules, gens_left):
    while gens_left > 0:
        pots = _apply_rules(pots, rules)
        gens_left -= 1
    return pots


def _apply_rules(pots, rules):
    min_one_index = pots.index(1)
    max_one_index = len(pots) - 1 - pots[::-1].index(1)
    new_pots = (
        pots[: min_one_index - 5]
        + [
            _apply_rule(pots, i, rules)
            for i in range(min_one_index - 5, max_one_index + 5)
        ]
        + pots[max_one_index + 5 :]
    )
    return new_pots


def _apply_rule(pots, i, rules):
    neighborhood = pots[i - 2 : i + 3]
    if neighborhood in rules:
        return 1
    return 0


# rule is a tuple like ([1,1,0,1,1], 1)
def _read_data(part_num, testing):
    lines = data_file_lines(12, 1, testing)
    initial_state = [1 if ch == "#" else 0 for ch in lines[0][15:]]
    rules = [_parse_rule(line) for line in lines[1:]]
    # filter out zero rules; _apply_rule sets to zero if no match
    rules = [nbhd for (nbhd, res) in rules if res == 1]
    return (initial_state, rules)


def _parse_rule(line):
    neighborhood = [1 if ch == "#" else 0 for ch in line[0:5]]
    result = 1 if line[9] == "#" else 0
    return (neighborhood, result)
