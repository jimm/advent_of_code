# Alchemical Reduction

from utils import *

tests = {
    "aA": "",
    "abBA": "",
    "abAB": "abAB",
    "aabAAB": "aabAAB",
    "dabAcCaCBAcCcaDA": "dabCBAcaDA",
}


def part1(env):
    if env.test:
        for input, output in tests.items():
            result = _remove_antipairs(input)
            print(len(result))
            if len(result) != len(output):
                print(f"error with '{input}' => '{output}', wrong result '{result}'")
    else:
        polymer = data_file_lines(env)[0]
        print(len(_remove_antipairs(polymer)))


def _remove_antipairs(p):
    lower_p = p.lower()
    i = 0
    while (i < len(p) - 1) or (i == 0 and len(p) == 2):
        if lower_p[i] == lower_p[i + 1] and p[i].islower() != p[i + 1].islower():
            if i == 0:
                p = p[2:]
                lower_p = lower_p[2:]
            else:
                p = p[:i] + p[i + 2 :]
                lower_p = lower_p[:i] + lower_p[i + 2 :]
                i -= 1
        else:
            i += 1
    return p


def part2(env):
    if env.test:
        for polymer in tests.keys():
            print(_min_removed_unit_len(polymer))
    else:
        polymer = data_file_lines(env)[0]
        print(_min_removed_unit_len(polymer))


def _min_removed_unit_len(p):
    return min(
        [
            len(_remove_antipairs(p[:].replace(ch, "").replace(ch.upper(), "")))
            for ch in "abcdefghijklmnopqrstuvwxyz"
        ]
    )
