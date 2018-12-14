# Chocolate Charts
#
# part 2: 71764232 is too high

from utils import *

PUZZLE_INPUT = 147061
PUZZLE_INPUT_DIGITS = [1,4,7,0,6,1]
TESTS = {
    9: [5,1,5,8,9,1,6,7,7,9],
    5: [0,1,2,4,5,1,5,8,9,1],
    18: [9,2,5,1,0,7,1,0,8,5],
    2018: [5,9,4,1,4,2,9,8,8,2],
}

def part1(testing=False):
    if testing:
        errors = False
        for num_recipes, expected in TESTS.items():
            answer = _ten_after(num_recipes)
            if answer != expected:
                print(f"{num_recipes} failed, expected {expected} saw {answer}")
                errors = True
        if not errors:
            print("ok")
    else:
        print("".join([str(i) for i in _ten_after(PUZZLE_INPUT)]))

def part2(testing=False):
    if testing:
        errors = False
        for expected, digits in TESTS.items():
            digits_to_find = digits # [0:5]
            answer = _num_recipes_before(digits_to_find)
            if answer != expected:
                print(f"{digits_to_find} failed, expected {expected} saw {answer}")
                errors = True
        if not errors:
            print("ok")
    else:
        print(_num_recipes_before(PUZZLE_INPUT_DIGITS))

def _ten_after(n):
    scores = [3, 7]
    scores_len = 2
    e1_index = 0
    e2_index = 1
    while len(scores) < n + 10:
        e1_val = scores[e1_index]
        e2_val = scores[e2_index]
        total = e1_val + e2_val
        if total >= 10:
            scores.append(1)
            scores_len += 1
            total -= 10
        scores.append(total)
        scores_len += 1
        e1_index = (e1_index + e1_val + 1) % scores_len
        e2_index = (e2_index + e2_val + 1) % scores_len
    return scores[n:n+10]

def _num_recipes_before(digits_to_find):
    scores = [3, 7]
    scores_len = 2
    e1_index = 0
    e2_index = 1
    ld = len(digits_to_find)
    while True:
        e1_val = scores[e1_index]
        e2_val = scores[e2_index]
        total = e1_val + e2_val
        scores_len += 1
        num_added = 1
        if total >= 10:
            scores.append(1)
            scores_len += 1
            num_added += 1
            total -= 10
        scores.append(total)
        e1_index = (e1_index + e1_val + 1) % scores_len
        e2_index = (e2_index + e2_val + 1) % scores_len

        if scores[-ld:] == digits_to_find:
            return scores_len-ld
        if num_added == 2 and scores[-(ld+1):-1] == digits_to_find:
            return scores_len-ld-1
