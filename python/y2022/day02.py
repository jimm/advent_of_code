# Rock Paper Scissors

from utils import *

SHAPE_SCORE = {"A": 1, "B": 2, "C": 3, "X": 1, "Y": 2, "Z": 3}
WIN_SCORE = {"lose": 0, "draw": 3, "win": 6}
# [theirs, yours] => win symbol
MOVE_RESULT = {
    ("A", "X"): "draw",
    ("A", "Y"): "win",
    ("A", "Z"): "lose",
    ("B", "X"): "lose",
    ("B", "Y"): "draw",
    ("B", "Z"): "win",
    ("C", "X"): "win",
    ("C", "Y"): "lose",
    ("C", "Z"): "draw",
}
CODE_TO_RESULT = {"X": "lose", "Y": "draw", "Z": "win"}
OPPONENT_AND_RESULT_TO_MY_SHAPE = {
    ("A", "win"): "Y",
    ("A", "lose"): "Z",
    ("A", "draw"): "A",
    ("B", "win"): "Z",
    ("B", "lose"): "X",
    ("B", "draw"): "Y",
    ("C", "win"): "X",
    ("C", "lose"): "Y",
    ("C", "draw"): "Z",
}


def part1(env):
    lines = data_file_lines(env)
    print(sum(part1_round_score(line) for line in lines))


def part2(env):
    lines = data_file_lines(env)
    print(sum(part2_round_score(line) for line in lines))


def part1_round_score(line):
    opponent_shape, my_shape = line.split()
    match MOVE_RESULT[(opponent_shape, my_shape)]:
        case "draw":
            return SHAPE_SCORE[my_shape] + 3
        case "win":
            return SHAPE_SCORE[my_shape] + 6
        case "lose":
            return SHAPE_SCORE[my_shape] + 0


def part2_round_score(line):
    opponent_shape, result_code = line.split()
    desired_result = CODE_TO_RESULT[result_code]
    my_shape = OPPONENT_AND_RESULT_TO_MY_SHAPE[(opponent_shape, desired_result)]
    match desired_result:
        case "draw":
            return SHAPE_SCORE[my_shape] + 3
        case "win":
            return SHAPE_SCORE[my_shape] + 6
        case "lose":
            return SHAPE_SCORE[my_shape] + 0
