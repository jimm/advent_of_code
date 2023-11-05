# Puzzle Name

from utils import *


# doubly-linked list
class Node:
    def __init__(self, data, left, right):
        self.data = data
        self.left = left
        self.right = right


class MarbleMania:
    def __init__(self, num_players, num_marbles, testing=False):
        self.scores = [0] * num_players
        self.next_player = 0
        # circle will always point to current node
        self.circle = Node(0, None, None)
        self.circle.left = self.circle
        self.circle.right = self.circle
        self.begin_printing_at = self.circle
        self.num_marbles = num_marbles
        self.next_marble = 1
        self.testing = testing

    def play(self):
        while self.next_marble < self.num_marbles:
            self._take_turn()
            if self.testing:
                self._print_state()
        return max(self.scores)

    def _take_turn(self):
        self.next_marble % 23 == 0 and self._take_funky_turn() or self._take_normal_turn()
        self.next_player = (self.next_player + 1) % len(self.scores)
        self.next_marble += 1

    def _take_normal_turn(self):
        place_after_loc = self.circle.right
        left = place_after_loc
        right = place_after_loc.right
        self.circle = Node(self.next_marble, left, right)
        left.right = self.circle
        right.left = self.circle
        return True

    def _take_funky_turn(self):
        self.scores[self.next_player] += self.next_marble
        remove_node = self.circle.left.left.left.left.left.left.left
        self.scores[self.next_player] += remove_node.data
        self.circle = remove_node.right
        remove_node.left.right = remove_node.right
        remove_node.right.left = remove_node.left
        return True  # make sure _take_normal_turn() not called

    def _print_state(self):
        if not self.testing:
            return
        # NOTE: we assume that self.begin_printing_at is never removed from
        # the game. (To take that into account is certainly possible but not
        # worth it.) We know this is true for the first test game with 9
        # players. It may not be true for ANY OTHER GAME.
        node = self.begin_printing_at
        while True:
            is_curr = node == self.circle
            print(f" {is_curr and '(' or ''}{node.data}{is_curr and ')' or ''}", end="")
            node = node.right
            if node == self.begin_printing_at:
                break
        print()

    def __str__(self):
        return f"{self.num_marbles} marbles, next is {self.next_marble}\n  circle: {self.circle}\n  scores: {self.scores}"


def part1(env):
    # answer only returned if testing, else None
    for num_players, last_marble_points, answer_high_score in _games(env):
        print(f"{num_players}, {last_marble_points}, {answer_high_score}")
        game = MarbleMania(
            num_players, last_marble_points + 1, env.test and num_players < 10
        )
        max_score = game.play()
        print(max_score)


def part2(env):
    num_players, last_marble_points, _ = next(_games(env))
    game = MarbleMania(num_players, (last_marble_points * 100) + 1, False)
    max_score = game.play()
    print(max_score)


def _games(env):
    for line in data_file_lines(env):
        words = line.split()
        yield (int(words[0]), int(words[6]), len(words) > 11 and int(words[11]) or None)
