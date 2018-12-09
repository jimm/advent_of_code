# Puzzle Name

from utils import *


# TODO clamp function
class MarbleMania:
    def __init__(self, num_players, num_marbles, testing=False):
        self.scores = [0] * num_players
        self.next_player = 0
        self.circle = [0]
        self.current_marble_index = 0
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
        place_after_loc = (self.current_marble_index + 1) % len(self.circle)
        if place_after_loc == len(self.circle):
            self.circle = self.circle.append(self.next_marble)
            self.current_marble_index = len(self.circle) - 1
        else:
            i = place_after_loc + 1
            self.circle = self.circle[:i] + [self.next_marble] + self.circle[i:]
            self.current_marble_index = i
        self.current_marble_index = place_after_loc + 1
        return True

    def _take_funky_turn(self):
        self.scores[self.next_player] += self.next_marble
        remove_index = self.current_marble_index - 7
        while remove_index < 0:
            remove_index += len(self.circle)
        remove_index = remove_index % len(self.circle)
        self.scores[self.next_player] += self.circle[remove_index]
        self.circle = self.circle[:remove_index] + self.circle[remove_index + 1 :]
        self.current_marble_index = remove_index
        return True             # make sure _take_normal_turn() not called

    def _print_state(self):
        if not self.testing:
            return
        for i in range(len(self.circle)):
            is_curr = i == self.current_marble_index
            print(f" {is_curr and '(' or ''}{self.circle[i]}{is_curr and ')' or ''}", end='')
        print()

    def __str__(self):
        return f"{self.num_marbles} marbles, next is {self.next_marble}\n  circle: {self.circle}\n  scores: {self.scores}"


def part1(testing=False):
    # answer only returned if testing, else None
    for num_players, last_marble_points, answer_high_score in _games(1, testing):
        print(f"{num_players}, {last_marble_points}, {answer_high_score}")
        game = MarbleMania(num_players, last_marble_points+1, testing and num_players < 10)
        max_score = game.play()
        print(max_score)


def part2(testing=False):
    num_players, last_marble_points, _ = next(_games(2, False))
    game = MarbleMania(num_players, (last_marble_points*100)+1, False)
    max_score = game.play()
    print(max_score)


def _games(part_num, testing):
    for line in data_file_lines(9, testing, part_num):
        words = line.split()
        yield (int(words[0]), int(words[6]), len(words) > 11 and int(words[11]) or None)
