# No Matter How You Slice It

import collections

from utils import data_file_lines


class Claim:
    @classmethod
    def parse(cls, line):
        words = line.split()
        coords = words[2].split(",")
        area = words[3].split("x")
        return Claim(
            int(words[0][1:]),
            int(coords[0]),
            int(coords[1][:-1]),
            int(area[0]),
            int(area[1]),
        )

    def __init__(self, id, x, y, w, h):
        self.id = id
        self.x = x
        self.y = y
        self.width = w
        self.height = h
        self.overlaps = False

    def __repr__(self):
        return f"Claim({self.id}, {self.x}, {self.y}, {self.width}, {self.height})"


def part1(env):
    claims = _read_claims(env)
    overlaps = collections.defaultdict(int)
    for claim in claims.values():
        for i in range(claim.x, claim.x + claim.width):
            for j in range(claim.y, claim.y + claim.height):
                overlaps[(i, j)] += 1
    print(len([k for k, v in overlaps.items() if v > 1]))


def part2(env):
    claims = _read_claims(env)
    overlaps = collections.defaultdict(list)
    for claim in claims.values():
        for i in range(claim.x, claim.x + claim.width):
            for j in range(claim.y, claim.y + claim.height):
                overlaps[(i, j)].append(claim.id)
    for claim_ids in overlaps.values():
        if len(claim_ids) > 1:
            for claim_id in claim_ids:
                claims[claim_id].overlaps = True
    for claim in claims.values():
        if not claim.overlaps:
            print(claim.id)


# Returns dict of {claim_id: claim}
def _read_claims(env):
    claims = [Claim.parse(line) for line in data_file_lines(env)]
    return {claim.id: claim for claim in claims}
