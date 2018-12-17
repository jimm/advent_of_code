# Author: Christian Careaga (christian.careaga7@gmail.com)
# A* Pathfinding in Python (2.7)
# Please give credit if used
#
# Modified by jimm for day 15 exercise (no diagonal movement, use classes).

from collections import namedtuple
from heapq import *

Point = namedtuple("Point", ["x", "y"])

def heuristic(a, b):
    # return (b.x - a.x) ** 2 + (b.y - a.y) ** 2
    # manhattan distance
    return abs(b.x - a.x) + abs(b.y - a.y)


def astar(world, start, goal):
    # this was old diagonal movement data
    # neighbors = [(0,1),(0,-1),(1,0),(-1,0),(1,1),(1,-1),(-1,1),(-1,-1)]
    # no diagonal movement, in REVERSE day15 rank order
    neighbors = [Point(0, 1), Point(1, 0), Point(-1, 0), Point(0, -1)]

    close_set = set()
    came_from = {}
    gscore = {start: 0}
    fscore = {start: heuristic(start, goal)}
    oheap = []

    heappush(oheap, (fscore[start], start))

    while oheap:
        current = heappop(oheap)[1]
        if current == goal:
            data = []
            while current in came_from:
                data.append(current)
                current = came_from[current]
            return data

        close_set.add(current)
        for p in neighbors:
            neighbor = Point(current.x + p.x, current.y + p.y)
            tentative_g_score = gscore[current] + heuristic(current, neighbor)
            if 0 <= neighbor.y < world.height:
                if 0 <= neighbor.x < world.width:
                    if not world.is_empty(neighbor):
                        continue
                else:
                    # world bound y walls
                    continue
            else:
                # world bound x walls
                continue

            if neighbor in close_set and tentative_g_score >= gscore.get(neighbor, 0):
                continue

            if tentative_g_score < gscore.get(neighbor, 0) or neighbor not in [
                i[1] for i in oheap
            ]:
                came_from[neighbor] = current
                gscore[neighbor] = tentative_g_score
                fscore[neighbor] = tentative_g_score + heuristic(neighbor, goal)
                heappush(oheap, (fscore[neighbor], neighbor))

    return False
