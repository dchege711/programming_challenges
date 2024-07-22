from typing import List, NamedTuple, Tuple
from math import inf
from heapq import heappush, heappop


class Step(NamedTuple):
    dr: int
    dc: int


def shortest_path_in_grid_with_obstacles_elimination(
    grid: List[List[int]], K: int
) -> int:
    R = len(grid)
    assert R > 0, "There should be at least one row"

    C = len(grid[0])
    assert all(len(row) == C for row in grid), f"All rows should have {C} columns"

    assert grid[0][0] == 0, "(0, 0) should not have an obstacle"
    assert grid[R - 1][C - 1] == 0, "Destination should not have an obstacle"

    possible_steps = [Step(0, -1), Step(0, 1), Step(1, 0), Step(-1, 0)]

    def in_range(r, c):
        return r >= 0 and c >= 0 and r < R and c < C

    def has_obstacle(r, c):
        return grid[r][c] == 1

    def bfs():
        cells_to_visit: List[Tuple[int, Tuple[int, int, int]]] = []
        heappush(cells_to_visit, (0, (0, 0, K)))
        visited = set()

        while cells_to_visit:
            num_steps, (r, c, k) = heappop(cells_to_visit)
            visited.add((r, c))

            if r == R - 1 and c == C - 1:
                return num_steps

            for dr, dc in possible_steps:
                next_r, next_c = r + dr, c + dc

                if not in_range(next_r, next_c):
                    continue

                if (next_r, next_c) in visited:
                    continue

                next_k = k - 1 if has_obstacle(next_r, next_c) else k
                if next_k < 0:
                    continue

                heappush(cells_to_visit, (num_steps + 1, (next_r, next_c, next_k)))

        return inf

    fewest_steps_to_dest = bfs()
    return fewest_steps_to_dest if fewest_steps_to_dest != inf else -1


def test(grid: List[List[int]], k: int, expected: int):
    ans = shortest_path_in_grid_with_obstacles_elimination(grid, k)
    assert ans == expected, f"Expected {expected}, but got {ans}"


if __name__ == "__main__":
    test([[0, 1], [1, 0]], 1, 2)
    test([[0, 1, 1], [1, 1, 1], [1, 0, 0]], 2, 4)
    test([[0, 0, 0], [1, 1, 0], [0, 0, 0], [0, 1, 1], [0, 0, 0]], 1, 6)
