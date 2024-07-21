from typing import List, NamedTuple
from math import inf
from functools import lru_cache


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

    def fewest_steps_to_point(r: int, c: int, k: int, visited: set):
        assert (r, c) not in visited, f"Should not revisit ({r}, {c})"
        visited.add((r, c))

        # We start from (0, 0). No steps needed to get here.
        if r == c and c == 0:
            return 0

        # If it's impossible to get here, return `inf`.
        if grid[r][c] == 1 and k < 0:
            return inf

        # Consider all the ways that we could have gotten to (r, c). Pick the
        # one with the fewest number of steps
        fewest_steps = inf
        for dr, dc in possible_steps:
            new_r, new_c = r + dr, c + dc
            if not in_range(new_r, new_c):
                continue

            if (new_r, new_c) in visited:
                continue

            is_empty_cell = grid[new_r][new_c] == 0
            steps = fewest_steps_to_point(
                new_r, new_c, k if is_empty_cell else k - 1, visited
            )

            if steps < fewest_steps:
                fewest_steps = steps

        return fewest_steps + 1

    fewest_steps_to_dest = fewest_steps_to_point(R - 1, C - 1, K, set())
    return fewest_steps_to_dest if fewest_steps_to_dest != inf else -1


def test(grid: List[List[int]], k: int, expected: int):
    ans = shortest_path_in_grid_with_obstacles_elimination(grid, k)
    assert ans == expected, f"Expected {expected}, but got {ans}"


if __name__ == "__main__":
    test([[0, 1], [1, 0]], 1, 2)
    test([[0, 1, 1], [1, 1, 1], [1, 0, 0]], 2, 4)
    test([[0, 0, 0], [1, 1, 0], [0, 0, 0], [0, 1, 1], [0, 0, 0]], 1, 6)

