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

    def has_obstacle(r, c):
        return grid[r][c] == 1

    def dfs(r: int, c: int, k: int, visited: set, depth: int):
        assert k >= 0, f"Should not traverse with a -ve k: {k}"

        prefix = ' ' * depth
        print(f"{prefix}Visiting ({r}, {c}), k={k}")
        assert (r, c) not in visited, f"Should not revisit ({r}, {c})"
        visited.add((r, c))

        # If we've gotten to the destination, return zero. The path length will
        # be computed as the DFS returns.
        if r == R - 1 and c == C - 1:
            print(f"{prefix}Gotten to destination ({R-1}, {C-1})")
            return 0

        # Advance the DFS in all possible unvisited directions
        fewest_steps = inf
        for dr, dc in possible_steps:
            new_r, new_c = r + dr, c + dc
            if not in_range(new_r, new_c):
                continue

            if (new_r, new_c) in visited:
                continue

            new_k = k - 1 if has_obstacle(new_r, new_c) else k
            if new_k < 0:
                continue

            steps = dfs(new_r, new_c, new_k, visited, depth + 1)
            fewest_steps = min(steps, fewest_steps)

        print(f"{prefix}Result: {fewest_steps + 1} to get to ({r}, {c})")
        return fewest_steps + 1

    fewest_steps_to_dest = dfs(0, 0, K, set(), 0)
    return fewest_steps_to_dest if fewest_steps_to_dest != inf else -1


def test(grid: List[List[int]], k: int, expected: int):
    ans = shortest_path_in_grid_with_obstacles_elimination(grid, k)
    assert ans == expected, f"Expected {expected}, but got {ans}"


if __name__ == "__main__":
    # test([[0, 1], [1, 0]], 1, 2)
    # test([[0, 1, 1], [1, 1, 1], [1, 0, 0]], 2, 4)
    test([[0, 0, 0], [1, 1, 0], [0, 0, 0], [0, 1, 1], [0, 0, 0]], 1, 6)

