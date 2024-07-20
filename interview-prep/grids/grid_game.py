from typing import List, Tuple, NamedTuple
from math import inf


class GridPath(NamedTuple):
    path: List[Tuple[int, int]]
    score: int


class Solution:
    def grid_game(self, grid: List[List[int]]) -> int:
        assert len(grid) == 2, f"Expected 2 rows, found {len(grid)}"

        n = len(grid[0])
        assert all(len(row) == n for row in grid), f"All rows must have length {n}"

        def maximum_path(r, c) -> GridPath:
            # Base case: We need to pick up the points at (0, 0).
            if r == 0 and c == 0:
                return GridPath([(r, c)], grid[r][c])

            best_score = -inf
            best_path: List[Tuple[int, int]] = []

            # Choose the best option from the top and left neighbors.
            for (dr, dc) in [(0, -1), (-1, 0)]:
                new_r, new_c = r + dr, c + dc
                if new_r < 0 or new_c < 0:
                    continue
                path, score = maximum_path(new_r, new_c)
                if score > best_score:
                    best_score = score
                    best_path = path

            return GridPath(best_path + [(r, c)], best_score + grid[r][c])

        # Let the first robot collect the max points.
        path_r1, _ = maximum_path(1, n - 1)
        for r, c in path_r1:
            grid[r][c] = 0

        # Then have the second robot try its best.
        _, score_r2 = maximum_path(1, n - 1)
        return score_r2


def test(grid: List[List[int]], expected: int):
    ans = Solution().grid_game(grid)
    assert ans == expected, f"Got {ans}, expected {expected}."


if __name__ == "__main__":
    test([[2, 5, 4], [1, 5, 1]], 4)
    test([[3, 3, 1], [8, 5, 2]], 4)
    test([[1, 3, 1, 15], [1, 3, 3, 1]], 7)
    test([[20,3,20,17,2,12,15,17,4,15],[20,10,13,14,15,5,2,3,14,3]], 63) # fails

