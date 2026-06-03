from math import inf

from typing import List

class Solution:
    def earliestFinishTime(
            self,
            land_start_times: List[int],
            land_durations: List[int],
            water_start_times: List[int],
            water_durations: List[int]) -> int:
        earliest_finish_time = inf
        for (li, land_start_time) in enumerate(land_start_times):
            land_finish_time = land_start_time + land_durations[li]
            if land_finish_time >= earliest_finish_time:
                continue

            for (wi, water_start_time) in enumerate(water_start_times):
                water_finish_time = water_start_time + water_durations[wi]
                if (
                    water_finish_time <= earliest_finish_time
                    and self.__has_no_inner_overlap(
                        land_start_time, land_finish_time, water_start_time, water_finish_time)):
                    candidate_finish_time = max(land_finish_time, water_finish_time)
                    earliest_finish_time = min(earliest_finish_time, candidate_finish_time)

        return earliest_finish_time

    def __has_no_inner_overlap(self, a_start: int, a_end: int, b_start: int, b_end: int) -> bool:
        if (a_start > a_end or b_start > b_end):
            raise ValueError(f"Invalid intervals: ({a_start}, {a_end}), ({b_start}, {b_end})")

        return (a_start <= b_start and a_end <= b_start) or (a_start > b_start and a_start >= b_end)

if __name__ == "__main__":
    solution = Solution()
    print(solution.earliestFinishTime([5], [3], [1], [10]))
