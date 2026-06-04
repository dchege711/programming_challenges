from math import inf

from typing import List, NamedTuple

import pytest


class Ride(NamedTuple):
    available_time: int
    duration: int


class Solution:
    def earliest_finish_time(
        self,
        land_available_times: List[int],
        land_durations: List[int],
        water_available_times: List[int],
        water_durations: List[int],
    ) -> int:
        land_rides = [Ride(t, d) for t, d in zip(land_available_times, land_durations)]
        water_rides = [
            Ride(t, d) for t, d in zip(water_available_times, water_durations)
        ]

        def _earliest_finish_time(
            fixed_rides: List[Ride], variable_rides: List[Ride]
        ) -> int:
            earliest_finish_time = inf
            for fixed_ride in fixed_rides:
                fixed_ride_finish_time = fixed_ride.available_time + fixed_ride.duration
                for variable_ride in variable_rides:
                    combined_finish_time = (
                        fixed_ride_finish_time
                        if (
                            variable_ride.available_time + variable_ride.duration
                            <= fixed_ride.available_time
                        )
                        else fixed_ride_finish_time + variable_ride.duration
                    )
                    earliest_finish_time = min(
                        earliest_finish_time, combined_finish_time
                    )
            return earliest_finish_time

        return min(
            _earliest_finish_time(water_rides, land_rides),
            _earliest_finish_time(land_rides, water_rides),
        )


class RidesConfiguration(NamedTuple):
    land_available_times: List[int]
    land_durations: List[int]
    water_available_times: List[int]
    water_durations: List[int]


@pytest.mark.parametrize(
    "rides_config,expected",
    [
        (RidesConfiguration([5], [3], [1], [10]), 14),
        (RidesConfiguration([2, 8], [4, 1], [6], [3]), 9),
    ],
)
def test_cases(rides_config: RidesConfiguration, expected: int):
    res = Solution().earliest_finish_time(
        rides_config.land_available_times,
        rides_config.land_durations,
        rides_config.water_available_times,
        rides_config.water_durations,
    )
    assert res == expected
