from typing import List, NamedTuple

import pytest

from .quadratic_solution import QuadraticSolution
from .linear_solution import LinearSolution


class RidesConfiguration(NamedTuple):
    land_available_times: List[int]
    land_durations: List[int]
    water_available_times: List[int]
    water_durations: List[int]


all_ride_configs = [
    (RidesConfiguration([5], [3], [1], [10]), 14),
    (RidesConfiguration([2, 8], [4, 1], [6], [3]), 9),
    (RidesConfiguration([82, 14], [42, 30], [6, 54], [91, 71]), 125),
]


@pytest.mark.parametrize("rides_config,expected", all_ride_configs)
def test_quadratic_solution(rides_config: RidesConfiguration, expected: int):
    res = QuadraticSolution().earliest_finish_time(
        rides_config.land_available_times,
        rides_config.land_durations,
        rides_config.water_available_times,
        rides_config.water_durations,
    )
    assert res == expected

@pytest.mark.parametrize("rides_config,expected", all_ride_configs)
def test_linear_solution(rides_config: RidesConfiguration, expected: int):
    res = LinearSolution().earliest_finish_time(
        rides_config.land_available_times,
        rides_config.land_durations,
        rides_config.water_available_times,
        rides_config.water_durations,
    )
    assert res == expected
