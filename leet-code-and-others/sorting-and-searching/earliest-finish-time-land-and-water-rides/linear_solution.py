from math import inf
from typing import List

from .models import Ride


class LinearSolution:
    def earliest_finish_time(
        self,
        land_available_times: List[int],
        land_durations: List[int],
        water_available_times: List[int],
        water_durations: List[int],
    ) -> int:
        # Convenience: Use a data structure that doesn't require lining up index lookups.
        land_rides = [Ride(t, d) for t, d in zip(land_available_times, land_durations)]
        water_rides = [
            Ride(t, d) for t, d in zip(water_available_times, water_durations)
        ]

        def _earliest_finish_time(firsts: List[Ride], seconds: List[Ride]) -> int:
            # Get the earliest possible time of completing a ride from `firsts`.
            earliest_first_finish_time = inf
            for ride in firsts:
                earliest_first_finish_time = min(
                    earliest_first_finish_time, ride.available_time + ride.duration
                )

            # Fixing `first`, compute the earliest time of a ride that starts after.
            earliest_first_then_second_finish_time = inf
            for ride in seconds:
                finish_time = (
                    max(earliest_first_finish_time, ride.available_time) + ride.duration
                )
                earliest_first_then_second_finish_time = min(
                    earliest_first_then_second_finish_time, finish_time
                )
            return earliest_first_then_second_finish_time

        return min(
            _earliest_finish_time(land_rides, water_rides),
            _earliest_finish_time(water_rides, land_rides),
        )
