from math import inf
from typing import List

from .models import Ride

class QuadraticSolution:
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

        def _earliest_finish_time(
            fixed_rides: List[Ride], variable_rides: List[Ride]
        ) -> int:
            earliest_finish_time = inf
            for fixed_ride in fixed_rides:
                # Start `fixed_ride` as soon as it's available.
                fixed_ride_finish_time = fixed_ride.available_time + fixed_ride.duration
                if fixed_ride_finish_time >= earliest_finish_time:
                    continue

                for variable_ride in variable_rides:
                    combined_finish_time = inf
                    if (
                        variable_ride.available_time + variable_ride.duration
                        <= fixed_ride.available_time
                    ):
                        # Prefer taking `variable_ride` before `fixed_ride` for best finish time
                        combined_finish_time = fixed_ride_finish_time
                    else:
                        # Otherwise, take `variable_ride` after `fixed_ride`, waiting if necessary
                        variable_ride_start_time = (
                            fixed_ride_finish_time
                            if variable_ride.available_time <= fixed_ride_finish_time
                            else variable_ride.available_time
                        )
                        combined_finish_time = (
                            variable_ride_start_time + variable_ride.duration
                        )
                    earliest_finish_time = min(
                        earliest_finish_time, combined_finish_time
                    )
            return earliest_finish_time

        return min(
            _earliest_finish_time(water_rides, land_rides),
            _earliest_finish_time(land_rides, water_rides),
        )
