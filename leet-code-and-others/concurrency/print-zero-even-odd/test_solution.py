from itertools import permutations, product
from threading import Thread
from typing import Tuple

import pytest

from .solution import ZeroEvenOdd

thread_execution_order = list(permutations(["zero", "even", "odd"]))

@pytest.mark.parametrize(("n", "thread_ids"), list(product([5], thread_execution_order)))
def test_solution(n: int, thread_ids: Tuple[str, str, str]):

    printed_numbers: list[int] = []

    zero_even_odd = ZeroEvenOdd(n)
    threads = {
        "zero": Thread(target=zero_even_odd.zero, args=(printed_numbers.append,)),
        "even": Thread(target=zero_even_odd.even, args=(printed_numbers.append,)),
        "odd": Thread(target=zero_even_odd.odd, args=(printed_numbers.append,)),
    }

    for thread_id in thread_ids:
        threads[thread_id].start()

    for thread_id in thread_ids:
        threads[thread_id].join()

    expected_numbers = []
    for i in range(1, n + 1):
        expected_numbers.append(0)
        expected_numbers.append(i)

    assert printed_numbers == expected_numbers


