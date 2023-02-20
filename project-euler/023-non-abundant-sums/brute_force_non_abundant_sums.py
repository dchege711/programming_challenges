#!/usr/bin/env python

from itertools import combinations_with_replacement

def sum_of_proper_divisors(n):
    if n == 0: return 0
    orig_n = n
    s, factor = 1, 2
    while factor * factor <= n and n > 1: # We only need to check up to sqrt(n)
        multiplicity = 0
        while n % factor == 0:
            n /= factor
            multiplicity += 1

        if multiplicity > 0:
            s *= (factor ** (multiplicity + 1) - 1) / (factor - 1)

        factor = 3 if factor == 2 else factor + 2

    # Account for any remaining prime factor greater than sqrt(n). There will be
    # at most one such factor. (n^2 - 1) / (n-1) simplifies to (n + 1).
    if n > 1: s *= (n + 1)

    return s - orig_n

def generate_abundant_nums(lo, hi):
    """
    Generate the abundant numbers in the range [lo, hi], in sorted order.
    """
    for n in range(lo, hi + 1):
        if sum_of_proper_divisors(n) > n: yield n

def sum_of_non_abundant_sums():
    """
    Solution for https://projecteuler.net/problem=23
    """
    # (28123, inf] can all be written as the sum of two abundant numbers. Start
    # by assuming all numbers in [1, 28123] cannot be expressed as the sum of
    # two abundant numbers.
    N = 28123
    ans = N * (N + 1) / 2

    # Generate all numbers that can be expressed as the sum of two abundant
    # numbers. Stop at 28124 because beyond that is a futile exercise.
    abundant_sums = set([])
    abundant_nums = generate_abundant_nums(1, N - 1)
    for (a, b) in combinations_with_replacement(abundant_nums, 2):
        n = a + b
        if n > N: continue
        else: abundant_sums.add(n)

    for n in abundant_sums:
        ans -= n

    return ans

if __name__ == "__main__":
    print(sum_of_non_abundant_sums())
