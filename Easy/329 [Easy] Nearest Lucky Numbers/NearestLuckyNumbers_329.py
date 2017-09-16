"""
Finds the nearest lucky numbers. (https://en.wikipedia.org/wiki/Lucky_number)

Input: N (a positive integer)
    103
    225
    997

Output: previousLuckyNumber < n < nextLuckyNumber
    99 < 103 < 105
    223 < 225 < 231
    997 is a lucky number

"""

import sys
from bisect import bisect

def findNearestLuckyNumber(n):
    """
    Find the nearest lucky numbers and print to standard output.

    """

    numbers = list(range(1, n*2 + 1))
    prevLucky = 0
    nextLucky = 0

    currentIndex = 0
    everyN = 2

    while True:
        print("Removing every", everyN, "element...")
        numbers = numbers[::everyN]
        print(numbers)
        currentIndex += 1
        everyN = numbers[currentIndex]


if __name__ == "__main__":
    n = int(sys.argv[1])
    findNearestLuckyNumber(n)
