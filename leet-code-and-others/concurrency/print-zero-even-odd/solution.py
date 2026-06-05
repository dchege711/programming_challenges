from enum import Enum
from functools import partial
from threading import Condition
from typing import Callable

class Turn(Enum):
    ZERO = 1
    EVEN = 2
    ODD = 3

class ZeroEvenOdd:
    def __init__(self, n: int):
        self.n = n
        self.condition = Condition()
        self.indexToPrint = 1 # 1-based for easier arithmetic in _is_turn

    def _is_turn(self, turn: Turn) -> bool:
        if self.indexToPrint % 2 == 1:
            return turn == Turn.ZERO
        elif self.indexToPrint % 4 == 0:
            return turn == Turn.EVEN
        return turn == Turn.ODD

    def _print_and_increment(self, printNumber: 'Callable[[int], None]'):
        num_to_print = 0 if self.indexToPrint % 2 == 1 else int(self.indexToPrint / 2)
        printNumber(num_to_print)
        self.indexToPrint += 1

    def zero(self, printNumber: 'Callable[[int], None]') -> None:
        while (self.indexToPrint <= self.n * 2):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.ZERO))
                self._print_and_increment(printNumber)
                self.condition.notify_all()


    def even(self, printNumber: 'Callable[[int], None]') -> None:
        while (self.indexToPrint <= self.n * 2):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.EVEN))
                self._print_and_increment(printNumber)
                self.condition.notify_all()


    def odd(self, printNumber: 'Callable[[int], None]') -> None:
        while (self.indexToPrint <= self.n * 2):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.ODD))
                self._print_and_increment(printNumber)
                self.condition.notify_all()
