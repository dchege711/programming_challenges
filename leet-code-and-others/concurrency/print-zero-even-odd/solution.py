from enum import Enum
from functools import partial
from threading import Condition
from typing import Callable

class Turn(Enum):
    ZERO = 1
    EVEN = 2
    ODD = 3
    TERMINATED = 4

class ZeroEvenOdd:
    def __init__(self, n: int):
        self.n = n
        self.condition = Condition()
        self.indexToPrint = 1 # 1-based for easier arithmetic in _is_turn

    def _get_current_turn(self) -> Turn:
        if self.indexToPrint > self.n * 2:
            return Turn.TERMINATED
        elif self.indexToPrint % 2 == 1:
            return Turn.ZERO
        elif self.indexToPrint % 4 == 0:
            return Turn.EVEN
        return Turn.ODD

    def _is_turn(self, turn: Turn) -> bool:
        return self._get_current_turn() == turn

    def _print_and_increment(self, printNumber: 'Callable[[int], None]'):
        num_to_print = 0 if self.indexToPrint % 2 == 1 else int(self.indexToPrint / 2)
        printNumber(num_to_print)
        self.indexToPrint += 1

    def _wait_for_turn_and_print(self, printNumber: 'Callable[[int], None]', turn: Turn):
        while not self._is_turn(Turn.TERMINATED):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, turn))
                self._print_and_increment(printNumber)
                self.condition.notify_all()

    def zero(self, printNumber: 'Callable[[int], None]') -> None:
        self._wait_for_turn_and_print(printNumber, Turn.ZERO)

    def even(self, printNumber: 'Callable[[int], None]') -> None:
        self._wait_for_turn_and_print(printNumber, Turn.EVEN)

    def odd(self, printNumber: 'Callable[[int], None]') -> None:
        self._wait_for_turn_and_print(printNumber, Turn.ODD)
