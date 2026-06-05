from threading import Condition
from enum import Enum
from functools import partial
from typing import Callable

class Turn(Enum):
    FOO = 1
    BAR = 2

class FooBar:
    def __init__(self, n):
        self.n = n
        self.condition = Condition()
        self.current_turn = Turn.FOO

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.FOO))
                printFoo()
                self.current_turn = Turn.BAR
                self.condition.notify()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            with self.condition:
                self.condition.wait_for(partial(self._is_turn, Turn.BAR))
                printBar()
                self.current_turn = Turn.FOO
                self.condition.notify()

    def _is_turn(self, turn: Turn) -> bool:
        return self.current_turn == turn
