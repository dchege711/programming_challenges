from threading import Event
from typing import Callable


class FooBar:
    def __init__(self, n):
        self.n = n
        self.foos_turn = Event()
        self.bars_turn = Event()
        self.foos_turn.set()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.foos_turn.wait()
            printFoo()
            self.foos_turn.clear()
            self.bars_turn.set()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.bars_turn.wait()
            printBar()
            self.bars_turn.clear()
            self.foos_turn.set()
