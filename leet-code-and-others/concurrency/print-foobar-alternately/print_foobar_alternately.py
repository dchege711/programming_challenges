from threading import Barrier
from typing import Callable

class FooBar:
    def __init__(self, n):
        self.n = n
        self.barrier = Barrier(2)

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            printFoo()
            self.barrier.wait()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.barrier.wait()
            printBar()
