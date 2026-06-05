from threading import BoundedSemaphore
from typing import Callable

class FooBar:
    def __init__(self, n):
        self.n = n
        self.foo_gate = BoundedSemaphore(1)
        self.bar_gate = BoundedSemaphore(1)
        self.bar_gate.acquire()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.foo_gate.acquire()
            printFoo()
            self.bar_gate.release()

    def bar(self, printBar: "Callable[[], None]") -> None:
        for _ in range(self.n):
            self.bar_gate.acquire()
            printBar()
            self.foo_gate.release()
