from threading import Lock, Event
from typing import Callable


class FooBar:
    def __init__(self, n):
        self.n = n
        self.lock = Lock()
        self.foo_called = Event()
        self.lock.acquire()

    def foo(self, printFoo: "Callable[[], None]") -> None:
        printFoo()
        self.foo_called.set()

        for _ in range(self.n - 1):
            with self.lock:
                printFoo()

    def bar(self, printBar: "Callable[[], None]") -> None:
        iteration_range = self.n
        if self.foo_called.is_set():
            printBar()
            iteration_range -= 1
            self.lock.release()

        for _ in range(iteration_range):
            with self.lock:
                printBar()
