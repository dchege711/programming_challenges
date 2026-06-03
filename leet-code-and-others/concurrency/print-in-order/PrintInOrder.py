import threading

from typing import Callable

class Foo:
    def __init__(self):
        self._first_is_done = threading.Event()
        self._second_is_done = threading.Event()

    def first(self, printFirst: 'Callable[[], None]') -> None:
        printFirst()
        self._first_is_done.set()

    def second(self, printSecond: 'Callable[[], None]') -> None:
        self._first_is_done.wait()
        printSecond()
        self._second_is_done.set()

    def third(self, printThird: 'Callable[[], None]') -> None:
        self._second_is_done.wait()
        printThird()
