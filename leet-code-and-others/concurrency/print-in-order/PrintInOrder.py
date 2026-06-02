from typing import Callable

class Foo:
    def __init__(self):
        pass


    def first(self, printFirst: 'Callable[[], None]') -> None:
        printFirst()


    def second(self, printSecond: 'Callable[[], None]') -> None:
        printSecond()


    def third(self, printThird: 'Callable[[], None]') -> None:
        printThird()
