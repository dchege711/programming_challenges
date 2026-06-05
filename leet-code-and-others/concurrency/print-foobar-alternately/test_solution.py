from threading import Thread
from typing import List

import pytest

from .print_foobar_alternately import FooBar

@pytest.mark.parametrize("call_foo_first", [True, False])
def test_solution(call_foo_first: bool):
    n = 2
    foo_bar = FooBar(n)

    log: List[str] = []

    def printFoo():
        log.append("foo")

    def printBar():
        log.append("bar")

    def call_foo():
        foo_bar.foo(printFoo)

    def call_bar():
        foo_bar.bar(printBar)

    thread_a = Thread(target=call_foo)
    thread_b = Thread(target=call_bar)

    threads = (thread_a, thread_b) if call_foo_first else  (thread_b, thread_a)

    for t in threads:
        t.start()

    for t in threads:
        t.join()

    assert log == ["foo", "bar"] * n
