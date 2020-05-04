---
title: "06. Python Tips And Tricks"
draft: true
weight: 6
---

## Prompt

What are some of the tips, tricks and go-to approaches you use in Python? [Link to /r/dailyprogrammer thread](https://www.reddit.com/r/dailyprogrammer/comments/2d957i/weekly_6_python_tips_and_tricks/)

## Observations

Python's `readline` method for `File` objects doesn't match my intuition. I'd expect `f.readline()` to throw an `EOFError` if there's nothing left to read. However, Python returns an empty string (one without a trailing `\n` character) {{< cite pyDocsMethodsOfFileObjects >}} The `EOFError` exception is only thrown when the `input()` function hits an EOF condition without reading any data.

## References

1. {{< citation
    id="pyDocsMethodsOfFileObjects"
    url="https://docs.python.org/3/tutorial/inputoutput.html#methods-of-file-objects"
    title="Methods of File Objects"
    publication="Python 3.8 Docs">}}

1. {{< citation
    id="pyEOFError"
    url="https://docs.python.org/3.8/library/exceptions.html#EOFError"
    title="Concrete exceptions: EOFError"
    publication="Python 3.8 Docs">}}
