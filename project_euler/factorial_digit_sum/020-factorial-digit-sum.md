---
date: 2022-01-15
domains:
- lisp-lang.org
- projecteuler.net
- stackoverflow.com
- www.youtube.com
local_url: http://localhost:1313/computer-science/programming-challenges/project-euler/020-factorial-digit-sum/020-factorial-digit-sum/
tags:
- lisp
- programming-environment
title: 020. Factorial Digit Sum
weight: 20
---

## Problem Statement {{% cite ProjectEuler020 %}}

\\(n!\\) means \\(n \times (n - 1) \times ... \times 3 \times 2 \times
1\\).

For example, \\(10! = 10 \times 9 \times ... \times 3 \times 2 \times 1
= 3628800\\), and the sum of the digits in the number \\(10!\\) is \\(3
\+ 6 + 2 + 8 + 8 + 0 + 0 = 27\\).

Find the sum of the digits in the number \\(100!\\)

## My Solution

It doesn't look like we can do without calculating the actual value of
\\(100!\\), and that is something that is too burdensome to compute by
hand. Programming it is!

How big is \\(100!\\)? Most programming languages have built-in support
for numbers less than \\(2^{64} - 1\\).

{{% figure
  src=`/img/computer-science/programming-challenges/project-euler/020-comparing-100f-with-2-to-64.jpg`

  caption=`[Failed attempt at comparing \\(2^{64}\\) to
  \\(100!\\)](https://onedrive.live.com/view.aspx?resid=D3A50A924AE586F1%214828&id=documents&wd=target%28Project%20Euler.one%7C4AFF21B6-FEDE-2540-83FB-3D9780501572%2F020.%20Factorial%20Digit%20Sum%7C03959692-0016-3048-AC6E-45D62929E872%2F%29)`
  %}}

Using Stirling's approximation, \\(\log_{k}(n!) \approx n \log_{k}(n) -
n \cdot \log_{k}(e)\\), {{% cite Inavda2019 %}} determines that \\(n!\\)
starts outgrowing \\(2^{n}\\) when \\(n \approx e \cdot k\\). In
practice tend to use \\(n \gg e \cdot k\\), so we assume that factorials
grow faster than exponentials.

In our case, \\(100 \gg 2 \cdot e\\), and so the most convenient
language for this problem is one that has large integer support, e.g.
Python.

```py
>>> from math import factorial
>>> s = f"{factorial(100)}"
>>> sum(int(i) for i in s)
648
```

But what about Lisp? I've seen it enough times on Hacker News to want to
toy with it. [Common-Lisp.net](https://common-lisp.net/) seems like a
good starting point. [Portacle](https://portacle.github.io/#get-mac),
the recommended beginner-friendly way to run Lisp, does not work for me
[despite trying these
workarounds](https://github.com/portacle/portacle/issues/53). {{% cite
LispLangOrg %}} has some alternate instructions.

{{% tag lisp %}}
{{% tag programming-environment %}}

Common Lisp is a standard, and [Steel Bank Common Lisp
(SBCL)](http://www.sbcl.org/) is a popular Common Lisp implementation.
[Quicklisp](https://www.quicklisp.org/beta/) is a package manager.
[SLIME: The Superior Lisp Interaction Mode for
Emacs](https://common-lisp.net/project/slime/) is a popular Common Lisp
IDE built on Emacs. These components might not have been as apparent if
Portacle worked as expected. Furthermore, I took less time to get my
setup via this "longer" route.

{{% comment %}}

Mistakenly did `brew install emacs` instead of `brew install --cask
emacs` as advocated for by [GNU Emacs download - GNU
Project](https://www.gnu.org/software/emacs/download.html).
[`homebrew-cask` extends Homebrew by installing and managing GUI macOS
applications](https://github.com/Homebrew/homebrew-cask).

{{% /comment %}}

{{% comment %}}

Emacs has an emphasis on being productive from the keyboard, e.g.
providing alternatives to using the arrow keys to avoid leaving the
touch typing position. However, I still like VS Code because of the
extensions that I have. Presumably, once I customize Emacs in a similar
way, I'll move into the Emacs camp and join the eternal Emacs vs. Vim
wars. [Emacs
Keymap](https://marketplace.visualstudio.com/items?itemName=hiro-sun.vscode-emacs)
provides Emacs keyboard shortcuts in VS Code.

{{% /comment %}}

{{% cite Baggers2013 %}} has a nice video of setting up Common Lisp,
Emacs, Slime and Quicklisp such that we can iterate on the code
efficiently. I think that's pretty cool. Python's REPL is the closest
that I've come to such a development environment.

{{< readfile
  file=`content/computer-science/programming-challenges/project_euler/factorial_digit_sum/factorial_digit_sum.lisp`
  highlight="lisp" >}}

Emacs + Slime has a steep learning curve. The keyboard bindings are
unlike anything that I frequently use. However, I think we did use
either Vim or Emacs in COS 217, because `C-x C-s` to save a file feels
familiar.

First impressions of Lisp development: the code is quite readable.
Pleasantly surprised by `sum (parse-integer (string c)) into total`.

## Learning from Others' Solutions {{% cite ProjectEulerThread020 %}}

I could have gone without the integer to string to characters to
integers roundtrip, e.g.

```py
def sum_of_digits(n):
    total = 0
    while (n > 0):
            total += n % 10
            n = int(n / 10)
    return total
```

{{% comment %}}

Ruby and Haskell seem elegant. Should try them in the next Project Euler
challenge.

{{% /comment %}}

## References

1. {{< citation
  id="ProjectEuler020"
  title="#20 Factorial digit sum - Project Euler"
  url="https://projecteuler.net/problem=20"
  accessed="2022-01-15">}}

1. {{< citation
  id="Inavda2019"
  title="Which function grows faster, exponential or factorial? - Stack Overflow"
  accessed="2022-01-16"
  url="https://stackoverflow.com/a/55301991">}}

1. {{< citation
  id="LispLangOrg"
  title="Getting Started | Common Lisp"
  url="https://lisp-lang.org/learn/getting-started/"
  accessed="2022-01-16">}}

1. {{< citation
  id="Baggers2013"
  title="Installing Common Lisp, Emacs, Slime & Quicklisp - YouTube"
  url="https://www.youtube.com/watch?v=VnWVu8VVDbI&t=291s"
  accessed="2022-01-16">}}

1. {{< citation
  id="ProjectEulerThread020"
  title="Thread 20 - Project Euler"
  url="https://projecteuler.net/thread=20"
  accessed="2022-01-16">}}
