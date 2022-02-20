---
date: 2022-02-19
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/haskell-meta/
title: Using Haskell for AoC 2021
weight: 100
---

This page contains remarks on Haskell that don't fit neatly into any
of the Advent of Code 2021 problems.

{{% open-comment %}}

Read [[1808.08329] When You Should Use Lists in Haskell (Mostly, You
Should Not)](https://arxiv.org/abs/1808.08329). I am using lists a lot,
and whenever I fret about efficiency, I mumble something to do with lazy
evaluation in Haskell.

{{% /open-comment %}}

## Setting Up My Haskell Environment for AoC

To manage dependencies,
[Cabal](https://cabal.readthedocs.io/en/3.6/getting-started.html) and
[Stack](https://docs.haskellstack.org/en/stable/README.html) are pretty
popular. Stack incorporates the Cabal build system.

{{% comment %}}

The folks at Stack maintain the Stackage package collection, a curated
set of packages from Hackage which are tested for compatibility. This
provides another proxy (in addition to [Category:Libraries -
HaskellWiki](https://wiki.haskell.org/Category:Libraries)) for
determining which third-party library to build on when several claim to
do the job.

{{% /comment %}}

The package structure is of the form:

```txt
2021
├── advent-of-code-y2021.cabal
├── app
│   └── Main.hs
├── src
│   ├── AoC2021.hs
│   └── SonarSweep
│       ├── 01-sonar-sweep.md
│       └── SonarSweep.hs
└── test
    └──  AoC2021Test.hs
```

I ended up using Cabal only as I thought it wouldn't have too many bells
and whistles. `cabal init --interactive` [got most of my `.cabal` file
set up](#my-cabal-file).

`cabal run advent-of-code-y2021` runs my solutions to the AoC problems
(see [app/Main.hs](#my-code-runner)). `cabal run advent-of-code-y2021-test`
runs some checks based on the sample inputs on AoC problem description
(see [test/AoC2021Test.hs](#my-test-runner)).

## VS Code Setup

[Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell),
[Haskell Syntax
Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell),
and
[haskell-linter](https://marketplace.visualstudio.com/items?itemName=hoovercj.haskell-linter)
(which is a wrapper for
[ndmitchell/hlint](https://github.com/ndmitchell/hlint)) are pretty
useful.

`hlint` is especially useful as some of the resources linked in
[https://www.haskell.org/documentation/](https://www.haskell.org/documentation/)
use idioms that have since been improved.

VS Code's Haskell Language Server is currently a bit buggy in that [the
symbols may not
update](https://github.com/haskell/haskell-language-server/issues/366)
leading to erroneous flagging of code that compiles fine. Restarting the
language server every once in a while is a tad inconvenient.

## Debugging Haskell

A lot of the errors are caught by the compiler given the strong typing.

One option is to load the .hs file into GHCi and experiment in there.

```shell
$ ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded package environment from /Users/dchege711/.ghc/x86_64-darwin-8.10.7/environments/default
Prelude> :load src/Dive/Dive.hs
[1 of 1] Compiling Dive.Dive        ( src/Dive/Dive.hs, interpreted )
Ok, one module loaded.
*Dive.Dive>
```

[Haskell GHCi Debug Adapter
Phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
seems like the de-factor debugger for VS Code. I'm having issues of the
form:

```log
test/AoC2021Test.hs:5:1: error:
    Could not load module ‘Paths_advent_of_code_y2021’
    it is a hidden module in the package ‘advent-of-code-y2021-0.1.0.0’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
5 | import Paths_advent_of_code_y2021 (getDataFileName)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

... so I need to figure out how to send arguments from the extension to
GHCi. That said, [GHCi comes with a debugger
included](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/ghci-debugger.html)
so that's promising!

## Others' Solutions

[jhidding](https://jhidding.github.io/aoc2021/#advent-of-code-2021) and
[mstksg](https://github.com/mstksg/advent-of-code-2021) have Haskell
solutions. It'll be nice to compare how they solved the problems. I
don't want to end up perfecting the wrong approach!

## Appendix

### My Cabal File

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/advent-of-code-y2021.cabal"
  highlight="haskell">}}

### My Code Runner

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/app/Main.hs"
  highlight="haskell">}}

### My Test Runner

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/test/AoC2021Test.hs"
  highlight="haskell">}}
