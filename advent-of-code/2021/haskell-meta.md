---
authors:
- Hidding, Johan
- Le, Justin
- Yorgey, Brent
date: 2022-02-19
domains:
- entangled.github.io
- github.com
- hackage.haskell.org
- haskell-haddock.readthedocs.io
- jhidding.github.io
- www.fpcomplete.com
- www.schoolofhaskell.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/haskell-meta/
title: Learning Haskell via AoC 2021
weight: 100
---

This page contains remarks on Haskell that I encountered when working
with source files that span multiple AoC 2021 problems.

{{% cite HiddingAoC2021 %}} and {{% cite LeAoC2021 %}} have Haskell
solutions. It'll be nice to compare how they solved the problems. I
don't want to end up perfecting the wrong approach!

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

{{% cite LeAoC2021 %}} has a more comprehensive setup, e.g. specifying
which problem to run, running tests, and running benchmarks. They even
published [an `advent-of-code-api`
package](https://hackage.haskell.org/package/advent-of-code-api) that
abstracts away the network calls!

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

## Literate Programming

Literate Haskell [intrigued me]({{< ref
"/computer-science/programming-challenges/project-euler/021-amicable-numbers/021-amicable-numbers#PE021Haskell"
>}}), but I don't use it much because it's not well-integrated into VS
Code's intellisense and linting extensions. {{% cite HiddingAoC2021 %}}
uses {{% cite Entangled %}}, which allows one to put code inside
markdown code blocks. Entangled then extracts code and writes it to
traditional source files, and syncs changes made in either the markdown
or the source file.

{{% comment %}}

Entangled is written in Haskell, and mostly by Hidding! Small world. The
project is still in active development, so something to keep an eye on.

It's a pretty nifty concept, and superior to my current approach of
using [a Hugo shortcode](https://gohugo.io/templates/files/) to embed
the source file into the generated HTML file. My current approach
doesn't display both the source and the markdown in the same space when
I'm editing the content.

{{% /comment %}}

{{% cite LeAoC2021 %}} uses {{% cite Haddock %}} to generate markup from
source code. Haddock reminds me of Python's
[Sphinx](https://www.sphinx-doc.org/en/master/), Java's
[Javadoc](https://www.oracle.com/technical-resources/articles/java/javadoc-tool.html)
and JavaScript's [JSDoc](https://jsdoc.app/).

## Overarching Code Considerations

### The Standard Library

{{% cite HiddingAoC2021 %}} uses the `RIO` library to replace the
standard `Prelude`. {{% cite RioLibrary %}} aims to be the de-facto
standard library for Haskell development, as the `base` package is quite
minimal, has some contentious APIs, and avoid re-inventing the wheel in
the name of fewer dependencies.

{{% comment %}}

[More on dependency management]({{< ref
"/computer-science/2020-11-14-software-dependencies">}}).

{{% /comment %}}

{{% comment %}}

`RIO` lives in https://github.com/commercialhaskell alongside Stack, and
Stackage.

{{% /comment %}}

Partial functions (e.g. `head`, `tail`, `init`, `last`, and `(!!)`) in
the `Prelude` (the module with a bunch of standard definitions that get
implicitly imported into every Haskell program) are an example of a
contentious standard library API. A **partial function** is one that
could crash for some inputs (e.g. `head []` crashes because an empty
list doesn't have a first item). A **total function** is one that is
well-defined on _all_ possible inputs. {{% cite Yorgey2014Functions %}}

### Lazy Evaluation

{{% open-comment %}}

Read [[1808.08329] When You Should Use Lists in Haskell (Mostly, You
Should Not)](https://arxiv.org/abs/1808.08329). I am using lists a lot,
and whenever I fret about efficiency, I mumble something to do with lazy
evaluation in Haskell.

{{% /open-comment %}}

Pattern matching drives evaluation. Expressions are only evaluated when
pattern-matched, and only as far as necessary for the match to proceed,
and no farther. For example, given these definitions:

```hs
repeat :: a -> [a]
repeat x = x : repeat x

take :: Int -> [a] -> [a]
take n _ | n <= 0   = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs
```

... we expect `take 3 (repeat 7)` to evaluate to `[7, 7, 7]`. A
step-by-step evaluation looks like:

```hs
  take 3 (repeat 7)
    -- Matches clause 2, which needs the 2nd arg. Expand `repeat 7` one
    -- step.
= take 3 (7 : repeat 7)
    -- Matches clause 3. (3-1) not yet evaluated as not needed for
    -- pattern-matching.
= 7 : take (3-1) (repeat 7)
    -- (3-1) <= 0 forces evaluation of (3-1).
= 7 : take 2 (repeat 7)
    -- Matches clause 2; expand 2nd arg one step.
= 7 : take 2 (7 : repeat 7)
= 7 : 7 : take (2-1) (repeat 7)
= 7 : 7 : take 1 (repeat 7)
= 7 : 7 : take 1 (7 : repeat 7)
= 7 : 7 : 7 : take (1-1) (repeat 7)
= 7 : 7 : 7 : take 0 (repeat 7)
= 7 : 7 : 7 : []
```

The GHC compiler uses graph reduction, where the expression being
evaluated is represented as a graph, so that different parts of the
expression can share pointers to the same subexpression. Haskell's
runtime works out the memoization aspect of dynamic programming on our
behalf!

{{% cite Yorgey2013Laziness %}}

## Appendix

### My Cabal File

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/advent-of-code-y2021.cabal"
  highlight="haskell">}}

### My Code Runner

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/app/Main.hs"
  highlight="haskell">}}

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/src/AoC2021.hs"
  highlight="haskell"
  id="AoC2021.hs">}}

{{% cite HiddingAoC2021-01 %}} has cleaner code for reading the input:

```hs
module Day01 where

import RIO
import qualified RIO.Text as Text

readInput :: MonadIO m => m [Int]
readInput = do
  text <- Text.lines <$> readFileUtf8 "data/day01.txt"
  return $ mapMaybe (readMaybe . Text.unpack) text
```

Notice how `readInput` parses the input and takes care of converting
into expected data types `[Int]` and takes care of parsing uncertainty
with `*Maybe`. In comparison, my [`SonarSweep.num*Increases`]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/sonarsweep/01-sonar-sweep#SonarSweep.hs"
>}}) and [`Dive.productOfFinalPosition*`]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/Dive/02-dive.md#Dive.hs"
>}}) functions receive a `[String]` which they then parse into intended
types. Furthermore, because I'm reading the input in [AoC2021.hs]({{<
ref "#AoC2021.hs" >}}), it'll be a neat way of comparing input-parsing
techniques.

### My Test Runner

{{< readfile
  file="/content/computer-science/programming-challenges/advent-of-code/2021/test/AoC2021Test.hs"
  highlight="haskell">}}

## References

1. {{< citation
  id="HiddingAoC2021"
  author="Johan Hidding"
  title="Advent of Code 2021"
  url="https://jhidding.github.io/aoc2021/#advent-of-code-2021"
  accessed="2022-02-20" >}}

1. {{< citation
  id="Entangled"
  author="Johan Hidding"
  title="Entangled: literate programming for the new millennium"
  url="https://entangled.github.io/"
  url_2="https://github.com/entangled/entangled/"
  accessed="2022-02-20" >}}

1. {{< citation
  id="RioLibrary"
  title="rio: A standard library for Haskell"
  url="https://hackage.haskell.org/package/rio-0.1.21.0#readme"
  url_2="https://www.fpcomplete.com/haskell/library/rio/"
  url_3="https://github.com/commercialhaskell/rio"
  accessed="2022-02-20" >}}

1. {{< citation
  id="LeAoC2021"
  author="Justin Le"
  title="mstksg/advent-of-code-2021: 🎅🌟❄️☃️🎄🎁"
  url="https://github.com/mstksg/advent-of-code-2021"
  accessed="2022-02-20" >}}

1. {{< citation
  id="Haddock"
  title="Welcome to Haddock’s documentation! — Haddock 1.0 documentation"
  url="https://haskell-haddock.readthedocs.io/en/latest/"
  accessed="2022-02-20" >}}

1. {{< citation
  id="HiddingAoC2021-01"
  author="Johan Hidding"
  title="Advent of Code 2021"
  url="https://jhidding.github.io/aoc2021/#day-1-sonar-sweep"
  accessed="2022-02-20" >}}

1. {{< citation
  id="MonadIO"
  title="Control.Monad.IO.Class"
  url="https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Control-Monad-IO-Class.html"
  accessed="2022-02-21" >}}

1. {{< citation
  id="Yorgey2014Functions"
  author="Brent Yorgey"
  title="3: Recursion Patterns, Polymorphism, and the Prelude - School of Haskell"
  url="https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/3-recursion-patterns-polymorphism-and-the-prelude#total-and-partial-functions"
  date="2014-07-14"
  accessed="2022-02-21" >}}

1. {{< citation
  id="Yorgey2013Laziness"
  author="Brent Yorgey"
  title="6: Laziness - School of Haskell"
  url="https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/6-laziness"
  accessed="2022-02-21" >}}
