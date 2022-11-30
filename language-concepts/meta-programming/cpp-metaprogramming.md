---
affiliations:
- Apple
cited-authors:
- Kremenek, Ted
date: 2022-11-28
domains:
- clang-analyzer.llvm.org
- en.wikipedia.org
- gcc.gnu.org
- llvm.org
- news.ycombinator.com
- releases.llvm.org
- source.chromium.org
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/meta-programming/cpp-metaprogramming/
tags:
- code-hygiene
title: C++ Meta-Programming
---

## Clang, LLVM, GCC, and MSVC

**LLVM** is an umbrella project, with several sub-projects, e.g. LLVM Core and
Clang. **LLVM Core** libraries provide an optimizer and code generator for
different CPUs. **Clang** is an "LLVM native" C/C++/Objective-C compiler which
aims for fast compilation, useful error and warning messages, and a platform for
building source-level tools. The **Clang Static Analyzer** and **clang-tidy**
are examples of such tools. {{% cite LLVM %}}

{{% comment %}}

So if I were to create a programming language, I can define a transformation
into LLVM intermediate representation (LLVM IR), and that will make use of LLVM
core to optimize it? Sweet!

{{% /comment %}}

**Microsoft Visual C++ (MSVC)** is Microsoft's proprietary compiler for C, C++
and C++/CX. It is bundled with Visual Studio. {{% cite WikiMSVC %}}

**GNU's Compiler Collection (GCC)** includes front ends for C, C++, Objective-C,
Fortran, Ada, Go, and D. {{% cite GCC %}}

LLVM, MSVC and GCC also have implementations (`libc++`, `MSVC STL`, and
`libstdc++`, respectively) of the C++ standard library. {{% cite WikiCppStdLib
%}}

{{% comment %}}

Found myself with Clang, probably by the fact that I work on a Chromium-based
browser, which already uses Clang. I expect that a lot of research will be on
the open-source Clang and GCC compilers, as opposed to proprietary ones such as
MSVC.

When Chrome/Chromium moved to Clang, {{% cite HNChromeClang2017 %}} bore
sentiment of Google being more invested in LLVM/Clang than in GNU/GCC. There are
politics when it comes to C++ toolchains.

{{% /comment %}}

## Improving Code Using Clang Tools

{{% tag code-hygiene %}}

{{% priors %}}

Anticipated capabilities from clang tools:

* Remove branches that are never executed in practice (reduces complexity).
* Increase `const` correctness to allow clients to pass around `const`
  references/pointers.
* Increase cohesiveness within a module, and reduce coupling with other modules.
* Flag/Fix violations of rules of thumb from static analyzers.
* Remove unused includes from source files.

{{% /priors %}}

The Chromium project has examples of "real-world" improvements via Clang tools,
e.g.:

* Adding `std::move` after running some heuristics, e.g., local variable or
  param, no qualifiers, not a reference nor pointer, not a constructor, is not
  captured by a lambda, etc.
* Updating conventions, e.g., `int mySuperVariable` to `int my_super_variable`
  and `const int maxThings` to `const int kMaxThings`.
* Updating API usage, e.g., `::base::ListValue::GetSize` to `GetList().size`,
  `std::string("")` to `std::string()`.

{{% cite ChromiumClangScripts %}}

{{% comment %}}

The vibe that I'm getting is that one can only go so far with find + replace.
Some changes require treating the source files as C++ source code instead of
simply text. For such changes, trying to craft a regex (or multiple passes) will
become too tedious, buggy, or even outright infeasible.

{{% /comment %}}

### Clang Static Analyzer

Uses a collection of algorithms and techniques to analyze source code in order
to find bugs that are traditionally found using run-time debugging techniques
such as testing. Slower than compilation. May have false positives. {{% cite
ClangStaticAnalyzer %}}

#### False Positives

False positives may occur due to analysis imprecision, e.g. false paths,
insufficient knowledge about the program. A sample false paths analysis:

```c
int f(int y) {
  int x;

  if (y) x = 1;

  printf("%d\n", y);

  if (y) return x;

  return y;
}
```

```shell
$ clang -warn-uninit-values /tmp/test.c
t.c:13:12: warning: use of uninitialized variable
  return x;
         ^
```

There are two feasible paths: neither branch taken `(y == 0)`, and both branches
taken `(y != 0)`, but the analyzer issues a bogus warning on an infeasible path
(not taking the first branch, but taking the second). {{% cite Kremenek2008 %}}

{{% comment %}}

The analyzer has gotten smarter since {{% cite Kremenek2008 %}}. `clang
-Wuninitialized /tmp/test.c` no longer issues that bogus warning.

{{% /comment %}}

#### Static Analyzer Algorithms

More precise analysis can reduce false positives. {{% cite Kremenek2008 %}}

**Flow-Sensitive Analyses** reason about the flow of values without considering
path-specific information:

```c
if (x == 0) ++x;  // x == ?
else x = 2;       // x == 2
y = x;            // x == ?, y == ?
```

... but they are linear-time algorithms. {{% cite Kremenek2008 %}}

**Path-Sensitive Analyses** reason about individual paths and guards on
branches:

```c
if (x == 0) ++x;  // x == 1
else x = 2;       // x == 2
y == x;           // (x == 1, y == 1) or (x == 2, y == 2)
```

... and can therefore avoid false positives based on infeasible paths. However,
they have a worst-case exponential-time, but there are tricks to reduce
complexity in practice. {{% cite Kremenek2008 %}}

{{% comment %}}

At this point, the takeaway can be, "Figure out how to run Clang's static
analyzer on your codebase, read the report, and then fix the legitimate issues."
Further reading might help illuminate the root cause of a false positive, but
that can be deferred until you encounter the false positive.

{{% /comment %}}

{{% cite ClangStaticAnalyzerDocs %}}

## References

1. {{< citation
  id="LLVM"
  title="The LLVM Compiler Infrastructure Project"
  url="https://llvm.org/"
  accessed="2022-11-28" >}}

1. {{< citation
	id="WikiMSVC"
  title="Microsoft Visual C++"
	url="https://en.wikipedia.org/wiki/Microsoft_Visual_C%2B%2B"
	accessed="2022-11-29" >}}

1. {{< citation
	id="GCC"
  title="GCC, the GNU Compiler Collection - GNU Project"
	url="https://gcc.gnu.org/"
	accessed="2022-11-29" >}}

1. {{< citation
  id="ChromiumClangScripts"
	title="tools/clang/ - Chromium Code Search"
	url="https://source.chromium.org/chromium/chromium/src/+/main:tools/clang/"
	accessed="2022-11-29" >}}

1. {{< citation
	id="WikiCppStdLib"
  title="C++ Standard Library"
	url="https://en.wikipedia.org/wiki/C%2B%2B_Standard_Library"
	accessed="2022-11-29" >}}

1. {{< citation
	id="HNChromeClang2017"
  title="Chrome now uses clang for production builds on Linux | Hacker News"
	url="https://news.ycombinator.com/item?id=8618779"
  date="2014-11-17"
	accessed="2022-11-29" >}}

1. {{< citation
	id="ClangStaticAnalyzer"
  title="Clang Static Analyzer"
	url="https://clang-analyzer.llvm.org/"
	accessed="2022-11-29" >}}

1. {{< citation
	id="ClangStaticAnalyzerDocs"
  title="Clang Static Analyzer — Clang 14.0.0 documentation"
	url="https://releases.llvm.org/14.0.0/tools/clang/docs/ClangStaticAnalyzer.html"
	accessed="2022-11-29" >}}

1. {{< citation
  id="Kremenek2008"
	author="Ted Kremenek"
  title="Finding Software Bugs with the Clang Static Analyzer"
	url="https://llvm.org/devmtg/2008-08/Kremenek_StaticAnalyzer.pdf"
  year="2008"
  affiliation="Apple"
	accessed="2022-11-30" >}}
