---
title: "Continuous Integration Tooling"
date: 2024-06-09
---

What automatic tools can I add to keep code quality high?

## CodeQL

CodeQL is a tool that runs variant analysis on code. The idea is that we
create a query from a known vulnerability, e.g., SQL injection, and then
run it against a codebase to find instances of that vulnerability. {{%
cite CodeQL %}} GitHub authorizes the use of CodeQL for public repos,
and so we are covered {{% cite CodeQLLicense %}}.
[ql-analysis.sh](https://github.com/dchege711/study_buddy/blob/main/ql-analysis.sh)
has a recipe for running the analysis locally in the repo. CodeQL also
runs on every PR, and blocks check-in if new vulnerabilities are
discovered.

{{% comment %}}

Variant analysis is a common term in the medical field for analyzing
genetic variants between individuals of a population. {{% cite
variantAnalysis %}} Search engine results bias heavily towards the
medical interpretation, implying that the use of the term is pretty new
in the software field.

{{% /comment %}}

## Formatting

Ensuring a consistent format is one of those things that should be set
once and automated. While I don't have collaborators who would come with
their own style, going through the exercise should help in future
collaborative endeavors.

I've had prior experiences with {{% cite dprint %}}, but {{% cite
Prettier %}} is the front-runner for JS/TS {{% cite StateofJS2021 %}}.
`dprint`'s selling point is speed; it can even incorporate `Prettier` as
a plugin with a 3X speed boost due to parallelism {{% cite Sherret2022
%}}.

Integrating `dprint` was a matter of (1) installing it as a dependency
and adding its config, (2) formatting the code base, and (3) enabling a
CI check and adding (2) to `.git-blame-ignore-revs` for a better
`git-blame` experience. {{% cite "issue158" %}} Unlike linters,
formatters do not change the semantics of your code, and so adding their
effects to `.git-blame-ignore-revs` is not error-prone.

## References

1. {{< citation
  id="CodeQL"
  title="CodeQL documentation"
  url="https://codeql.github.com/docs/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="CodeQLLicense"
  title="codeql-cli-binaries/LICENSE.md at main · github/codeql-cli-binaries"
  url="https://github.com/github/codeql-cli-binaries/blob/main/LICENSE.md"
  accessed="2024-06-09" >}}

1. {{< citation
  id="variantAnalysis"
  title="Variant analysis | Human genetic variation"
  url="https://www.ebi.ac.uk/training/online/courses/human-genetic-variation-introduction/variant-identification-and-analysis/variant-analysis/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="Prettier"
  title="Prettier · Opinionated Code Formatter"
  url="https://prettier.io/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="StateofJS2021"
  title="The State of JS 2021: Other Tools"
  url="https://2021.stateofjs.com/en-US/other-tools/#utilities"
  accessed="2024-06-09" >}}

1. {{< citation
  id="dprint"
  title="dprint - Code Formatter"
  url="https://dprint.dev/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="Sherret2022"
  author="David Sherret"
  title="Speeding up Prettier locally and on your CI with dprint"
  url="https://david.deno.dev/posts/faster-prettier-with-dprint/#final-notes"
  url_2="https://news.ycombinator.com/item?id=31160722"
  accessed="2024-06-09" >}}

1. {{< citation
  id="issue158"
  title="Consistent styling using dprint · Issue #158 · dchege711/study_buddy"
  url="https://github.com/dchege711/study_buddy/issues/158"
  accessed="2024-06-09" >}}
