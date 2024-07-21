---
cited-authors:
- Sherret, David
date: 2024-06-09
domains:
- bestofjs.org
- biomejs.dev
- codeql.github.com
- david.deno.dev
- dprint.dev
- eslint.org
- github.com
- news.ycombinator.com
- prettier.io
- standardjs.com
- survey.stackoverflow.co
- typescript-eslint.io
- www.ebi.ac.uk
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/ci-tooling/
title: Continuous Integration Tooling
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
Prettier %}} is the front-runner for JS/TS {{% cite bestOfJSFormatters
%}}. `dprint`'s selling point is speed; it can even incorporate
`Prettier` as a plugin with a 3X speed boost due to parallelism {{% cite
Sherret2022 %}}.

{{% comment %}}

Interesting that "built in Rust" is becoming more of a selling point. Is
Rust the new hacker's language?

StackOverflow's 2023 Developer Survey has an "Admired and Desired"
section for technologies. "Desired" captures the % of respondents that
want to use a technology. "Admired" captures the % of users that have
used the same technology in 2023, and want to keep using it. The top 5
admired languages of 2023 were Rust (84.66%), Elixir (73.13%),
TypeScript (71.7%), Zig (71.33%), and Clojure (68.51%). {{% cite
SOSurvey2023 %}}

{{% /comment %}}

Integrating `dprint` was a matter of (1) installing it as a dependency
and adding its config, (2) formatting the code base, and (3) enabling a
CI check and adding (2) to `.git-blame-ignore-revs` for a better
`git-blame` experience. {{% cite "issue158" %}} Unlike linters,
formatters do not change the semantics of your code, and so adding their
effects to `.git-blame-ignore-revs` is not error-prone.

## Linter

A linter enforces statically-defined rules that discourage bug-prone
constructs. {{% cite issue162 %}} captures adding a linter to the
project.

{{% cite bestOfJSLinters %}} floats {{% cite StandardJS %}}, {{% cite
ESLint %}} and {{% cite RomeTools %}} as the top 3 in terms of Github
stars. {{% cite RomeTools %}} was pretty ambitious, but the company
failed, and {{% cite Biome %}} became its successor. {{% cite
typescript-eslint %}} enables {{% cite ESLint %}} and {{% cite Prettier
%}} for TypeScript. In terms of downloads over the last month, {{% cite
ESLint %}} and {{% cite typescript-eslint %}} dominate. Will try {{%
cite typescript-eslint %}}, using its popularity as a proxy for it being
worth a shot.

{{% comment %}}

Evaluating various JS/TS technologies seems tedious for a first-time
user. The above reasoning should have made me pick {{% cite Prettier %}}
over {{% cite dprint %}} were it not for my prior experience with it,
and {{% cite Sherret2022 %}}.

{{% /comment %}}

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
  id="bestOfJSFormatters"
  title="Best of JS • Formatter projects"
  url="https://bestofjs.org/projects?tags=formatter&sort=total"
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

1. {{< citation
  id="issue162"
  title="[ES] Add linter to catch common TS errors · Issue #162 · dchege711/study_buddy"
  url="https://github.com/dchege711/study_buddy/issues/162"
  accessed="2024-06-09" >}}

1. {{< citation
  id="bestOfJSLinters"
  title="Best of JS • Linter projects"
  url="https://bestofjs.org/projects?tags=lint&sort=total"
  url_2="https://bestofjs.org/projects?tags=lint&sort=monthly-downloads"
  accessed="2024-06-09" >}}

1. {{< citation
  id="SOSurvey2023"
  title="Stack Overflow Developer Survey 2023"
  url="https://survey.stackoverflow.co/2023/#section-admired-and-desired-programming-scripting-and-markup-languages"
  accessed="2024-06-09" >}}

1. {{< citation
  id="StandardJS"
  title="JavaScript Standard Style"
  url="https://standardjs.com/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="ESLint"
  title="Find and fix problems in your JavaScript code - ESLint - Pluggable JavaScript Linter"
  url="https://eslint.org/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="RomeTools"
  title="rome/tools: Unified developer tools for JavaScript, TypeScript, and the web"
  url="https://github.com/rome/tools"
  accessed="2024-06-09" >}}

1. {{< citation
  id="Biome"
  title="Announcing Biome | Biome"
  url="https://biomejs.dev/blog/annoucing-biome/"
  accessed="2024-06-09" >}}

1. {{< citation
  id="typescript-eslint"
  title="typescript-eslint"
  url="https://typescript-eslint.io/"
  accessed="2024-06-09" >}}
