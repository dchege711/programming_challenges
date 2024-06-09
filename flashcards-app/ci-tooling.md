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
