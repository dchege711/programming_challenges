---
date: 2026-11-01
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/dev-setups/csharp/
title: Dev Environment for C#
---

See `dotnet new list` for recipes.

## Write Code and Exercise It Using Tests

```sh
dotnet new classlib --name Foo
dotnet new mstest --name Foo.Tests
dotnet reference add Foo/Foo.csproj --project Foo.Tests/Foo.Tests.csproj
dotnet new solution --name Foo
dotnet solution Foo.slnx add Foo/Foo.csproj
dotnet solution Foo.slnx add Foo.Tests/Foo.Tests.csproj
```

## Formatting and Linting

```sh
dotnet new editorconfig --empty true
```

... and have `<EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>` in the
`.csproj`. {{% cite Reilly2022 %}}

## References

1. {{< citation
  id="Reilly2022"
  author="John Reilly"
  date="2022-04-06"
  title="ESLint your C# in VS Code with Roslyn Analyzers | johnnyreilly"
  url="https://johnnyreilly.com/eslint-your-csharp-in-vs-code-with-roslyn-analyzers"
  accessed="2026-11-01" >}}
