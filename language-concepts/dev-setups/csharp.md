---
cited-authors:
- Reilly, John
date: 2026-11-01
domains:
- johnnyreilly.com
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

`dotnet add package FluentAssertions` in the tests project for more ergonomic
test assertion style.

## Formatting and Linting

```sh
dotnet new editorconfig --empty true
```

... and have `<EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>` in the
`.csproj`. {{% cite Reilly2022 %}}

My base setup has been:

```yaml
root = true

[*.cs]
csharp_style_namespace_declarations = file_scoped
csharp_style_expression_bodied_methods = true;
csharp_style_var_for_built_in_types = true
csharp_style_var_when_type_is_apparent = true
dotnet_style_prefer_conditional_expression_over_return = false

dotnet_analyzer_diagnostic.severity = error
```

... with an override in the tests project:

```yaml
[*.cs]

# CA1707: Identifiers should not contain underscores
# Allow test names to have underscores.
dotnet_diagnostic.CA1707.severity = none

# Remove unnecessary expression value (IDE0058)
# Allow Fluent assertions like: myCollection.Should().BeEqualTo([...])
dotnet_diagnostic.IDE0058.severity = none
```

Each `.csproj` has these in the `<PropertyGroup>`:

```xml
<EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
<GenerateDocumentationFile>true</GenerateDocumentationFile>
<NoWarn>$(NoWarn);1591</NoWarn>
```

... where:

* `EnforceCodeStyleInBuild` makes linter errors surface as build errors.
* `GenerateDocumentationFile` enables rules like "remove unnecessary usings".
* Suppressing `CS1591: Missing XML comment for publicly visible type or member`
  is worthwhile because we're not publishing a library.

## References

1. {{< citation
  id="Reilly2022"
  author="John Reilly"
  date="2022-04-06"
  title="ESLint your C# in VS Code with Roslyn Analyzers | johnnyreilly"
  url="https://johnnyreilly.com/eslint-your-csharp-in-vs-code-with-roslyn-analyzers"
  accessed="2026-11-01" >}}
