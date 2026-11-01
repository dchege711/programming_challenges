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
dotnet new solution --name Foo
dotnet solution Foo.slnx add Foo/Foo.csproj
dotnet solution Foo.slnx add Foo.Tests/Foo.Tests.csproj
```
