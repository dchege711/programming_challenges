---
title: "C# Performance Tools"
date: 2026-01-02
---

## BenchmarkDotNet

Some work projects use `BenchmarkDotNet` as the .NET library for benchmarking.
Getting familiar with it should pay dividends. {{% cite BenchmarkDotNet %}}

To run the benchmarks in the `Day13ClawContraption` class:

```bash
dotnet run -c Release -- -f '*Day13ClawContraption*'
```

A `job` describes how to run your benchmark, e.g, ID, environment, run settings.
`BenchmarkDotNet` has a smart algorithm for choosing values like
`IterationCount`, so you typically don't need to specify those. {{% cite
JobsBenchmarkDotNet %}} Sample measurements for the default job:

| Method | Mean | Error | StdDev |
| --- | --- | --- | --- |
| Foo | 3.845 s | 0.0747 s | 0.0800 s |

### Memory Diagnoser

The Common Language Runtime continually balances two priorities when it comes to
garbage collection (GC):

* Not letting an application's working set get too large by delaying GC
* Not letting the GC run too frequently (during GC, all other managed threads
  are suspended)

The managed heap is divided into 3 generations, 0, 1, and 2, so it can handle
long-lived and short-lived objects separately:

* <em>Generation 0</em>: The youngest and contains short-lived objects. New
  objects are stored here, unless they're large in which case they go to the
  large object heap (LOH / generation 3).
* <em>Generation 1</em>: After GC collects `Gen 0`, it compacts the memory for
  the reachable objects and promotes them to `Gen 1`. Objects that survive
  collections tend to have longer lifetimes, and so the promotion makes sense.
  If a GC of `Gen 0` doesn't reclaim enough memory, then the GC can collect `Gen
  1` and if need be, `Gen 2`, but in most cases, `Gen 0` collection is
  sufficient. Objects in `Gen 1` that survive GC are promoted to `Gen 2`.
* <em>Generation 2</em>: Contains long-lived objects, e.g., static data in a
  server application. Objects in `Gen 2` that survive GC remain in `Gen 2`.
  Objects on the large object heap are also collected in `Gen 2`.

{{% cite DotNetGCFundamentals %}}

`MemoryDiagnoser` allows measuring the number of allocated bytes and garbage
collection frequency, e.g.,

| Method | Gen0 | Gen1 | Gen2 | Allocated |
| --- | --- | --- | --- | --- |
| Foo | 596 | 193 | 48 | 3.49 MB |

* <em>Allocated</em> contains the size of allocated <em>managed</em> memory (not
  `stackalloc` or native heap allocations). It's per single invocation,
  <em>inclusive</em>.
* <em>Gen X</em> contains the number of `Gen X` collections scaled to per 1,000
  operations, e.g., GC collects memory 596 times per 1,000 benchmark invocations
  in generation 0.
* `-` and `0` are synonymous, e.g., `-` in `Gen X` means no garbage collection
  was performed for generation `X`.

{{% cite SitnikMemoryDiagnoser %}}

### Perf Profiles

`BenchmarkDotNet.Diagnostics.Windows` allows collecting ETL traces that show
where most of the time is spent. However, this package is only available on
Windows because it internally uses Event Tracing for Windows (ETW) to capture
stack traces and important .NET Runtime events. {{% cite SitnikEtwProfiler %}}

`BenchmarkDotNet.Diagnostics.dotTrace` can also capture traces using the
`dotTrace` command-line profiler. However, `dotTrace` is not available for free;
JetBrains charges for it. {{% cite BenchmarkDotNet.Diagnostics.dotTrace %}}

Trying to use `dotnet-trace` as it's free. For some reason,

```shell
dotnet-trace collect --format speedscope -- dotnet run -c Release
```

... doesn't give me much. `-- <command>` has a disclaimer for commands that
launch multiple apps, but I don't think it applies here. Running
`dotnet run -c Release -- -f '*Day13ClawContraption*'` in one terminal and then:

```shell
dotnet-trace collect --name "AoC2024.Benchmarks" --format speedscope --output advent-of-code/2024/AoC2024.Benchmarks/BenchmarkDotNet.Artifacts/perf-profile.nettrace
```

... in another seems like a step forward because `dotnet-trace` exits after the
benchmark exits. {{% cite dotnet-trace %}}

Still no dice though; seeing a lot `UNMANAGED_CODE_TIME` without any actionable
insights. Creating a separate program that benchmarks the code directly instead
of going through `BenchmarkDotNet` works! In hindsight, I think I was profiling
`BenchmarkSwitcher.FromAssembly(typeof(Program).Assembly).Run(args)` instead of
my user code. One more `.csproj` it is!

## References

1. {{< citation
  id="BenchmarkDotNet"
  title="Home | BenchmarkDotNet"
  url="https://benchmarkdotnet.org/index.html"
  accessed="2026-01-02" >}}

1. {{< citation
  id="JobsBenchmarkDotNet"
  title="Jobs | BenchmarkDotNet"
  url="https://benchmarkdotnet.org/articles/configs/jobs.html"
  accessed="2026-01-02" >}}

1. {{< citation
  id="SitnikMemoryDiagnoser"
  author="Adam Sitnik"
  title="The new MemoryDiagnoser is now better than ever! – Adam Sitnik – .NET Performance and Reliability"
  url="https://adamsitnik.com/the-new-Memory-Diagnoser/"
  accessed="2026-01-02" >}}

1. {{< citation
  id="DotNetGCFundamentals"
  title="Fundamentals of garbage collection - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/fundamentals"
  accessed="2026-01-02" >}}

1. {{< citation
  id="SitnikEtwProfiler"
  author="Adam Sitnik"
  title="Profiling .NET Code with BenchmarkDotNet – Adam Sitnik – .NET Performance and Reliability"
  url="https://adamsitnik.com/ETW-Profiler/"
  accessed="2026-01-02" >}}

1. {{< citation
  id="BenchmarkDotNet.Diagnostics.dotTrace"
  title="NuGet Gallery | BenchmarkDotNet.Diagnostics.dotTrace 0.15.8"
  url="https://www.nuget.org/packages/BenchmarkDotNet.Diagnostics.dotTrace#readme-body-tab"
  url_2="https://benchmarkdotnet.org/articles/samples/IntroDotTraceDiagnoser.html"
  accessed="2026-01-02" >}}

1. {{< citation
  id="dotnet-trace"
  title="dotnet-trace diagnostic tool - .NET CLI - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/core/diagnostics/dotnet-trace"
  accessed="2026-01-02" >}}