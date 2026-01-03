---
title: "C# Benchmarks Using BenchmarkDotNet"
date: 2026-01-02
---

Some work projects use `BenchmarkDotNet` as the .NET library for benchmarking.
Getting familiar with it should pay dividends. {{% cite BenchmarkDotNet %}}

To run the benchmarks in the `Day13ClawContraption` class:

```bash
dotnet run -c Release -- -f '*Day13ClawContraption*'
```

1. {{< citation
  id="BenchmarkDotNet"
  title="Home | BenchmarkDotNet"
  url="https://benchmarkdotnet.org/index.html"
  accessed="2026-01-02" >}}
