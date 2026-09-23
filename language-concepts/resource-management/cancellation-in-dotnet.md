---
title: "Cancellation in .NET"
date: 2026-09-23
draft: true
---

## Overview

Cancellation is cooperative. One part of the code politely notifies the other
code that it'd like it to please stop. The responding code may immediately stop,
or continue until it reaches a valid stopping point, or ignore the cancellation
request entirely. {{% cite Cleary2022 %}}

Most code has the form:

```cs
async Task DoSomethingAsync(int data, CancellationToken cancellationToken)
{
  var intermediateValue = await DoFirstStepAsync(data, cancellationToken);
  await DoSecondStepAsync(intermediateValue, cancellationToken);
}
```

... where the `CancellationToken` is passed down to whatever APIs you call. {{%
cite Cleary2022 %}}

By convention, the `CancellationToken` is the last in the method signature:

```cs
async Task DoSomethingAsync(int data, CancellationToken cancellationToken = default)
{
  ...
}
```

... where the `default` `CancellationToken` is `CancellationToken.None`, i.e.,
a cancellation token that will never be canceled. {{% cite Cleary2022 %}}

Unless you're also p/Invoking APIs that take timeout parameters, taking a single
`CancellationToken` is sufficient to represent any kind of cancellation, e.g., a
user pressing a Cancel button, an application shutting down, a client
disconnecting from a server, a timeout, etc. {{% cite Cleary2022 %}}

The cancellation contract has canceled code throw `OperationCanceledException`
when the cancellation is observed and has actually canceled some work. If the
cancellation request arrives too late, then the method returns normally without
throwing `OperationCanceledException`. {{% cite Cleary2022 %}}

When using `Task.Run`, do not pass the `CancellationToken` to `Task.Run` because
that just cancels the scheduling of the delegate to the thread pool, and not the
delegate itself, i.e.,

```cs
async Task DoSomethingAsync(CancellationToken cancellationToken)
{
  var test = await Task.Run(() =>
  {
    // Do something, ignoring `cancellationToken`
  }, cancellationToken);
  ...
}
```

... Instead, use the `cancellationToken` inside the delegate. {{% cite
Cleary2022 %}}

## References

1. {{< citation
  id="Cleary2022"
  author="Stephen Cleary"
  date="2022-02-24"
  title="Cancellation, Part 1: Overview"
  url="https://blog.stephencleary.com/2022/02/cancellation-1-overview.html"
  accessed="2026-09-23" >}}

1. {{< citation
  id="Cleary2022-02"
  author="Stephen Cleary"
  date="2022-03-03"
  title="Cancellation, Part 2: Requesting Cancellation"
  url="https://blog.stephencleary.com/2022/03/cancellation-2-requesting-cancellation.html"
  accessed="2026-09-23" >}}

1. {{< citation
  id="Cleary2022-03"
  author="Stephen Cleary"
  date="2022-03-10"
  title="Cancellation, Part 3: Detecting Cancellation"
  url="https://blog.stephencleary.com/2022/03/cancellation-3-detecting-cancellation.html"
  accessed="2026-09-23" >}}

1. {{< citation
  id="Cleary2022-04"
  author="Stephen Cleary"
  date="2022-03-17"
  title="Cancellation, Part 4: Polling"
  url="https://blog.stephencleary.com/2022/03/cancellation-4-polling.html"
  accessed="2026-09-23" >}}

1. {{< citation
  id="Cleary2024"
  author="Stephen Cleary"
  date="2024-08-08"
  title="Cancellation, Part 5: Registration"
  url="https://blog.stephencleary.com/2024/08/cancellation-5-registration.html"
  accessed="2026-09-23" >}}

1. {{< citation
  id="Cleary2024-02"
  author="Stephen Cleary"
  date="2022-10-10"
  title="Cancellation, Part 6: Linking"
  url="https://blog.stephencleary.com/2024/10/cancellation-6-linking.html"
  accessed="2026-09-23" >}}
