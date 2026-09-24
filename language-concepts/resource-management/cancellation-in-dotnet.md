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

## Requesting Cancellation

In most cases, the framework you're using provides the `CancellationToken`,
e.g., ASP.NET provides a `CancellationToken` that represents an unexpected
client disconnect. Use `CancellationTokenSource` when you need to provide your
own `CancellationToken` that can be cancelled later. {{% cite Cleary2022-02 %}}

Each `CancellationToken` created from a `CancellationTokenSource` is a small
`struct` that refers back to its `CancellationTokenSource`. A
`CancellationToken` can only respond to cancellation request. To request a
cancellation, keep a reference to the `CancellationTokenSource` and request
cancellations through it. {{% cite Cleary2022-02 %}}

For the common case of requesting cancellation after a timeout:

```cs
async Task DoSomethingWithTimeoutAsync()
{
  using CancellationTokenSource cts = new(TimeSpan.FromMinutes(5));
  await DoSomethingAsync(cts.Token);

  // At the end of this method, the CTS is disposed and its tokens should not
  // be used after this point.
}
```

... or call `CancelAfter` on an existing `CancellationTokenSource`. {{% cite
Cleary2022-02 %}}

Consider a GUI application with a "Cancel" button:

```cs
Constructor() => CancelButton.Enabled = false;

private CancellationTokenSource? _cts;

async void StartButton_Click(...)
{
  // Requirement: Either the Start or Cancel button can be enabled at any given time.
  StartButton.Enabled = false;
  CancelButton.Enabled = true;

  using var cts = _cts = new();

  try
  {
    await DoSomethingAsync(_cts.Token);
    ... // Display success in the UI.
  }
  catch (Exception ex)
  {
    ... // Display error in the UI.
  }
  finally
  {
    // Requirement: Start button remain disabled until operation completes
    // successfully, or with an Exception (including OperationCanceledException).
    StartButton.Enabled = true;
    CancelButton.Enabled = false;
  }
}

async void CancelButton_Click(...)
{
  if (_cts is not CancellationTokenSource cts)
    throw new IllegalOperationException("Cancel called without a prior operation");

  // Requirement: After cancellation, the Cancel button remains enabled but is a noop.
  cts.Cancel();
}
```

{{% cite leary2022-02 %}}

What if the user should be able to start a new operation as soon as the old
operation is cancelled, without waiting for the old operation to complete?

```cs
Constructor() => CancelButton.Enabled = false;

private CancellationTokenSource? _cts;

async void StartButton_Click(...)
{
  StartButton.Enabled = false;
  CancelButton.Enabled = true;

  using var cts = _cts = new();

  // Requirement: Only show updates when we're the current operation. Use
  // `cts == _cts` because `_cts` changes every time StartButton is clicked.
  try
  {
    await DoSomethingAsync(_cts.Token);
    if (cts == _cts)
    {
      ... // Display success in the UI.
    }
    catch (Exception ex)
    {
      if (cts == _cts)
      {
        ... // Display error in the UI.
      }
    }
    finally
    {
      StartButton.Enabled = true;
      CancelButton.Enabled = false;
    }
  }
}

async void CancelButton_Click(...)
{
  StartButton.Enabled = true; // NEW
  CancelButton.Enabled = false; // NEW

  if (_cts is not CancellationTokenSource cts)
    throw new IllegalOperationException("Cancel called without a prior operation");

  cts.Cancel();

  // Requirement: Cancelled operations do not update the UI with success/errors
  _cts = null; // NEW
}
```

{{% cite Cleary2022-02 %}}

Always clean up `CancellationTokenSource`'s resources (e.g., timeout timers,
attached listeners). This cleanup happens either on
`CancellationTokenSource.Dispose()` or on `CancellationTokenSource.Cancel()`.
Ensure at least one of the two happens in a `CancellationTokenSource`'s
lifetime. {{% cite Cleary2022-02 %}}

## Detecting Cancellation

By convention, methods that take `CancellationToken` throw
`OperationCanceledException` when they are cancelled. The typical response is:

```cs
async Task TryDoSomethingAsync()
{
  using CancellationTokenSource cts = new();
  ... // Wire up something that may cancel `cts`.

  try
  {
    await DoThingAsync(cts.Token);
  }
  catch (Exception ex) when (ex is not OperationCanceledException)
  {
    ... // Normal error handling; logging, etc.
  }
}
```

... because handling `OperationCanceledException`s is outside the norm. {{% cite
Cleary2022-03 %}}

While `OperationCanceledException` has a `CancellationToken` property, this may
not match the token from your `CancellationTokenSource`. If for some reason you
need to catch `OperationCanceledException`s, guard it with
`cts.IsCancellationRequested` and not `ex.CancellationToken == cts.Token`. {{%
cite Cleary2022-03 %}}

## Responding to Cancellation via Polling

```cs
void DoSomethingAsync(CancellationToken cancellationToken)
{
  while (!done)
  {
    cancellationToken.ThrowIfCancellationRequested();
    ... // Do work
  }
}

void DoSomethingAntiPatternAsync(CancellationToken cancellationToken)
{
  while (!cancellationToken.IsCancellationRequested)
  {
    ... // Do work
  }
  // Anti-pattern because we don't throw OperationCanceledException on
  // cancellation. Caller can't know if the operation ran to completion.
}
```

{{% cite Cleary2022-04 %}}

How often to call `ThrowIfCancellationRequested` is an art. For CPU-bound code,
it's a matter of testing what cancellation feels responsive enough. Another rule
of thumb is checking right before doing something expensive. {{% cite
Cleary2022-04 %}}

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
