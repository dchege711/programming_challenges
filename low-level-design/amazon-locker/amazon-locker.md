---
date: 2026-10-28
domains:
- chromium.googlesource.com
- cppreference.com
- en.wikipedia.org
- towardsdev.com
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/low-level-design/amazon-locker/amazon-locker/
title: 'Low Level Design: Amazon Locker'
---

Design a locker system like Amazon Locker where delivery drivers can deposit
packages and customers can pick them up using a code. {{% cite
AmazonLockerHelloInterview %}}

## Clarifying Questions

* Establish scope: *Are we modeling just the piece from when the driver arrives
  at the locker until the customer? How does the customer get the code?*
* Questions on core flows: *Are there different sized compartments? Can a small
  package go into a large compartment if the small ones are full? Are access
  tokens unique per package?*
* Edge cases: *What if someone enters the wrong code multiple times? What
  happens to a package if it's never picked up? What if all compartments of a
  given size are full when a driver tries to deposit?*

{{% cite AmazonLockerHelloInterview %}}

## Final Requirements

Requirements:

* Carrier deposits a package by specifying a size (small, medium, or large).
  System assigns an available compartment of matching size, opens compartment
  and returns access token. Error if no space.
* On successful deposit, an access token is generated and returned.
* User retrieves by entering access token. System validates code and opens
  compartment. Throw specific error if code is invalid or expired.
* Access tokens expire after 7 days. Customers can't use expired codes. Package
  remains in compartment until staff removes it.
* Staff can open all compartments with expired packages, remove the packages and
  return them to sender.
* Invalid access tokens are rejected with clear error messages, e.g., wrong
  code, already used, expired.

{{% cite AmazonLockerHelloInterview %}}

Out of scope: delivery logistics for how the package gets to the locker; how the
access token reaches the customer; lockout after failed access token attempts;
UI/rendering layer; multiple locker stations; ayment or pricing.

{{% cite AmazonLockerHelloInterview %}}

{{% comment %}}

Really gotta think like a PM here. My initial impression was that delivering the
access token to the consumer was fair game. Also assumed that we'd need an audit
trail for liability reasons. Assumed that difference in locker sizes was not in
scope, and neither was reclaiming space from expired pickups.

{{% /comment %}}

## Class Design

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/amazon_locker.h"
  highlight="cpp" >}}

{{% comment %}}

TIL that `#pragma once` is not universally supported, e.g., hard links,
networked file systems. Thus, the #include guard macro is still king. Modules,
introduced in C++20, eliminate the problem altogether because they are not
handled by the pre-processor, and they can only be import at most once into a
translation unit. {{% cite PragmaOnceWiki %}}

{{% /comment %}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/compartment.h"
  highlight="cpp" >}}

Physical state (contains a package, is broken, etc.) lives on the entity because
it describes the entity's condition. Relational state (assigned to this token,
reserved by this user) lives in the orchestrator. {{% cite
AmazonLockerHelloInterview %}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/access_token.h"
  highlight="cpp" >}}

## Implementation

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/amazon_locker.cc"
  highlight="cpp" >}}

{{% comment %}}

An LLM helped me generate `generate_access_code`, and there's stuff to unpack
w.r.t. to views and pipelines.

A `view` is a lazily evaluated `range` that wraps another `range` and applies a
transformation or filtering on demand, element by element, only as you iterate
-- without copying the original data, and with \\(\mathcal{O}(1)\\) move
construction and destruction regardless of the number of elements. {{% cite
Sagar2026 %}}. This contrasts with range algorithms, which are applied to ranges
eagerly. {{% cite RangesCppRef %}}

Range adaptors are mechanisms for creating `views`, e.g., `std::views::filter`,
`std::views::transform`, `std::views::reverse`, `std::views::drop`,
`std::views::keys`, `std::views::enumerate`, `std::views::zip`,
`std::views::cartesian_product`, `std::views::cache_latest`, etc. {{% cite
Sagar2026 %}} {{% cite RangesCppRef %}}

Range adaptors overload `operator|` so that `baz(bar(foo(vec), 3), 7)` becomes
`vec | foo | bar(3) | baz(7)`. The pipe form resembles unix pipelines, and reads
more naturally. {{% cite Sagar2026 %}}

Views eliminate intermediate allocations and copies as they operate in a pull
model. Only when the consumer asks for the next element does the request travel
backward through the pipeline. {{% cite Sagar2026 %}}

```cpp
auto numbers = std::views::iota(1) | std::views::take(5);
for (int n : numbers) {
  std::cout << n << " ";
}
// Output: 1 2 3 4 5
```

{{% /comment %}}

## Extensions

### Allow a Smaller Package to Use Larger Compartment as a Fallback

Only the `find_available_compartment` function needs to change.

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/amazon_locker_with_size_fallback.h"
  highlight="cpp" >}}

{{< detail-readFile
  file="/content/computer-science/low-level-design/exercises/amazon-locker/src/amazon_locker_with_size_fallback.cc"
  highlight="cpp" >}}

### Handle Compartments That Are Broken or Under Maintenance

The solution is to add more states in `Compartment::State`, i.e.,

```cpp
enum class OldState : std::uint8_t { kAvailable, kOccupied };

enum class State : std::uint8_t { kAvailable, kOccupied, kOutOfService };
```

... and appropriate methods like `Compartment::mark_in_service()`.

{{% comment %}}

The Chromium project advocates for enums over booleans when passing parameters,
e.g.,

```cpp
void my_function(bool enable_sandbox, bool record_stats);

enum class Sandbox { Disable, Enable };
enum class RecordStats { Disable, Enable };
void my_function_deluxe(Sandbox sandbox, RecordStats record_stats);
```

... as the latter is easier to read, and impossible to accidentally swap the
order of parameters. {{% cite EnumsOverBoolsChromium %}}

{{% /comment %}}

{{% comment %}}

Using enums over booleans is also extensible. This shines in places like
database schemas where using a `boolean` adds backward-compatibility tax when
the scenario evolves in the future and you need to represent 3 or more states.

{{% /comment %}}

### Ensure Packages Are Deposited Before Generating Access Tokens

{{% priors %}}

Currently, `AmazonLocker`'s `deposit_package` is one step. We'd need to break it
into two:

* `open_compartment`: Calls `find_available_compartment`, and opens the
  `Compartment`.
* `on_close_compartment`: Called after the compartment is closed. This is the
  part that generates the access token.

The caller of `on_close_compartment` would need to be smart about it, e.g.,
detecting that there's an item and the compartment has been closed.

{{% /priors %}}

{{% cite AmazonLockerHelloInterview %}} introduces a new
`Compartment::State::kReserved`. `AmazonLocker` maintains a mapping from
`reservationId` to `Compartment`s. `AmazonLocker::reserve_compartment` marks the
compartment as `kReserved`, generates the `reservationId`, opens the
compartment, and returns `reservationId`. `AmazonLocker::confirm_deposit`
retrieves a `Compartment` by `reservationId`, marks it `kOccupied`, and
generates an `AccessToken` for it.

{{% comment %}}

The initial `open_compartment` and `on_close_compartment` design was handwavy.
Ask yourself questions along the flow. If the staff member calls
`open_compartment` twice, what prevents the second `open_compartment` call from
returning the same compartment as the one from the first call? That should have
led you to the need for `Compartment::State::kReserved` and the additional
bookkeeping.

{{% /comment %}}

## References

1. {{< citation
  id="AmazonLockerHelloInterview"
  title="Amazon Locker Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/problem-breakdowns/amazon-locker"
  accessed="2026-10-28" >}}

1. {{< citation
  id="PragmaOnceWiki"
  title="pragma once - Wikipedia"
  url="https://en.wikipedia.org/wiki/Pragma_once"
  accessed="2026-10-30" >}}

1. {{< citation
  id="Sagar2026"
  date="2026-03-13"
  title="C++20 std::views — Master Modern Ranges and Pipelines | by Sagar | Towards Dev"
  url="https://towardsdev.com/cpp20-std-views-guide-ranges-pipelines-a95790b3cd8c"
  accessed="2026-10-30" >}}

1. {{< citation
  id="RangesCppRef"
  title="Ranges library (since C++20) - cppreference.com"
  url="https://cppreference.com/cpp/ranges"
  accessed="2026-10-30" >}}

1. {{< citation
  id="EnumsOverBoolsChromium"
  title="Chromium Docs - Prefer enums over bools for parameters"
  url="https://chromium.googlesource.com/chromium/src/+/HEAD/docs/patterns/prefer-enums.md"
  accessed="2026-10-31" >}}
