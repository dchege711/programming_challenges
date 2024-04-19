---
title: "Use of Local Storage"
date: 2024-04-18
---

Back when I wrote this, the motivation for using `localStorage` was to
reduce the trips to the server so that the app is usable offline.
However, with two data stores (`localStorage` and the server), the
former has a possibility of going stale. What usage is correct and how
can we avoid stale data?

## `localStorage.clear()`

On a user confirming that they wish to delete their account, we send a
`POST` message to `/account/delete-account`, and on any valid response,
clear `localStorage` and set `window.location.href = "/"`.

This usage of `localStorage` is not informative as there is no data
written there by the `/account` page.

## `localStorage['session_info']`

`getAccountInfo: () => AuthenticateUser | null` fetches the
`session_info` entry and `JSON.parse`s it into an `AuthenticateUser`.
This is a possible failure point because the parsed JSON cannot be
trusted to be a valid `AuthenticateUser` instance. This bit should be
removed.

There's no write location for `session_info` either, so this
`getAccountInfo()` returns `null`, except for users on a very old
version of the site where we'd write into `session_info`.

## `localStorage['metadata']`

Set after the `POST` message to `/read-metadata`. Contains a
JSON-serialization of `IMetadata[]`, which is usually a single item
array.

One write comes from `setupInitializationData`, which is called after
the `POST` message to `/login` succeeds. Another write comes from
`refreshMetadata()`, which is called from `initializeAccountPage` and
`initializeHomepage`.

There is no read of this entry. It can be safely removed.

## `localStorage['minicards]`

Looking at the read references first is more useful. There are no reads
in the app, and so there's no need to analyze write locations.

## `localStorage[cardId]`

`CardsManager.findCard` looks for the card in `localStorage` before
requesting one from the server. If the card ends up being requested from
the server, the method persists the server result in `localStorage` in
anticipation of the next query.

`/browse`'s `copyCardToOwnCollection` writes to `localStorage` once the
`/duplicate-card` `POST` message succeeds.

`CardsManager.updateCard` overwrites the old entry with the input
`card`. `CardsManager.findCard` writes to `localStorage` on fetching the
card from the server.

Stale card entries could come from the user interacting with the app on
a different browser/machine. The client app does not subscribe to
updates from the server. While `sessionStorage` is cleared after the
page session is done, `localStorage` has no expiry date. {{% cite
localStorageMDN %}} This makes the likelihood of the cards being stale
even more likely.

{{% comment %}}

At first, it seemed that making `CardsManager` a web component and
clearing the `localStorage` in the disconnected callback was a use-case
for web components, but turns out the platform already has
`sessionStorage` for that!

{{% /comment %}}

## References

1. {{< citation
  id="localStorageMDN"
  title="Window: localStorage property - Web APIs | MDN"
  url="https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage"
  accessed="2024-04-18" >}}
