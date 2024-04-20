---
title: Client/Server Interface
date: 2024-04-19
---

## How to handle redirects without setting `window.location.href`?

Right now, there's a pattern of doing:

```ts
sendHTTPRequest("POST", "/login/", {})
  .then((_) => {
      window.location.href = "/";
  })
  .catch((err) => { console.error(err); });
```

Isn't this something that the server can do? In response, why not issue
a redirect?

{{< figure
  src="/img/computer-science/programming-challenges/flashcards/client-server-interface/unfollowed-redirect.jpg"
  caption=`Screenshot of the redirect chain from /login. The POST
  request gets a 303 (See Other) redirect to /home. The browser then
  makes a GET request to /home, which results in a 304 (Not Modified).
  Why doesn't the browser navigate me to /home?`
  >}}

{{% comment %}}

The `304 Not Modified` response is sent when there is no need to
retransmit the requested resources. {{% cite status304MDN %}} In this
case, the page at `/home` has not changed, and the browser can use the
cached version. If the content had changed, then the browser would have
requested the resource from the server, resulting in a 200 response.

{{% /comment %}}

{{% comment %}}

Installing
[`http-status-codes`](https://www.npmjs.com/package/http-status-codes)
for more legible status codes.

{{% /comment %}}

This issue does not happen when I issue a `GET` request to `/logout` by
clicking on a `<a href="/logout">Log Out</a>`. The server responds with
a `303 (See Other)` to `/browse`, and the browser loads `/browse` in the
tab. If I do the same with `fetch('/logout', { method: 'GET' })`
instead, then the browser does not navigate the page to `/browse`
despite the `/browse` response being observable in the Network tab. The
consensus online is that `fetch` responses do not lead to automatic
browser navigation.

{{% comment %}}

Re: `fetch` vs. `XMLHttpRequest`. The latter is the older way of making
HTTP requests from JavaScript without leaving the page; it is based on
events. `fetch` is built around promises, which are considered the
preferred way of doing asynchronous operations. `fetch` and
`XMLHttpRequest` are implemented by browser vendors. {{% cite fetchXHRSO
%}} On top of these, numerous libraries exist, e.g., `Fetch polyfill`,
`isomorphic-fetch`, `axios`, `jQuery`, etc., to fill various gaps, e.g.,
different syntax, varying browser support, etc. {{% cite Andrew2016 %}}

{{% /comment %}}

## Typing the Client/Server Interface

One source of bugs is the server and the client being out of sync w.r.t.
the shape of the data being exchanged. Granted, I have both the server
and the client in the same repo, how can I avoid mismatches in data? [Is
there a library or a standard way of creating an interface for my api
endpoints? :
typescript](https://www.reddit.com/r/typescript/comments/yryz83/is_there_a_library_or_a_standard_way_of_creating/)
floats `tRPC`, `GraphQL`, `OpenAPI`, and some others, with `tRPC` coming
in first and `GraphQL` as the incumbent.

GitHub Copilot can help out here with quick intros on what `tRPC` and
`GraphQL` entail. With `GraphQL`, the client can strongly type the API,
define the data payloads, fetch all the data in one request, query for
supported types, and subscribe to real-time updates. `GraphQL` is
intended to be an improvement over `REST`. `tRPC` is designed with
TypeScript in mind. Advantages of `tRPC` include no schema syncing as
the input/output is inferred directly from function signatures, and can
work with GraphQL (or any other data fetching method). In the case of my
app, using `tRPC` involves replacing `Express` routes with `tRPC`
procedures. Let's try `tRPC`!

{{% comment %}}

Impressed by Copilot. Definitely saved me time on this one.

{{% /comment %}}

## References

1. {{< citation
  id="status304MDN"
  title="304 Not Modified - HTTP | MDN"
  url="https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/304"
  accessed="2024-04-19" >}}

1. {{< citation
  id="fetchXHRSO"
  title="javascript - Difference between fetch, ajax, and xhr - Stack Overflow"
  url="https://stackoverflow.com/a/52261205r"
  accessed="2024-04-19" >}}

1. {{< citation
  id="Andrew2016"
  title="AJAX/HTTP Library Comparison"
  date="2016-02-03"
  url="https://www.javascriptstuff.com/ajax-libraries/"
  accessed="2024-04-19" >}}
