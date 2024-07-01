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
procedures. Trying `tRPC`!

{{% comment %}}

Impressed by Copilot. Definitely saved me time on this one.

{{% /comment %}}

What's the use case for `tRPC` for endpoints that return a static page
in response to a `GET` request? Copilot says that serving HTML files
should be left to traditional web frameworks like `Express` as those
have built-in support for SSR of HTML.

[Adding tRPC endpoints that don't need
authentication](https://github.com/dchege711/study_buddy/compare/f662e388435764043fa2c23619353c59b23d8dc7..cc2f374acd6f2f9a11c72f61927b6b989964fc25)
was mostly mechanical. However, there were a few gotchas. Some
methods/props on a `Mongoose` document (e.g., the `Document` interface)
cannot be shared between the server and the client, and so we need to
define a safe interface that can be inferred by `tRPC` and availed on
the client side. {{% cite trpcUnknownTypeMongo %}} We also encountered a
challenge in typing the inputs, given that the app relies on `Mongoose`
validation that occurs after `tRPC` has seen the inputs. Ended up adding
a passthrough input parser, e.g.,

```ts
export const authRouter = router({
  registerUser: publicProcedure
    .input((params: unknown) => params as RegisterUserAndPasswordParams)
    .mutation(({ input }) => {
      return registerUserAndPassword(input);
    }),
});
```

{{% cite trpcTypeInputWOValidation %}} The docs are not explicit about
this because not validating is considered unsafe, and the `tRPC`
maintainers don't want to advocate for it. With the base knowledge from
non-auth endpoints and reading the docs several times, [adding
auth-dependent
endpoints](https://github.com/dchege711/study_buddy/compare/cc2f374...ea80ac5?diff=split&w=)
integrated nicely with `Express`'s session management.

## Augmenting Server Objects Client Side

For example, the server transmits JSON data of the `ICardRaw` shape. Is
it possible to add helper methods in the client for ease of use? {{%
cite declMerging %}} suggests that we can do:

```ts
// server-side-card.ts
export class Card {}

// client-side-card.ts
import { Card } from '../../server-side-card.js';
declare module '../../server-side-card.js' {
  interface Card {
    get formattedTags(): string[];
  }
}

Card.prototype.formattedTags = function() {
  return tags.split(' ').filter(Boolean).map(t => `#${t}`);
}

// consumer.ts
import { Card } from '../../server-side-card.js';
import 'client-side-card.js';

let c: Card;
console.log(c.formattedTags);
```

However, we have `type PrivateCardResult = RouterOutput['fetchCard'][0]`
in `trpc.ts`. While we can extend the `type` {{% cite extendTypesSO %}},
it doesn't seem like we can add dynamic properties of the form:

```ts
type Foo = { s: string };
interface FooDeluxe extends Foo {
  emphasized: () => string;
}
// 'FooDeluxe' only refers to a type, but is being used as a value here.ts(2693)
FooDeluxe.prototype.emphasized = function() {
  return this.s.toUpperCase();
}
```

Foregoing `tRPC`'s typed procedures is not worth it.

## Validating Inputs

We use `zod` (vouched for by `tRPC`) to parse/validate the input on the
server. {{% cite zod %}} {{% cite pr182 %}} The "parse, don't validate"
mantra resonates as something to apply to programming projects: instead
of validating input, parse it such that incorrect state is impossible to
represent. {{% cite King2019 %}}

`zod` and `tRPC` work especially well:

```ts
const fetchCardParamsValidator = z.object({
  cardID: z.string().refine(isMongoId, { message: "Invalid card ID" }),
});

const inAppRouter = router({
  fetchCard: authedProcedure
    .input(fetchCardParamsValidator)
    .query(({ input, ctx }) => {
      return CardsDB.read({ ...input, userIDInApp: ctx.user.userIDInApp });
    }),
});
```

Executing `fetchCard({cardID: { $ne: "000000000000000000000000" }})`
leads to a `TRPCError` caused by a `ZodError`. At the input stage, we
did not just validate that `input` is of the correct shape, we discarded
`input`s that were not of the correct shape. `CardsDB.read` no longer
needs to worry that `cardID` could be corrupted.

Typical Express middleware {{% cite expressMiddleware %}} falls short of
the "parse, don't validate" approach because the type information is not
inferrable from the `request` object. For this, we fallback to assuming
that the validation middleware was applied, and it's safe to trust
`request.body`.

## Cross-Site Request Forgery (CSRF) Prevention

In a CSRF attack, the attacker wants the victim to perform
state-changing requests unknowingly. Suppose Alice is logged into
`bank.com` in her browser. To send money to Bob, the request is of the
form `GET http://bank.com/transfer.do?acct=BOB&amount=100 HTTP/1.1`.
Maria, the attacker, send Alice an email with an invisible picture:
`<img src="http://bank.com/transfer.do?acct=MARIA&amount=100000"
width="0" height="0" border="0">`. When Alice opens the email, the
browser will try loading the image, thereby submitting the transfer
request to `bank.com`, without Alice's knowledge. {{% cite csrfOWASP %}}

Token-based mitigation involves the server setting a CSRF token, which
the client then echoes back. The server rejects the request if the
echoed token is not valid. Because CSRF tokens are ephemeral, the chance
that the token will still be valid when an attacker attempts a CSRF
attack is reduced. Session-based tokens offer a good compromise between
security and usability. Request-based tokens have usability concerns,
e.g., breaking when the user uses the back button. {{% cite
csrfPreventionOWASP %}}

The `lusca` package provides CSRF protection for Express apps. {{% cite
codeQLMissingTokenValidation %}} {{% cite lusca %}} `lusca` docs were
lacking as they showed how to configure CSRF protection on the server
but not on the client, leaving me with `403` errors to handle.

One technique is to use `<form>` tags:

```html
<form action="/transfer.do" method="post">
<input type="hidden" name="_csrf" value="OWY4NmQwODE4ODRjN2Q2NTlhMmZlYWEwYzU1YWQwMTVhM2JmNGYxYjJiMGI4MjJjZDE1ZDZMGYwMGEwOA==">
[...]
</form>
```

{{% cite csrfPreventionOWASP %}}. This approach works well for forms
rendered through `response.render`.

What about for `tRPC` endpoints? A breakpoint in
`node_modules/lusca/lib/csrf.js` shows that the CSRF token is looked for
in either `req.body["_csrf"]` or `req.headers["x-csrf-token"]`. `tRPC`
sends payloads of the form:

```json
{
  "0": {
    "_id": "6682027e7c0aca290ed4ab4c",
    "title": "Modified title",
  }
}
```

... where `0` is a `tRPC` internal; the client code for the above call
is something like `trpc.updateCard.mutate({_id, title})`. `tRPC`
supports computing custom headers per request {{% cite headerstRPC %}}.
By setting the CSRF token in a `<meta>` element, and then reading that
before issuing the `tRPC` call, we're able to have `tRPC` calls succeed.

What about testing? The client-side tests [uses a dev server]({{< ref
"/computer-science/programming-challenges/flashcards-app/testing#e2e-testing"
>}}), which doesn't come with CSRF enforcement. The server-side uses an
actual server instance, and it seems that each state-mutating test would
need to pass a CSRF token. Maybe we can have a few tests that
specifically check CSRF protections, and then disable CSRF for the rest
of the tests?

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

1. {{< citation
  id="trpcUnknownTypeMongo"
  title="[Next.JS] x.data.user is of type 'unknown' · trpc/trpc · Discussion #3661"
  url="https://github.com/trpc/trpc/discussions/3661"
  accessed="2024-04-20" >}}

1. {{< citation
  id="trpcTypeInputWOValidation"
  title="docs: Howto add typed input (without validation) · Issue #3339 · trpc/trpc"
  url="https://github.com/trpc/trpc/issues/3339"
  accessed="2024-04-20" >}}

1. {{< citation
  id="declMerging"
  title="TypeScript: Documentation - Declaration Merging > Module Augmentation"
  url="https://www.typescriptlang.org/docs/handbook/declaration-merging.html#module-augmentation"
  accessed="2024-04-30" >}}

1. {{< citation
  id="extendTypesSO"
  title="javascript - Possible to extend types in Typescript? - Stack Overflow"
  url="https://stackoverflow.com/a/41385149"
  accessed="2024-05-03" >}}

1. {{< citation
  id="zod"
  title="Zod | Documentation"
  url="https://zod.dev/"
  accessed="2024-06-30" >}}

1. {{< citation
  id="pr182"
  title="[Major] Add server-side input validation by dchege711 · Pull Request #182 · dchege711/study_buddy"
  url="https://github.com/dchege711/study_buddy/pull/182"
  accessed="2024-06-30" >}}

1. {{< citation
  id="King2019"
  title="Parse, don't validate"
  author="Alexis King"
  url="https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/"
  date="2019-11-05"
  accessed="2024-06-30" >}}

1. {{< citation
  id="expressMiddleware"
  title="Using Express middleware"
  url="https://expressjs.com/en/guide/using-middleware.html"
  accessed="2024-06-30" >}}

1. {{< citation
  id="csrfOWASP"
  title="Cross Site Request Forgery (CSRF) | OWASP Foundation"
  url="https://owasp.org/www-community/attacks/csrf"
  accessed="2024-06-30" >}}

1. {{< citation
  id="codeQLMissingTokenValidation"
  title="Missing CSRF middleware — CodeQL query help documentation"
  url="https://codeql.github.com/codeql-query-help/javascript/js-missing-token-validation/"
  accessed="2024-06-30" >}}

1. {{< citation
  id="lusca"
  title="krakenjs/lusca: Application security for express apps."
  url="https://github.com/krakenjs/lusca#readme"
  accessed="2024-06-30" >}}

1. {{< citation
  id="csrfPreventionOWASP"
  title="Cross-Site Request Forgery Prevention - OWASP Cheat Sheet Series"
  url="https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html"
  accessed="2024-06-30" >}}

1. {{< citation
  id="headerstRPC"
  title="Custom header | tRPC"
  url="https://trpc.io/docs/client/headers"
  accessed="2024-06-30" >}}
