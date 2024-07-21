---
cited-authors:
- Arguelles, Carlos
- Bender, Adam
- Ivanković, Marko
- Petrović, Goran
date: 2024-06-22
domains:
- dl.acm.org
- docs.npmjs.com
- git-scm.com
- github.com
- lit.dev
- modern-web.dev
- nodejs.org
- open-wc.org
- stackoverflow.com
- stryker-mutator.io
- testing.googleblog.com
- webdriver.io
- www.npmjs.com
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/testing/
title: Testing in a Monorepo
---

## Testing Web Components

While any test framework can work, it's better to test web components in
a browser environment because that's where they'll be used. Node-based
frameworks would require too much shimming of DOM calls that'd make the
tests unrepresentative. {{% cite WebTestRunner %}} and {{% cite
WebDriverIO %}} are good options for browser-based testing. {{% cite
testingLit %}}

{{% cite WebTestRunner %}} is powered by ES-build, and so is the
client-side of the app; let's go down this path and see where it leads.
CommonJS modules don't work in the browser {{% cite esModulesWebDev %}}.
[Earlier on]({{< ref
"/computer-science/programming-challenges/flashcards-app/of-builds-and-bundlers#es-modules-vs-commonjs"
>}}), using ESM on the server turned out difficult. Hopefully, that
doesn't me bite back now.

{{% comment %}}

`npm i --save-dev @web/test-runner` added 186 packages! Guess that's on
par for being able to run tests in a real browser. Its [deps
page](https://www.npmjs.com/package/@web/test-runner?activeTab=dependencies)
lists 16, but seems like when we install a package, grand-dependencies
blow up the count from 16 to 186?

{{% /comment %}}

## ESM vs. CommonJS Modules

`web-test-runner` failed with `SyntaxError: The requested module
'./index.js' does not provide an export named 'default'`. Debugging the
test in the browser via `web-test-runner --watch` shows that the error
is in `/node_modules/chai/index.mjs`'s `import chai from './index.js';`
statement. This is a more actionable error message; it's the result of
using a CommonJS module, which is not supported by `web-test-runner` {{%
cite testRunner1439 %}}. `npm install --save-dev
chai@npm:@esm-bundle/chai` gets me the ESM version of `chai`, and the
`web-test-runner` tests now pass.

Somewhere along the path, `ts-mocha` broke with `TypeError: Unknown file
extension ".ts"`. Trying out `git bisect` {{% cite gitBisect %}}. The
recipe is basically:

```zsh
$ rm -rf node_modules && rm -rf dist && rm ./tsconfig.tsbuildinfo
$ npm install && npm run build && npm run test:server
```

The server-side tests broke after `npm install --save
chai@npm:@esm-bundle/chai`. {{% cite SO71378840 %}} notes that this
Mocha/TS-node error occurs whenever any of your dependencies are ES6
modules, but your TypeScript target is not. Ah, the
`chai@npm:@esm-bundle/chai` syntax given by `@esm-bundle/chai`'s NPM
page installs it as an alias to `chai` and that's why the server-side
tests error out {{% cite NPMInstall %}}. In this case, `npm install
--save-dev @esm-bundle/chai` is sufficient as we can use `chai` in the
server tests, and `@esm-bundle/chai` in the client-side tests.

Even better, `@open-wc/testing` (which we'm using to test web components)
exposes `chai` as an ESM, and comes with useful plugins:

* `@open-wc/semantic-dom-diff` for dom tree / snapshot testing
* `@open-wc/chai-a11y-axe` for a11y testing
* `chai-dom` for dom testing

No need to install `@esm-bundle/chai` separately. {{% cite openWCTesting
%}}

## E2E Testing

How can we test an E2E scenario like being able to edit a card?
`web-test-runner` spins up an actual browser. Invoking UI that
communicates with the server does result in an HTTP request.

`web-test-runner.config.mjs` supports mocking out resources through
`@web/dev-server-import-maps`, e.g., swapping out `/src/my-module.js`
for a `/mocks/my-module.js` which avoids any interaction with the
server. {{% cite testRunnerMocking %}}

However, we have a mono-repo and can run the actual server, albeit with
a test database, for the tests. {{% cite start-server-and-test %}} is
the top search hit for `run local server in npm test`, and its
description matches what we want: starts server, waits for URL, then
runs test command; when the tests end, shuts down server.

Suppose the server starts and is listening on `http://localhost:5000`.
`web-test-runner` will also start up a dev server at port `8000` by
default. However, this doesn't match the server that is using `:5000`.
If we specify `--port 5000` to `web-test-runner`, then it errors out
with `EADDRINUSE`. Can we avoid spinning up a dev server, and instead
use the server from {{% cite start-server-and-test %}}?

`web-test-runner` supports a `server: Server` entry {{% cite
testRunnerConfig %}}. Presumably, `Server` is the type that comes from
Node {{% cite nodeHTTPServer %}}, and this should be compatible with the
`Server` returned by Express's `listen` method {{% cite
express-serve-static-core %}}. Can I point `server` to the result from
`app.listen()` where `app` is imported from `src/server.ts`? Nope, the
imports do not work out between the client-side ESM and server-side
CommonJS.

If `web-test-runner` will spin up a dev server, then maybe we can use
the `middleware: Middleware[]` config to rewrite requests from `:8000`
to `:5000`? {{% cite testRunnerConfig %}} {{% cite devServerMiddleware
%}} Even after rewriting a URL like `/trpc/searchPublicCards` to
`http://localhost:5000/trpc/searchPublicCards`, the request still 404s
out. Navigating to `http://localhost:5000/trpc/searchPublicCards` in the
browser brought up by `web-test-runner`'s `--watch` mode succeeds
though. This might be because in a direct navigation the request header
has `localhost:5000` as the `host`, but in the test, the host is
`localhost:4999`; we're veering into CORS territory. Changing the
configuration of the app's server for the sake of testing seems like an
overkill. Not to mention, I'd still need a more guaranteed way of
obtaining the app server's URL.

Back to {{% cite testRunnerMocking %}}, how much of `src/trpc.ts` do we
need to mock? Not much to get benefits; a piecemeal approach works.
Mocking `trpc.searchPublicCards.query` is enough to get me testing the
`search-bar` component extensively.

Can we also add snapshot testing for simple pixel-by-pixel comparisons?
`@web/test-runner` with `mocha` supports snapshot testing via the
`@open-wc/semantic-dom-diff` package {{% cite semantic-dom-diff %}}.
`@web/test-runner-commands` supports commands like `saveSnapshot` and
`compareSnapshot`. {{% cite test-runner-commands %}}

Why is that `await expect(searchBar).shadowDom.to.equalSnapshot();`
fails with `Failed to fetch dynamically imported module`? Navigating to
the link in question shows a file of the form:

```js
/* @web/test-runner snapshot v1 */
export const snapshots = {};

/* 0.6734464157701581 */
```

The browser has the error message:

> Failed to load module script: Expected a JavaScript module script but
the server responded with a MIME type of "text/plain". Strict MIME type
checking is enforced for module scripts per HTML spec.

Why would the dev server mark the snapshots module as `text/plain`?
There is an open GitHub thread on others encountering this issue. {{%
cite modernWeb2127 %}} Posted a question in the Discord channel linked
from [modern-web.dev](https://modern-web.dev/discover/slack/).

## Coverage

Code coverage is a lossy metric and should not be treated as the only
source of truth. Prioritizing coverage tends to bake in testability when
writing code. Code coverage only asserts that lines have been executed
by a test, and not whether they've been tested. A lot of the value of
code coverage is to highlight what's not covered; deliberating on the
parts not covered is more valuable than over-indexing on some threshold
for code coverage. {{% cite Arguelles2020 %}}

## Mutation Testing

Mutation testing offers stronger guarantees than statement coverage. It
involves inserting small faults into programs and measuring the ability
of the test suite to catch them. {{% cite Petrovic2018 %}}

`StrykerJS` seems like the dominant mutation testing package for TS
projects. For example, given this code:

```ts
function isUserOldEnough(user: User): boolean {
  return user.age >= 18;
}
```

... the `BinaryOperator` and `RemoveConditionals` mutators generate
mutants like `return user.age > 18`, `return user.age < 18`, `return
false`, `return true`. Stryker then runs the tests for each mutation,
expecting that at least one test will fail. {{% cite Stryker %}}

## References

1. {{< citation
  id="testingLit"
  title="Testing – Lit"
  url="https://lit.dev/docs/tools/testing/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="WebTestRunner"
  title="Web Test Runner: Modern Web"
  url="https://modern-web.dev/docs/test-runner/overview/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="WebDriverIO"
  title="WebdriverIO · Next-gen browser and mobile automation test framework for Node.js | WebdriverIO"
  url="https://webdriver.io/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="esModulesWebDev"
  title="Going Buildless: ES Modules: Modern Web"
  url="https://modern-web.dev/guides/going-buildless/es-modules/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="testRunner1439"
  title="[@web/test-runner] cannot import all npm packages · Issue #1439 · modernweb-dev/web"
  url="https://github.com/modernweb-dev/web/issues/1439"
  accessed="2024-06-22" >}}

1. {{< citation
  id="gitBisect"
  title="Git - git-bisect Documentation"
  url="https://git-scm.com/docs/git-bisect"
  accessed="2024-06-22" >}}

1. {{< citation
  id="SO71378840"
  title="typescript - ts-node and mocha 'TypeError [ERR_UNKNOWN_FILE_EXTENSION]: Unknown file extension '.ts' error even with 'ts-node/esm' loader and CommonJS modules"
  url="https://stackoverflow.com/questions/71378840/ts-node-and-mocha-typeerror-err-unknown-file-extension-unknown-file-extensio"
  url_2="https://stackoverflow.com/a/76524534"
  accessed="2024-06-22" >}}

1. {{< citation
  id="NPMInstall"
  title="npm-install | npm Docs"
  url="https://docs.npmjs.com/cli/v8/commands/npm-install"
  accessed="2024-06-22" >}}

1. {{< citation
  id="openWCTesting"
  title="Testing: Testing Package: Open Web Components"
  url="https://open-wc.org/docs/testing/testing-package/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="testRunnerMocking"
  title="Writing Tests: Mocking: Modern Web"
  url="https://modern-web.dev/docs/test-runner/writing-tests/mocking/"
  accessed="2024-06-22" >}}

1. {{< citation
  id="start-server-and-test"
  title="start-server-and-test - npm"
  url="https://www.npmjs.com/package/start-server-and-test"
  accessed="2024-06-22" >}}

1. {{< citation
  id="testRunnerConfig"
  title="Test Runner: CLI and Configuration: Modern Web"
  url="https://modern-web.dev/docs/test-runner/cli-and-configuration/"
  accessed="2024-06-23" >}}

1. {{< citation
  id="nodeHTTPServer"
  title="HTTP | Node.js v20.15.0 Documentation"
  url="https://nodejs.org/docs/v20.15.0/api/http.html#class-httpserver"
  accessed="2024-06-23" >}}

1. {{< citation
  id="express-serve-static-core"
  title="@types/express-serve-static-core - npm"
  url="https://www.npmjs.com/package/@types/express-serve-static-core?activeTab=code"
  accessed="2024-06-23" >}}

1. {{< citation
  id="devServerMiddleware"
  title="Dev Server: Middleware: Modern Web"
  url="https://modern-web.dev/docs/dev-server/middleware/#rewriting-request-urls"
  accessed="2024-06-23" >}}

1. {{< citation
  id="semantic-dom-diff"
  title="Testing: Semantic Dom Diff: Open Web Components"
  url="https://open-wc.org/docs/testing/semantic-dom-diff/"
  accessed="2024-06-23" >}}

1. {{< citation
  id="test-runner-commands"
  title="Test Runner: Commands: Modern Web"
  url="https://modern-web.dev/docs/test-runner/commands/"
  accessed="2024-06-23" >}}

1. {{< citation
  id="modernWeb2127"
  title="Snapshot testing is not working · Issue #2127 · modernweb-dev/web"
  url="https://github.com/modernweb-dev/web/issues/2127"
  accessed="2024-06-23" >}}

1. {{< citation
  id="Arguelles2020"
  date="2020-08-07"
  authors="Carlos Arguelles; Marko Ivanković; Adam Bender"
  title="Google Testing Blog: Code Coverage Best Practices"
  url="https://testing.googleblog.com/2020/08/code-coverage-best-practices.html"
  accessed="2024-07-04" >}}

1. {{< citation
  id="Petrovic2018"
  date="2018-05-27"
  authors="Goran Petrović; Marko Ivanković"
  title="State of Mutation Testing at Google"
  url="https://dl.acm.org/doi/10.1145/3183519.3183521"
  url_2="https://dl.acm.org/doi/pdf/10.1145/3183519.3183521"
  accessed="2024-07-04" >}}

1. {{< citation
  id="Stryker"
  title="Introduction | Stryker Mutator"
  url="https://stryker-mutator.io/docs/"
  accessed="2024-07-04" >}}
