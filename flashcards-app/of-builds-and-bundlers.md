---
title: Of Builds and Bundlers
date: 2024-04-20
---

Moving to TypeScript entails configuring how the JavaScript will be
eventually consumed by both the server and the client.

## Separating the Client Bundle from the Server Bundle

The server code runs in `Node` while the client code runs in the
browser. {{% cite projectReferences %}} advises separating the
configurations for advantages like faster type-checking and compiling,
lower memory usage when using an editor, and improved enforcement of the
logical groupings of your program.

Webpack utilizes loaders to preprocess files, allowing the dev to bundle
any static resource. For example, `markdown-loader` compiles markdown
into HTML. {{% cite webpackLoaders %}} I've been using
[`ts-loader`](https://github.com/TypeStrong/ts-loader), and it should be
informative to know what else is out there, e.g.,
[`esbuild-loader`](https://github.com/privatenumber/esbuild-loader),
which has been mentioned at work. `esbuild-loader`'s claim to fame is
speed; other benefits include transpiling to ES6+ using `esbuild`.
`ts-loader` admits to being slow, but because of type-checking all files
in every rebuild though granted that can be dangerously disabled via
`transpileOnly: true`. [`esbuild`](https://esbuild.github.io/) makes
bold claims about being pretty fast, e.g., 87x faster than `rollup 4 +
terser`.

## ES Modules vs. CommonJS

If `package.json` omits a value for `type`, `.js` files are treated as
if the `package.json` specified `"type": "commonjs"`. To override the
`package.json` `type` value for a given file, use either `.mjs` for
module treatment or `.cjs` for CommonJS treatment. {{% cite nodeModules
%}} On the client side, it seems using ESM is an easy choice given that
we want to use `esbuild-loader`. The server-side is a bit more
complicated; we have CommonJS working and the attempt at ESM ended up
with challenges similar to those of {{% cite esmNodeReddit %}}. Shelving
ESM on the server to a later iteration of the app.

## References

1. {{< citation
  id="projectReferences"
  title="TypeScript: Documentation - Project References"
  url="https://www.typescriptlang.org/docs/handbook/project-references.html#handbook-content"
  accessed="2024-04-20" >}}

1. {{< citation
  id="webpackLoaders"
  title="Loaders | webpack"
  url="https://webpack.js.org/loaders/"
  accessed="2024-04-21" >}}

1. {{< citation
  id="nodeModules"
  title="Modules: Packages | Node.js v22.0.0 Documentation"
  url="https://nodejs.org/api/packages.html#type"
  accessed="2024-04-29" >}}

1. {{< citation
  id="esmNodeReddit"
  title="ESM not gaining traction in back-end Node? : node"
  url="https://www.reddit.com/r/node/comments/14rg9ym/esm_not_gaining_traction_in_backend_node/"
  accessed="2024-04-29" >}}
