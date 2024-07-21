---
date: 2024-04-27
domains:
- github.com
- highlightjs.org
- katex.org
- markdown-it.github.io
- news.ycombinator.com
- showdownjs.com
- stackoverflow.com
- w3c.github.io
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/markup-features/
title: Markup Features
---

## Syntax Highlighting

Previously, we'd highlight code on the client by loading
`src/lib/highlight.pack.js`, a bundle downloaded from {{% cite
HighlightJS %}} but served from our domain, and then execute
`hljs.highlightBlock` on demand, e.g., on page load, when showing a
card, etc. This doesn't work well with a web-component-centric design.
Running `hljs.highlightBlock` through possible Shadow DOM boundaries is
a hassle.

[Back in
2018](https://github.com/dchege711/study_buddy/commit/02768f1f58eb73d9ae55d1f1d7eb31ec93dd7867),
we installed `highlightjs`, a shim for the official HighlightJS. Times
have changed and {{% cite HighlightJS %}} has an official Node package,
[`highlight.js`](https://www.npmjs.com/package/highlight.js). Now, on
the server, compute the syntax-highlighted HTML and have it stored in
`Card.descriptionHTML`. All that's left is to provide CSS on the client.

## LaTeX Rendering

Like syntax highlighting, our current approach is to process LaTeX on
the client by calling `MathJax.Hub.Queue(["Typeset", MathJax.Hub,
elementId])` on demand. We did try to use it from the server back then,
but that was going against the grain {{% cite MathJaxSO %}}. The
situation doesn't seem to have changed since {{% cite MathJaxDemos %}}.

{{% cite KaTeX %}} promises to be fast in the browser and easy to use
for server-side-rendering. Will give KaTeX a shot and see how it works
out. The main package provides syntax like `const s =
katex.renderToString('e = mc^2')` which results in `<span
class="katex">...</span>`.

<details>
<summary>Rendered string expanded with whitespace for legibility</summary>

```html
<span class="katex">
  <span class="katex-mathml">
    <math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
      <semantics>
        <mrow>
          <mi>e</mi><mo>=</mo><mi>m</mi><msup><mi>c</mi><mn>2</mn></msup>
        </mrow>
        <annotation encoding="application/x-tex">e = mc^2</annotation>
      </semantics>
    </math>
  </span>
  <span class="katex-html" aria-hidden="true">
    <span class="base">
      <span class="strut" style="height: 0.4306em;"></span>
      <span class="mord mathnormal">e</span>
      <span class="mspace" style="margin-right: 0.2778em;"></span>
      <span class="mrel">=</span>
      <span class="mspace" style="margin-right: 0.2778em;"></span>
    </span>
    <span class="base">
      <span class="strut" style="height: 0.8641em;"></span>
      <span class="mord mathnormal">m</span>
      <span class="mord">
        <span class="mord mathnormal">c</span>
        <span class="msupsub">
          <span class="vlist-t">
            <span class="vlist-r">
              <span class="vlist" style="height: 0.8641em;">
                <span class="" style="top: -3.113em; margin-right: 0.05em;">
                  <span class="pstrut" style="height: 2.7em;"></span>
                  <span class="sizing reset-size6 size3 mtight">
                    <span class="mord mtight">2</span>
                  </span>
                </span>
              </span>
            </span>
          </span>
        </span>
      </span>
    </span>
  </span>
</span>
```

</details>

{{% cite MathWebHN %}} noted that on a math-heavy page, a page that
would otherwise be 50K gets inflated to ~1MB after SSR. With
compression, the difference is ~ the size of the compressed KaTeX
library, so might as well do CSR. Each card is ~2KB in total, and
looking at a cost of ~40KB per card. Skipping out on the MathML to save
on data size isn't worth it as MathML is used by ATs such as VoiceOver,
Orca, JAWS, and NVDA. {{% cite MathMLA11y %}}

Another challenge with using KaTeX is that parsing delimiters does not
come out of the box. {{% cite KaTeXAutoRender %}} provides a
`renderMathInElement(elem: HTMLElement): void` API, but assumes a
browser environment, e.g., calling `document.createDocumentFragment`.
{{% cite KatexElement %}} provides a `<katex-element>` web component,
but to know what to put in there, I'd still need delimited text --
having all of the card description in a `<katex-element>` would lead to
undesirable styling in non-math sections.

Can we solve {{% cite KaTeXAutoRender %}}'s dependence on the DOM using
{{% cite JSDOM %}}? Or can we extract the delimiter logic from {{% cite
KaTeXAutoRender %}}? The former will break when we upgrade `katex`, and
the latter will miss out on future bug fixes.

What about {{% cite markdownItTexMath %}}? That needs replacing {{% cite
Showdown %}} with {{% cite MarkdownIt %}} as the markdown-to-HTML
converter, but that should be mostly fine. Even better, {{% cite
MarkdownIt %}} preserves language hints for code-blocks and should avoid
the need for using {{% cite JSDOM %}}, promising advantages over {{%
cite Showdown %}}.

## References

1. {{< citation
  id="HighlightJS"
  title="highlight.js"
  url="https://highlightjs.org/"
  accessed="2024-04-27" >}}

1. {{< citation
  id="MathJaxSO"
  title="typescript - import mathjax throws error @types/mathjax/index.d.ts is not a module"
  url="https://stackoverflow.com/a/71090542"
  accessed="2024-04-27" >}}

1. {{< citation
  id="MathJaxDemos"
  title="MathJax-demos-node/simple at master · mathjax/MathJax-demos-node"
  url="https://github.com/mathjax/MathJax-demos-node/tree/master/simple#simple-component-examples"
  accessed="2024-04-27" >}}

1. {{< citation
  id="KaTeX"
  title="KaTeX – The fastest math typesetting library for the web"
  url="https://katex.org/"
  accessed="2024-04-27" >}}

1. {{< citation
  id="MathWebHN"
  title="Problems with math rendering on the web (2020) | Hacker News"
  url="https://news.ycombinator.com/item?id=27656446"
  url_2="https://news.ycombinator.com/item?id=27657305"
  accessed="2024-04-27" >}}

1. {{< citation
  id="MathMLA11y"
  title="MathML Accessibility Gap Analysis | MathML Document Repository"
  url="https://w3c.github.io/mathml-docs/gap-analysis/"
  accessed="2024-04-27" >}}

1. {{< citation
  id="KaTeXAutoRender"
  title="Auto-render Extension · KaTeX"
  url="https://katex.org/docs/autorender"
  accessed="2024-04-27" >}}

1. {{< citation
  id="KatexElement"
  title="georges-gomes/katex-element: `<custom-element>` for katex"
  url="https://github.com/georges-gomes/katex-element"
  accessed="2024-04-27" >}}

1. {{< citation
  id="markdownItTexMath"
  title="goessner/markdown-it-texmath: Support TeX math equations with your Markdown documents."
  url="https://github.com/goessner/markdown-it-texmath"
  accessed="2024-04-27" >}}

1. {{< citation
  id="Showdown"
  title="Showdownjs - A markdown to HTML converter"
  url="https://showdownjs.com/"
  accessed="2024-04-27" >}}

1. {{< citation
  id="MarkdownIt"
  title="markdown-it demo"
  url="https://markdown-it.github.io/"
  accessed="2024-04-27" >}}

1. {{< citation
  id="JSDOM"
  title="jsdom/jsdom: A JavaScript implementation of various web standards, for use with Node.js"
  url="https://github.com/jsdom/jsdom"
  accessed="2024-04-27" >}}
