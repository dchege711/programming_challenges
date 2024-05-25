---
title: Home Page
date: 2024-05-05
weight: 6
---

{{< figure
  src="/img/computer-science/programming-challenges/flashcards/web-components/home-landing-page.jpg"
  caption=`When a user lands at /home, this UI is shown. A couple of
  components are shareable from /browse, e.g., search-bar,
  search-results.` >}}

## Sharing Code with `/browse`

Components initially created for [the `/browse` page]({{< ref
"./browse-page" >}}) are useful in `/home` as well.

### The `CardsViewingPage` Interface

This functionality can be shared between the two pages:

```ts
export class CardsViewingPage extends LitElement {
  @provide({ context: searchResultsContext })
  @state() protected searchResults: CardSearchResult[] = [];
  @state() protected selectedResult: Card | null = null;

  @provide({ context: cardsCarouselContext })
  @state() protected cardsCarousel = new CardsCarousel([]);

  protected cardFetcher: CardFetchEndpoint;

  constructor(cardFetcher: CardFetchEndpoint) {
    super();
    this.cardFetcher = cardFetcher;
    this.addEventListeners();
  }

  render() {
    throw new Error('CardsViewingPage must be subclassed and implement render()');
  }

  // Add event listeners, e.g., search-results, search-result-selected,
  private addEventListeners() {...}

  // Call `this.cardFetcher` and set `this.selectedResult`.
  private updateSelectedCard(cardID: string) {...}

  static styles = css`
    :host {
      display: flex;
      flex-direction: column;
      gap: 10px;
    }
  `;
}

@customElement('home-page')
export class HomePage extends CardsViewingPage {
  constructor() {
    super(trpc.fetchCard.query);
  }
  render() {...}
}

@customElement('browse-page')
export class BrowsePage extends CardsViewingPage {
  constructor() {
    super(trpc.fetchPublicCard.query);
  }
  render() {...}
}
```

### Customizing the `<search-bar>` Component

The difference between the `<search-bar>` rendered by `/browse` and the
one rendered by `/home` is the endpoint used by `SearchBar.fetchResults`
(either `trpc.searchPublicCards.query` or `trpc.searchCards.query`). We
currently pass a `boolean` to distinguish, but is it possible to pass
the endpoint itself so as to be "closer to the metal"? While this code
type-checks:

```ts
export type CardSearchEndpoint = typeof trpc.searchCards.query | typeof trpc.searchPublicCards.query;

@customElement('search-bar')
export class SearchBar extends LitElement {
  searchEndpoint: CardSearchEndpoint | null = null;
  // ...
}

@customElement('browse-page')
export class BrowsePage extends CardsViewingPage {
  // ...
  render() {
    return html`
      <search-bar .searchEndpoint=${trpc.searchPublicCards.query}>
      </search-bar>
      ...
    `;
  }
}
```

... it fails at runtime with `Uncaught (in promise) TypeError:
nextDirectiveConstructor is not a constructor` after a `Static values
'literal' or 'unsafeStatic' cannot be used as values to non-static
templates. Please use the static 'html' tag function. See
https://lit.dev/docs/templates/expressions/#static-expressions` warning.
It's [possible to pass functions via data
attributes](https://lit.dev/playground/#project=W3sibmFtZSI6InNpbXBsZS1ncmVldGluZy50cyIsImNvbnRlbnQiOiJpbXBvcnQge2h0bWwsIExpdEVsZW1lbnR9IGZyb20gJ2xpdCc7XG5pbXBvcnQge2N1c3RvbUVsZW1lbnR9IGZyb20gJ2xpdC9kZWNvcmF0b3JzLmpzJztcblxuY29uc3QgbGVuZ3RoQ2FsYyA9IChzOiBzdHJpbmcpID0-IHsgcmV0dXJuIHMubGVuZ3RoOyB9XG5cbnR5cGUgUGFzc2FibGVGbiA9IHR5cGVvZiBsZW5ndGhDYWxjO1xuXG5AY3VzdG9tRWxlbWVudCgnc2FtcGxlLWVsZW1lbnQnKVxuY2xhc3MgU2FtcGxlRWxlbWVudCBleHRlbmRzIExpdEVsZW1lbnQge1xuICBmOiBQYXNzYWJsZUZuIHwgbnVsbCA9IG51bGw7XG4gIFxuICByZW5kZXIoKSB7XG4gICAgcmV0dXJuIGh0bWxgPHA-VGhlIHJlc3VsdCBpcyAke3RoaXMuZignU28nKX08L3A-YDtcbiAgfVxufVxuXG5AY3VzdG9tRWxlbWVudCgnc2ltcGxlLWdyZWV0aW5nJylcbmV4cG9ydCBjbGFzcyBTaW1wbGVHcmVldGluZyBleHRlbmRzIExpdEVsZW1lbnQge1xuICByZW5kZXIoKSB7XG4gICAgcmV0dXJuIGh0bWxgXG4gICAgICAgIDxzYW1wbGUtZWxlbWVudCAuZj0ke2xlbmd0aENhbGN9Pjwvc2FtcGxlLWVsZW1lbnQ-XG4gICAgYDtcbiAgfVxufVxuIn0seyJuYW1lIjoiaW5kZXguaHRtbCIsImNvbnRlbnQiOiI8IURPQ1RZUEUgaHRtbD5cbjxoZWFkPlxuICA8c2NyaXB0IHR5cGU9XCJtb2R1bGVcIiBzcmM9XCIuL3NpbXBsZS1ncmVldGluZy5qc1wiPjwvc2NyaXB0PlxuPC9oZWFkPlxuPGJvZHk-XG4gIDxzaW1wbGUtZ3JlZXRpbmcgbmFtZT1cIldvcmxkXCI-PC9zaW1wbGUtZ3JlZXRpbmc-XG48L2JvZHk-XG4ifSx7Im5hbWUiOiJwYWNrYWdlLmpzb24iLCJjb250ZW50Ijoie1xuICBcImRlcGVuZGVuY2llc1wiOiB7XG4gICAgXCJsaXRcIjogXCJeMy4wLjBcIixcbiAgICBcIkBsaXQvcmVhY3RpdmUtZWxlbWVudFwiOiBcIl4yLjAuMFwiLFxuICAgIFwibGl0LWVsZW1lbnRcIjogXCJeNC4wLjBcIixcbiAgICBcImxpdC1odG1sXCI6IFwiXjMuMC4wXCJcbiAgfVxufSIsImhpZGRlbiI6dHJ1ZX1d),
so that's not what's happening here. The use case does not match the one
described in {{% cite expressionsLit %}}:

```ts
class MyButton extends LitElement {
  tag = literal`button`;

  render() {
    const activeAttribute = getActiveAttribute(); // Must be trusted!
    return html`
      <${this.tag} ${unsafeStatic(activeAttribute)}=${this.active}>
      </${this.tag}>
    `;
  }
```

Using `.searchEndpoint=${(q: CardSearchQuery) =>
trpc.searchPublicCards.query(q)}` works though. Huh, I hope this doesn't
bite back in the future.

## References

1. {{< citation
  id="expressionsLit"
  title="Expressions – Lit > Static expressions"
  url="https://lit.dev/docs/templates/expressions/#static-expressions"
  accessed="2024-05-05" >}}
