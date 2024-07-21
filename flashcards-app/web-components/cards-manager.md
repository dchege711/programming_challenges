---
date: 2024-04-28
domains:
- developer.mozilla.org
- github.com
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/web-components/cards-manager/
title: The Cards Manager
weight: 4
---

## UI Design

{{< figure
  src="/img/computer-science/programming-challenges/flashcards/web-components/browse-card-viewer.jpg"
  caption=`Legacy card viewing UI at /browse.` >}}

This time we'll use the more semantic and a11y-friendly `<dialog>`
element {{% cite dialogMDN %}}. Centering it in the page is done by the
browser, and that saves us a bit of hassle -- thought it would have been
feasible with how `<search-bar>`'s `<ul>` floats above the page.

## `CardsManager` Interface

The previous/next buttons make use of the `CardsManager` object that has
the API:

```ts
interface MiniCard { _id: string; urgency: number, tags: string[] }
interface TagsAndIds { [tag: string]: { [id: string]: { urgency: number }} }

class CardsManager {
  constructor(tagsAndIds: TagsAndIds, userID: string, cardSourceURL="/read-card", minicards: MiniCard[] = []);
  function initializeFromTags(tagsToUse: Set<string> | null): Promise<void>;
  function initializeFromMinicards(minicards: MiniCard[], includeTagNeighbors=false): Promise<void>;
  function initializeFromTrash(cardIds: string[]): Promise<void>;
  function fetchCard(cardId: string): Promise<Card | null>;
  function next(): Promise<Card | null>;
  function hasNext(): Boolean;
  function previous(): Promise<Card | null>;
  function hasPrevious(): Boolean;
  function removeCard(cardId: string): string;
  function status(): void;
  function insertCard(cardId: string, urgency: number): void;
  function updateCard(card: Card): void;
  function quartiles(): [number, number, number, number, number];
  function saveCard(card: Card, url: string);
}
```

Internally, the `CardsManager` uses {{% cite avlW8R %}}'s AVL tree which
is available both in a Node and in a browser environment.

At its core, the `CardsManager` maintains the queue of cards that are
accessible using the previous and next buttons. The card itself is
responsible for actions like persisting its content on the server. We
can use the "events go up; properties come down" design principle to
have a card announce changes in its content, and the `CardsManager` can
listen for such updates and updates its state accordingly.

The `CardsManager` should therefore be at the top-level web component.
It can be availed via `Context` to descendants that need it, e.g., the
previous and next buttons.

Having 4 ways of initializing a `CardsManager` seems excessive. The
current initialization paths are used as:

```ts
// Browse page
cardsManager = new CardsManager(tagsAndIDs, publicUserID, '/read-public-card');
cardsManager.initializeFromMinicards([{ _id: cardIdToShow, urgency, tags}], includeTagNeighbors=true);

// Home
cardsManager = new CardsManager(tagsAndIDs, currentUserID, '/read-card', mini_cards);
cardsManager.initializeFromTags(selectedTags || new Set([]));
cardsManager.initializeFromMinicards([clickedSearchResult]);

// Account page
cardsManager = new CardsManager(metadata.node_information[0], currentUserID);
cardsManager.initializeFromTrash(state.metadata.trashed_cards[0]);
```

The fact that [we're using tRPC]({{< ref
"/computer-science/programming-challenges/flashcards-app/client-server-interface#typing-the-clientserver-interface"
>}}) means that we can replace `cardSourceURL` with
`cardSearchEndpoint`. `CardsManager` queries the endpoint in
`fetchCard`, `next`, `previous`, and `status`. However, this is not
strictly needed. For instance, in the web-component based `/browse`, the
`<browse-page>` component queries the endpoint in response to a
`search-result-selected` message. Similarly, the next/previous buttons
can fire this event, and have the `<browse-page>` load the appropriate
card, and pass it as a property to the `public-card-viewer`.

The `userId` parameter is also not needed. The endpoints themselves
enforce the appropriate user ID, e.g., `fetchCard` enforces the ID that
is in the session object.

`CardsManager.initializeFromTags` is not needed anymore, given that we
are doing away with the tags bar on the left. Selecting cards that match
a given tag will be done via the search box, which will bubble up the
`CardSearchResult[]` via the `search-results` event to the
`searchResultsContext`.

`CardsManager.initializeFromMinicards` is a weird one. `/home` passes
`includeTagNeighbors=false`, the `CardsManager` only has 1 card in the
queue, which is somewhat less useful. That said, this case is still
covered by the `search-results` event. `/browse` passes
`includeTagNeighbors=true`, which makes the `CardsManager` also add
cards that share a tag with the card that is being shown based on the
`TagsAndIds` passed in the ctor. This can be computed instead by
`<browse-page>` and passed in `CardsManager`'s ctor as a `MiniCard[]`.

`CardsManager.initializeFromTrash` should be removed from `/account` and
in its place, the user can access deleted cards from `<search-results>`.

With the above changes, `CardsManager` can be initialized through the
simpler `constructor(miniCards: MiniCard[])` that also captures that
`CardsManager` deals with the set of cards that the user can see. It
would also be beneficial to rename the class to something
`CardsCarousel` instead of the `CardsManager` which promises more
functionality than is provided.

## References

1. {{< citation
  id="dialogMDN"
  title="`<dialog>`: The Dialog element - HTML: HyperText Markup Language | MDN"
  url="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog"
  accessed="2024-04-28" >}}

1. {{< citation
  id="avlW8R"
  title="w8r/avl: :eyeglasses: Fast AVL tree for Node and browser"
  url="https://github.com/w8r/avl"
  accessed="2024-04-28" >}}
