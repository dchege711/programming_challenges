---
date: 2026-10-25
domains:
- doc.rust-lang.org
- www.hellointerview.com
local_url: http://localhost:1313/computer-science/programming-challenges/low-level-design/connect-four/connect-four/
title: Connect Four
---

## Problem Statement

Build the object-oriented design for a two-player Connect Four game. Players
take turns dropping discs into a 7-column, 6-row board. The first to align 4 of
their own discs vertically, horizontally, or diagonally wins. {{% cite
ConnectFourHelloInterview %}}

{{< figure
  src="/img/computer-science/low-level-design/connect-four/play.gif"
  caption="Playing Connect Four with red winner example. Source: Wikimedia." >}}

## Requirements

These are guided by clarifying questions that determine the core operations to
support, system states and their transitions, handling errors, and establishing
scope.

* Two players take turns dropping discs into a 7-column, 6-row board.
* A disc falls to the lowest available row in the chosen column.
* The game ends when:
  * A player gets 4 discs in a row (vertical, horizontal, or diagonal). They win.
  * The board is full. It's a draw.
* Invalid moves should be rejected early: dropping in a full column, moving out
  of turn, moving after the game is over.
* Out of scope: UI support, concurrent games, move history, undo, board size
  configuration.

{{% cite ConnectFourHelloInterview %}}

{{% comment %}}

For out of scope, I also had: designing an AI opponent; multiplayer over the
network. Variations like "sometimes the game allows a player to make two
consecutive moves" directly contradict the prompt, despite them adding a twist
to the game.

{{% /comment %}}

## Core Entities and Relationships

The **Player** is identified by either `Red` or `Yellow`. The **Board** holds
the discs, owns disc placement, and can tell if 4 discs are connected. The
**Game** keeps track of whose `Player`'s turn it is, allows a `Player` to place
a disc, and keeps track of the `Won`, `Drawn`, `InProgress` states.

## Implementation

Using boolean flags for game state, e.g., `isOver`, `hasWinner`, and `isDraw`,
makes impossible states representable despite your domain not allowing it, e.g.,
`isOver=false` and `hasWinner=true` is not prevented by the type system, but
there can't be a winner without the game being over. {{% cite
ConnectFourHelloInterview %}}

{{% comment %}}

I've been told to look into domain driven design before. This is a simplified
example on the usefulness of creating types that are domain-driven.

{{% /comment %}}

Defining `enum Player { Red, Yellow }` and storing `Player`s in `Board` leads to
downstream complications, e.g., what if `Player` gets more properties? An `enum
DiscColor { Red, Yellow}` maps better to the requirements, e.g., two players
take turns dropping their own discs. {{% cite ConnectFourHelloInterview %}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/POD.cs"
  highlight="cs" >}}

`Board.CheckWin` can trip us into over-engineering. A polymorphic `WinChecker`,
e.g., `HorizontalWinChecker`, `VerticalWinChecker`, etc., is over-engineered
because we'll never need another type of `WinChecker`, and we don't need to swap
out win-checking logic at runtime. {{% cite ConnectFourHelloInterview %}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/Board.cs"
  highlight="cs" >}}

{{% cite ConnectFourHelloInterview %}} argues that `Game` shouldn't call
`CanPlaceDisc` first; for separation of concerns, `Board.PlaceDisc` should
internalize the `CanPlaceDisc` check, returning a sentinel value when the
placement fails. I disagree. `Game` still needs to know about the sentinel value
and so there's not really a separation of concerns; I prefer an explicit
`CanPlaceDisc` call.

Some languages, e.g., Rust, support associating enums with data, e.g.,

```rs
enum Message {
  Quit,
  Move { x: i32, y: i32 },
  Write(String),
  ChangeColor(i32, i32, i32),
}
```

... which would allow us to store the winning `Player` alongside `State.Won`.
{{% cite EnumsRust %}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/Game.cs"
  highlight="cs" >}}

## Extensibility: Supporting `Undo`

Supporting `Undo` tests whether your design has a clean separation between
orchestration (`Game`) and state (`Board`). {{% cite ConnectFourHelloInterview
%}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/GameWithUndo.cs"
  highlight="cs" >}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/Board.WithUndo.cs"
  highlight="cs" >}}

## Extensibility: Adding a Computer Opponent

An abstraction atop `Player` (e.g., `HumanPlayer`, `BotPlayer`) doesn't add much
value because `HumanPlayer` doesn't "do" anything. Instead, change the game
loop. {{% cite ConnectFourHelloInterview %}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/Board.BotOpponent.cs"
  highlight="cs" >}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/Game.BotOpponent.cs"
  highlight="cs" >}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour/BotEngine.cs"
  highlight="cs" >}}

{{< readFile
  file="/content/computer-science/low-level-design/exercises/connect-four/ConnectFour.Tests/GameLoopTests.cs"
  highlight="cs" >}}

## References

1. {{< citation
  id="ConnectFourHelloInterview"
  title="Connect Four Low Level Design | Hello Interview Low Level Design"
  url="https://www.hellointerview.com/learn/low-level-design/problem-breakdowns/connect-four"
  accessed="2026-10-25" >}}

1. {{< citation
  id="EnumsRust"
  title="Defining an Enum - The Rust Programming Language"
  url="https://doc.rust-lang.org/book/ch06-01-defining-an-enum.html"
  accessed="2026-10-25" >}}
