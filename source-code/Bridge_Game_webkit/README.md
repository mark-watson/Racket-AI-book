# Racket WebKit Rubber Bridge

A rubber bridge game in a native macOS window. You play South against
three computer players. Partner North and the opponents East and West use
heuristic Standard American bidding (Stayman, Blackwood, Gerber, weak
twos, preempts) and simple card play rules: top of a sequence, fourth
best, second hand low, cheapest winning card, ruffing and discarding.

This is a Racket port of the Haskell `Bridge_webkit` example. It uses the
same HTML/CSS/JavaScript front end and the same small Objective-C WebKit
wrapper. The wrapper is called through Racket's FFI instead of Haskell's.

## Architecture

```
  index.html  (felt table, card fan, bidding box, score sheet)
      |   window.webkit_racket.invoke(command, payload)  -> Promise
      v
  webkit_racket.m -> libwebkit_racket.dylib   (NSWindow + WKWebView)
      |   C callback: const char* cb(command, payload_json, userdata)
      v
  webkit.rkt      (ffi/unsafe binding, JSON in/out)
      v
  bridge-api.rkt  (session state, command handlers, JSON marshalling)
      v
  engine.rkt  bidding.rkt  play.rkt  scoring.rkt  cards.rkt   (pure game logic)
```

The game logic is purely functional. A `game` is an immutable struct, and
`apply-bid` and `apply-card-play` return a new one. `bridge-api.rkt` holds
the single mutable variable, which is the session in progress. It does not
depend on WebKit, so `tests.rkt` can play complete rubbers without opening
a window.

## Files

| File | Purpose |
|------|---------|
| `cards.rkt` | players, cards, deck, dealing, hand evaluation |
| `bidding.rkt` | auction rules and the AI bidder |
| `play.rkt` | trick rules and the AI card player |
| `scoring.rkt` | rubber bridge scoring |
| `engine.rkt` | the deal state machine |
| `bridge-api.rkt` | UI commands and JSON state |
| `webkit.rkt` | FFI binding to the WebKit window |
| `main.rkt` | program entry point |
| `webkit_racket.m/.h` | Objective-C Cocoa/WKWebView wrapper |
| `index.html` | the user interface |
| `tests.rkt` | unit tests and headless rubber simulations |

## Commands sent by the UI

| Command | Payload | Effect |
|---------|---------|--------|
| `get-state` | `{}` | return the current state |
| `bid` | `{"bid": "1H"}` (also `Pass`, `DBL`, `RDBL`) | South bids, then the AI bids until South's next turn |
| `play-card` | `{"card": "10D"}` | South plays a card (or plays North's card when North is dummy) |
| `ai-play-single` | `{}` | the computer plays one card for the seat whose turn it is |
| `next-deal` | `{}` | deal the next hand of the rubber |
| `reset-game` | `{}` | start a new rubber |

Every command returns the full game state as JSON, or `{"error": "..."}`.

## Build and run (macOS only)

```
make          # compiles libwebkit_racket.dylib with clang
racket main.rkt
make test     # runs the unit tests and headless simulations
```

You need Racket 8 or later and the Xcode command line tools for `clang`.
No Racket packages need to be installed.

## Notes and differences from the Haskell version

- When partner North wins the contract, the North and South hands are
  swapped so that you play as declarer from the South seat, the same as in
  the Haskell version.
- Redoubles are scored. The Haskell `extractContract` scanned the auction
  newest first and missed a redouble that came after a double.
- Below-the-line points are added to the above-the-line total when a game
  is won, so the final rubber score includes them.
- A passed-out deal moves to the next dealer.
- When an AI player is void in the led suit, it ruffs only with a trump
  that wins the trick. Otherwise it discards.
- The UI's "Done" phase is reported when the rubber ends, and it offers a
  **New Rubber** button.
