# OPS5 in Racket

A Racket conversion of the OPS5 production-rule interpreter written in
Scheme for the 1995 Springer-Verlag book by Mark Watson. OPS5 is a
forward-chaining rule system: you define productions (rules) with `p`,
add facts to working memory with `make`, and run the match-fire loop
with `run`.

Two example programs are included:

- `draw.ops`: card hand analysis. Finds pairs and three of a kind in a
  poker hand.
- `monkey.ops`: the classic monkey and bananas planning problem. A
  monkey on a couch works out how to get bananas from the ceiling using
  a ladder.

## Requirements

Racket. No packages to install; the code uses only the standard
library.

## Starting the interpreter

Run the driver from this directory:

```
racket load.rkt
```

You can also name one or more `.ops` files on the command line. They
load before the REPL starts:

```
racket load.rkt draw.ops
```

The REPL prompt is `OPS5>`. Useful commands:

| Command              | Action                                  |
|----------------------|-----------------------------------------|
| `(load "draw.ops")`  | load a program file                     |
| `(i-g-v)`            | initialize (or reset) OPS5              |
| `(p name lhs --> rhs)` | define a production                   |
| `(make class ...)`   | add a working-memory element            |
| `(run)`              | run the productions                     |
| `(wm)`               | print working memory                    |
| `(exit)`             | leave the REPL (EOF also works)         |

## Running draw.ops

`draw.ops` calls `(i-g-v)` and asserts its own data (a goal and four
cards) at load time, so you only need `(run)`:

```
$ racket load.rkt draw.ops

OPS5> (run)
 three of a kind heart diamond diamond 10
 ...
found a pair club 10 heart
found a pair club 10 diamond
found a pair diamond 10 club
 ...
found a pair diamond 10 heart
 ...
End -- no production true

(2 productions (42 // 42 nodes))(28 firings (33 RHS actions)) ...
```

Expected result: 28 firings. The rules report three of a kind for each
ordered suit triple and the pairs among the three 10 cards.

## Running monkey.ops

`monkey.ops` defines its rules and a startup production `t1`, but it
does not assert the trigger fact. Do that by hand, then run:

```
$ racket load.rkt monkey.ops

OPS5> (make start 1)
OPS5> (run)

I am a monkey lying on the couch
... a heavy couch
there are some bananas on the ceiling at position 2-2
there is a ladder on the floor at position 9-5
I sure would like those bananas

The action begins:

Since the bananas are on the ceiling at position 2-2
I would like to move the ladder under them.
...
I will now climb onto ladder
what I want to do now is get the bananas
End -- no production true

(19 productions (212 // 212 nodes))(16 firings (42 RHS actions)) ...
```

Expected result: 16 firings. The run stops after the monkey climbs the
ladder and states its goal. That is the natural end state of
`monkey.ops` itself, not an engine bug: production `mb3` creates a goal
with `object nil`, `mb4` tests `holds nil`, and `mb4` sets the status
to the misspelled `satified`, so no production matches the final state.
See `PROBLEMS.md` for details.

## One program per session

Run each example in a fresh session. Two quirks make mixing programs in
one REPL confusing:

1. `(i-g-v)` excises (removes) all loaded productions. If you reset and
   then call `(run)`, nothing fires because no rules remain. Reload the
   `.ops` file after `(i-g-v)`.
2. Loading a second `.ops` file without resetting keeps the old working
   memory, so rules can fire against stale facts.

The simple habit: `(exit)` and relaunch with the next file:

```
racket load.rkt draw.ops
...
racket load.rkt monkey.ops
```

## Syntax notes for writing your own .ops files

- Curly braces `{` and `}` must be wrapped in double quotes: `"{"` and
  `"}"`.
- Put spaces around the `^` attribute marker: `(object ^ name <w>)`.
- Variables are atoms in angle brackets: `<suit>`, `<num>`.
- `(make ...)` adds a fact, `(modify n ^ attr value)` changes the fact
  with time tag `n`, and `(ops-write ...)` prints, with `(crlf)` for a
  newline.

## Files

| File          | Role                                            |
|---------------|-------------------------------------------------|
| `load.rkt`    | driver: loads the system into a namespace, starts the REPL |
| `compat.rkt`  | compatibility layer for old MIT Scheme idioms   |
| `ops5.rkt`    | top-level OPS5 commands                         |
| `compiler.rkt`| production compiler (LHS to Rete network)       |
| `network.rkt` | Rete network construction and token flow        |
| `rhs.rkt`     | right-hand side actions                         |
| `lit.rkt`     | literalize support                              |
| `draw.ops`    | card hand example                               |
| `monkey.ops`  | monkey and bananas example                      |
| `PROBLEMS.md` | log of the conversion work and bugs fixed       |
