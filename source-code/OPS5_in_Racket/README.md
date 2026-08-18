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
racket OPS5_all.rkt draw.ops      # then type (run)
racket OPS5_all.rkt monkey.ops    # then (make start 1) and (run)
racket OPS5_all.rkt               # or just the REPL, then (load "draw.ops")
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


## One program per session

Run each example in a fresh session. Two quirks make mixing programs in
one REPL confusing:

1. `(i-g-v)` excises (removes) all loaded productions. If you reset and
   then call `(run)`, nothing fires because no rules remain. Reload the
   `.ops` file after `(i-g-v)`.
2. Loading a second `.ops` file without resetting keeps the old working
   memory, so rules can fire against stale facts.

The simple habit: `(exit)` and relaunch with the next file.


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
