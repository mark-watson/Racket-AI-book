# Racklog Expert Systems / Logic Examples

The same two problems as `../OPS5_in_Racket`, solved with
[Racklog](https://docs.racket-lang.org/racklog/) (Prolog-style logic
programming embedded in Racket) instead of OPS5 production rules, so the
two approaches can be compared side by side.

| File         | Problem              | OPS5 counterpart                    |
|--------------|----------------------|-------------------------------------|
| `monkey.rkt` | monkey and bananas   | `../OPS5_in_Racket/monkey.ops`      |
| `draw.rkt`   | card hand analysis   | `../OPS5_in_Racket/draw.ops`        |

## Requirements

```
raco pkg install racklog
```

## Run

```
racket monkey.rkt
racket draw.rkt
```

## OPS5 vs Racklog, in one paragraph

OPS5 is *forward chaining*: facts live in working memory, rules match
against them, and firing a rule asserts new facts until nothing matches.
The program is the rule set plus initial facts; control flow is implicit
in the match-fire loop. Racklog is *backward chaining*: you state a goal
(`%which (plan) (%canget start ... plan)`) and the system works backward
through relations, unifying variables and backtracking on failure. The
monkey problem shows the difference most clearly: OPS5 narrates the
monkey's actions as rules fire in order, while Racklog searches the
state space declaratively and returns the plan as an answer to a query.

## Racklog notes

- Relations are defined with `%rel`; clause heads are Racket
  expressions, so constant symbols are quoted (`'floor`) and compound
  terms use `list`: `(list 'state p 'floor 'nothing p)`.
- Logic variables are listed in the `%rel` second subform; use named
  throwaway variables instead of `_` in heads.
- `%which` returns one answer as an association list; `%more` backtracks
  for the next; `%find-all` collects every answer.
- `%member`, `%/=`, `%not`, `%is` come built in.
