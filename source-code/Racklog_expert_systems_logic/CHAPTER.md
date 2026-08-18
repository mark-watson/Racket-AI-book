# Logic Programming with Racklog: Two Classic Problems Revisited

In the previous chapter we used OPS5, a *forward-chaining* production
rule system, to solve two classic problems: analyzing a poker hand and
planning how a monkey can reach bananas hanging from a ceiling. In this
chapter we solve the *same two problems* with Racklog, a Prolog-style
logic programming library embedded in Racket. Keeping the problems
identical lets us focus on what actually changed: the reasoning
strategy.

## Forward Chaining vs. Backward Chaining

A production system like OPS5 reasons **forward**. Facts sit in a
working memory. Rules continuously pattern-match against those facts,
and whenever a rule's conditions are satisfied it *fires*, adding or
modifying facts. The program never states a goal explicitly; the
desired outcome emerges when the match-fire loop has transformed the
initial facts into the final ones. You saw this in the monkey example:
rules `mb1` through `mb18` fired one after another, narrating the
monkey's actions until the goal fact was marked satisfied.

Logic programming reasons **backward**. We define *relations* (facts
and rules that describe what is true) and then pose a **query**. The
system works backward from the query, trying to prove it by unifying it
against the heads of rules and recursively proving their bodies,
backtracking whenever a choice fails. The query itself names the goal,
and the answer is the set of variable bindings that make the query true.

In forward chaining we say "whenever you see this situation, do this."
In backward chaining we say "here is what it means to reach the goal;
find me a way." For the monkey problem, the difference is vivid: OPS5
*performs* the plan step by step, while Racklog *searches* for a plan
and returns it as the answer to a question.

Racklog is not a separate language you shell out to; it is an ordinary
Racket library. Logic variables, unification, and backtracking are
implemented as Racket macros and procedures, so a logic program lives
inside a normal `#lang racket` file and can call, and be called by,
regular Racket code.

## Problem One: Analyzing a Card Hand

Recall the draw poker example. The hand we analyze is exactly the one
we gave OPS5:

- 10 of hearts
- 10 of diamonds
- 10 of clubs
- 4 of diamonds

We want to find every pair and every three of a kind. In OPS5 we wrote
productions that watched working memory and asserted `pair` and `three`
facts. In Racklog we do something simpler: the hand itself is a
relation, and "pair" and "three of a kind" are *derived* relations
defined by rules over it.

A Racklog relation is defined with `%rel`, which takes a list of logic
variables followed by clauses. Each clause has a head (the argument
pattern) and an optional body of goals that must all succeed. One
syntactic detail matters: clause heads are ordinary Racket expressions,
so constant symbols are quoted (`'heart`) and compound terms are built
with `list`.

### The code

Here is the complete file `draw.rkt`:

```racket
#lang racket

;; Card hand analysis in Racklog (logic programming / backward chaining).
;; Same hand and queries as ../OPS5_in_Racket/draw.ops (production rules /
;; forward chaining): find pairs and three of a kind.
;;
;; Run: racket draw.rkt

(require racklog)

;; The hand, as facts: %card(Suit, Rank)
(define %card
  (%rel ()
    [('heart 10)]
    [('diamond 10)]
    [('club 10)]
    [('diamond 4)]))

;; Suit ordering, so each combination is reported only once
;; (club 10 + diamond 10, but not also diamond 10 + club 10).
(define %suit<
  (%rel ()
    [('club 'diamond)] [('club 'heart)] [('club 'spade)]
    [('diamond 'heart)] [('diamond 'spade)]
    [('heart 'spade)]))

;; %pair(Suit1, Suit2, Rank)
(define %pair
  (%rel (s1 s2 n)
    [(s1 s2 n)
     (%card s1 n)
     (%card s2 n)
     (%suit< s1 s2)]))

;; %three-of-a-kind(Suit1, Suit2, Suit3, Rank)
(define %three-of-a-kind
  (%rel (s1 s2 s3 n)
    [(s1 s2 s3 n)
     (%card s1 n)
     (%card s2 n)
     (%card s3 n)
     (%suit< s1 s2)
     (%suit< s2 s3)]))

(printf "Card hand analysis -- Racklog version")
(printf " (compare with ../OPS5_in_Racket/draw.ops)\n\n")
(printf "Hand: 10 of hearts, 10 of diamonds, 10 of clubs, 4 of diamonds.~n~n")

(for ([p (%find-all (s1 s2 n) (%pair s1 s2 n))])
  (printf "pair:            ~a of ~a and ~a of ~a~n"
          (cdr (assq 'n p)) (cdr (assq 's1 p))
          (cdr (assq 'n p)) (cdr (assq 's2 p))))

(for ([t (%find-all (s1 s2 s3 n) (%three-of-a-kind s1 s2 s3 n))])
  (printf "three of a kind: ~a of ~a, ~a, and ~a~n"
          (cdr (assq 'n t)) (cdr (assq 's1 t))
          (cdr (assq 's2 t)) (cdr (assq 's3 t))))
```

The `%card` relation is pure data: four facts, one per card. The
interesting part is `%pair`. Its single clause says: `(s1 s2 n)` is a
pair if `%card` holds for suit `s1` at rank `n`, `%card` holds for a
different suit `s2` at the *same* rank `n`, and `s1` sorts before `s2`.
Because the logic variable `n` appears in both `%card` goals,
unification forces both cards to share a rank. That single shared
variable does the work that the OPS5 rule expressed with the pattern
`(card <suit> <num>)` matched twice against working memory.

The `%suit<` relation deserves a second look. The OPS5 version used
negated conditions like `-(pair <suit2> <suit> <num>)` to avoid
reporting the same pair twice in different orders. In logic programming
we get the same effect declaratively: by requiring `s1` to come before
`s2` in a fixed suit ordering, each unordered combination is generated
exactly once, and no bookkeeping facts are needed.

The two `%find-all` forms at the bottom are the bridge back to ordinary
Racket. `%find-all` runs a query, collects every solution, and returns
a plain Racket list of association lists, which we then print with an
everyday `for` loop. `%which` (used in the next example) returns just
the first solution, and `%more` would backtrack to the next one.

### Running it

```
$ racket draw.rkt
Card hand analysis -- Racklog version (compare with ../OPS5_in_Racket/draw.ops)

Hand: 10 of hearts, 10 of diamonds, 10 of clubs, 4 of diamonds.

pair:            10 of diamond and 10 of heart
pair:            10 of club and 10 of heart
pair:            10 of club and 10 of diamond
three of a kind: 10 of club, diamond, and heart
```

### Interpreting the output

Three cards share the rank 10, and three cards taken two at a time give
exactly three pairs, which is exactly what the program reports. The
lone 4 of diamonds participates in nothing and correctly never appears
in the output. The single three of a kind uses all three 10s.

Compare the shape of the two solutions. The OPS5 program was about
*events*: rules firing, facts appearing in working memory. This program
contains no events at all: `%pair` is a timeless statement about what
"pair" means, and the three lines of output are simply every way the
statement can be made true for this hand. The cost of that elegance is
that control is implicit: the order in which solutions are found
depends on clause order and Racklog's depth-first search, something we
will see matter more in the next example.

## Problem Two: The Monkey and the Bananas

The monkey problem is a *planning* problem, and it is where the two
paradigms diverge most. Recall the scenario: a monkey is on a couch at
position 5-7; bananas hang from the ceiling at 2-2; a light ladder
stands on the floor at 9-5. The monkey can walk, carry the ladder,
climb it, and grasp the bananas.

To solve this declaratively we model the world as a **state**, a
snapshot of everything that can change:

```$
(\textit{monkey-at},\ \textit{monkey-on},\ \textit{holds},\ \textit{ladder-at})
```

The bananas never move, so they are not part of the state; they are a
fixed fact about the world. A **plan** is then a sequence of actions
that transforms the initial state into any state where the monkey holds
the bananas. This is the classic state-space search formulation: states
are nodes, actions are edges, and planning is graph search.

In Racklog we express this with two relations. `%move` relates a state,
an action, and the resulting state. It is the complete "physics" of
the monkey's world in seven clauses. `%canget` is the recursive
planner: from a goal state the plan is empty; from any other state the
plan is one legal move followed by a plan from the resulting state,
provided we have not visited that state before (the visited check is
what keeps depth-first search from walking in circles forever).

### The code

Here is the complete file `monkey.rkt`:

```racket
#lang racket

;; Monkey and bananas in Racklog (logic programming / backward chaining).
;; Same problem as ../OPS5_in_Racket/monkey.ops (production rules / forward
;; chaining): a monkey on the couch at 5-7 wants bananas on the ceiling at
;; 2-2, using a light ladder on the floor at 9-5.
;;
;; Run: racket monkey.rkt

(require racklog)

;; State: (state MonkeyAt MonkeyOn Holds LadderAt)
;; The bananas never move, so they are not part of the state.

(define %location (%rel () [('5-7)] [('2-2)] [('9-5)]))

;; %move: (StateBefore Action StateAfter)
(define %move
  (%rel (p q h l)
    ;; on the ladder under the bananas: grab them
    [((list 'state '2-2 'ladder 'nothing '2-2)
      (list 'grasp 'bananas)
      (list 'state '2-2 'ladder 'bananas '2-2))]
    ;; at the ladder with free hands: climb it
    [((list 'state p 'floor 'nothing p)
      (list 'climb 'ladder)
      (list 'state p 'ladder 'nothing p))]
    ;; the ladder is light: pick it up
    [((list 'state p 'floor 'nothing p)
      (list 'pick-up 'ladder)
      (list 'state p 'floor 'ladder p))]
    ;; put the ladder back down
    [((list 'state p 'floor 'ladder p)
      (list 'drop 'ladder)
      (list 'state p 'floor 'nothing p))]
    ;; carry the ladder somewhere else
    [((list 'state p 'floor 'ladder p)
      (list 'carry 'ladder 'to q)
      (list 'state q 'floor 'ladder q))
     (%location q) (%/= p q)]
    ;; walk somewhere else
    [((list 'state p 'floor 'nothing l)
      (list 'walk p 'to q)
      (list 'state q 'floor 'nothing l))
     (%location q) (%/= p q)]
    ;; jump down from whatever we are on
    [((list 'state p h 'nothing l)
      (list 'jump-down-from h)
      (list 'state p 'floor 'nothing l))
     (%/= h 'floor)]))

;; %canget: (State VisitedStates Plan) -- Plan is a list of actions.
;; Note: plain depth-first search, so the plan found is not guaranteed
;; shortest; add iterative deepening if that ever matters here.
(define %canget
  (%rel (s s2 action plan visited at on l-at)
    ;; goal state: monkey holds the bananas
    [((list 'state at on 'bananas l-at) visited '())]
    [(s visited (cons action plan))
     (%move s action s2)
     (%not (%member s2 visited))
     (%canget s2 (cons s visited) plan)]))

(define start '(state 5-7 couch nothing 9-5))

(printf "Monkey and bananas -- Racklog version")
(printf " (compare with ../OPS5_in_Racket/monkey.ops)\n\n")
(printf "Start: monkey on the couch at 5-7, ladder on the floor at 9-5,~n")
(printf "       bananas on the ceiling at 2-2.~n~n")

(define answer (%which (plan) (%canget start (list start) plan)))

(if answer
    (for ([step (cdr (assq 'plan answer))]
          [i (in-naturals 1)])
      (printf "~a. ~a~n" i step))
    (printf "no plan found~n"))
```

Read `%move` the way you would read a physics textbook: each clause is
one law. "If the monkey is at the ladder, on the floor, holding
nothing, then climbing the ladder leaves the monkey on the ladder."
Notice how much of each clause is just the *same* logic variable
appearing in the before-state and the after-state. The shared `p` in
the climb clause is what guarantees the monkey and the ladder are in
the same place, with no explicit equality test.

Read `%canget` as a two-line definition of "solvable":

1. A state where the monkey holds the bananas is solvable with the
   empty plan.
2. Any other state is solvable if some move leads to an unvisited state
   that is itself solvable. The plan is that move prepended to the
   plan for the new state.

That recursive clause is the entire search algorithm. `%not` and
`%member` (both built into Racklog) implement loop detection. When we
finally pose the query with `%which`, Racklog's backtracking search
does the rest, and the logic variable `plan` comes back bound to the
answer.

### Running it

```
$ racket monkey.rkt
Monkey and bananas -- Racklog version (compare with ../OPS5_in_Racket/monkey.ops)

Start: monkey on the couch at 5-7, ladder on the floor at 9-5,
       bananas on the ceiling at 2-2.

1. (jump-down-from couch)
2. (walk 5-7 to 2-2)
3. (walk 2-2 to 9-5)
4. (pick-up ladder)
5. (carry ladder to 5-7)
6. (carry ladder to 2-2)
7. (drop ladder)
8. (climb ladder)
9. (grasp bananas)
```

### Interpreting the output

The plan is correct: jump off the couch, fetch the ladder, carry it
under the bananas, climb, grasp. But look at steps 2–3 and 5–6: the
monkey walks to 2-2 *first* (presumably to gaze longingly at the
bananas), and later carries the ladder via 5-7. This is a valid plan,
not the shortest one.

That detour is a direct consequence of the search strategy, and seeing
it is worth more than a prettier answer. Racklog explores depth-first
in clause order: at each state it tries `grasp`, then `climb`, then
`pick-up`, then `drop`, then `carry`, then `walk`, then `jump-down`,
and within `walk` it tries destinations in the order `%location` lists
them. The first complete plan it stumbles into is the one printed. OPS5
had the opposite character: its conflict-resolution strategy picked
which rule to fire next, and the OPS5 author shaped the plan by writing
rules in a careful order; control lived in the rule set rather than in
a search procedure.

If shortest plans mattered, the fix would be iterative deepening (try
all plans of length 1, then 2, then 3), which guarantees the first
solution found is minimal. For this tiny state space (three locations,
a handful of `on`/`holds` combinations) plain depth-first search
answers instantly, so we keep the simpler program.

## Wrapping Up

Same problems, two tools, two philosophies:

| | OPS5 (previous chapter) | Racklog (this chapter) |
|---|---|---|
| Reasoning | forward chaining | backward chaining |
| Program is | rules over working memory | relations and queries |
| Goal is | implicit, emerges from firing | explicit, named in the query |
| Monkey output | narration of actions as they fire | a plan returned as data |
| Control lives in | rule order and conflict resolution | clause order and backtracking |

Neither is "better." Production rules shine when a system must react
continuously to arriving facts: monitoring, diagnosis, agents. Logic
programming shines when the question itself is the program: parsing,
planning, constraint solving, anything naturally phrased as "find an
`x`$ such that …". Racklog's particular charm is that the whole thing
is just Racket, so when the logic ends (printing a plan, formatting a
hand) you are back in an ordinary general-purpose language with no
seams.

## Exercises

1. Add a `%two-pair` relation to `draw.rkt` that finds two pairs at
   different ranks. Test it by adding a 4 of clubs to the hand.
2. In `monkey.rkt`, add a second ladder at 5-7. Does the plan change?
   Why or why not?
3. Replace `%which` with `%find-all` in `monkey.rkt` to enumerate every
   plan. Before running it, predict whether the result is finite.
4. Rewrite the `%suit<` relation using ranks (club = 1, diamond = 2,
   …) and Racklog's `%is` and `%<` predicates. Which version do you
   find clearer?
