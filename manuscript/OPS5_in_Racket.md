# Implementing OPS5: A Forward-Chaining Production System in Racket

I converted OPS5 from Common Lisp to MIT Scheme in the early 1980s. The code for this chapter is that code, converted to run in Racket. Dear reader, even though I spent years of my life working on rule based symbolic AI, now I don’t really recommend rule based systems for practical work.

OPS5 was written by Charles Forgy (Carnegie Mellon University) and is the classic production-system language. The original Common Lisp code was “very hackable” and I modified it twice for projects at SAIC in the 1980s. I hope that Racket developers find the version converted to Racket to also be “hackable.”

An expert system shell is a program that runs rules over a set of facts. You write the rules. The shell decides which rules apply, picks one, fires it, and repeats.

The conversion does not change the algorithm. The interesting work is the algorithm itself: how a forward-chaining rule engine matches rule conditions against facts without scanning every fact on every cycle. The answer is the Rete algorithm, and it is the heart of this chapter.

## Production Systems and Forward Chaining

A production system has three parts:

1. **Working memory**: a set of facts. In OPS5 each fact is a tagged list, such as `(card heart 10)` or `(monkey ^ at 5-7 ^ on couch ^ holds nothing)`.
2. **Productions**: rules. Each production has a left-hand side (LHS), a pattern that matches working memory, and a right-hand side (RHS), actions that change working memory.
3. **The inference engine**: the loop that matches the LHS of every production against working memory, chooses one matching production, and runs its RHS.

Forward chaining means the engine works from facts to conclusions. It looks at what is true now, finds rules whose conditions hold, fires them, and lets the new facts trigger more rules. It runs until no rule matches or until a rule halts it. This is the opposite of backward chaining, which starts from a goal and asks which rules could prove it.

The engine repeats one cycle:

1. **Match**: find all productions whose LHS is satisfied by the current working memory. The set of satisfied instantiations is the **conflict set**.
2. **Resolve**: pick one instantiation from the conflict set.
3. **Act**: run that instantiation's RHS, which adds, removes, or changes facts.
4. **Repeat**, unless a halt fires or the conflict set is empty.

The hard part is step 1. A naive engine, on every cycle, tests every production against every combination of facts. If a production has several condition elements, the engine must try combinations of facts across those elements. The cost grows fast. We fix this with the Rete algorithm.

## The Rete Algorithm

Rete (Latin for "net") solves the match problem by storing partial matches in a network and updating them incrementally. When a fact enters or leaves working memory, the engine pushes that one change through the network instead of re-scanning everything.

The naive cost of matching is roughly `O(R \cdot W^{c})`$ per cycle, where `R` is the number of rules, `W` is the number of working memory elements, and `c` is the number of condition elements in a rule. Rete turns this into work proportional to the size of the change, not the size of memory.

The network has two halves.

**The alpha network** tests one condition element at a time. Each node checks one field of one fact against a constant or a variable binding: "is the suit equal to heart," "is the number greater than 7." A fact that passes all alpha tests for a condition element enters that element's alpha memory as a token.

**The beta network** joins tokens across condition elements. A beta node takes tokens from two parent memories and combines the ones that agree on their shared variables. The join tests live here: "the `<num>` bound in condition 1 equals the `<num>` bound in condition 2." Beta memories store the joined, partial matches so later joins reuse them.

Three extra node types matter:

- A **memory node** (`&mem`) holds tokens so downstream joins can read them.
- A **join node** (`&and`) combines a new token with the tokens in the opposite memory, running the inter-condition tests.
- A **negative node** (`&not`) handles a negated condition element, one prefixed with `-`. It fires only when no fact matches the negated pattern.

At the bottom of the network sits a **terminal node** (`&p`) for each production. When a full match reaches a `&p` node, the engine adds it to the conflict set. When a match is withdrawn, the engine removes it.

Rete also shares nodes across productions. If two rules test `(card <suit> <num>)`, they can share the alpha subnetwork for that pattern. The stats the engine prints separate **real** nodes from **virtual** nodes: virtual counts every node the compiler builds, real counts the unique ones after sharing. We will see that in this conversion the two counts come out equal, because the node-sharing lookup does not find matches.

## Conflict Resolution

Many productions may match at once. OPS5 picks one with a strategy. This code supports two.

- **LEX (lexicographic)**: among matching instantiations, prefer the one whose matched facts are most recent. If two share the same recency pattern, the engine breaks the tie by sorting the time tags of the matched facts and comparing the sorted lists element by element.
- **MEA (means-ends analysis)**: like LEX, but it first compares only the time tag of the first condition element. The first condition element acts as the goal condition and dominates the choice.

Both strategies also respect **refraction**: an instantiation that has already fired cannot fire again on the same facts. This stops a rule from firing forever on unchanged working memory. The engine records fired instantiations and rejects them when they reappear.

## The OPS5 Language

An OPS5 program is a sequence of forms. The core forms are `literalize`, `p`, `make`, and `run`.

`literalize` declares a class of fact and the attribute names it uses. This lets the compiler assign fixed field positions to attributes so a pattern can name a field by attribute instead of by position.

```scheme
(literalize card
  suit
  number)
```

A production is written with `p`:

```scheme
(p production-name
   condition-element
   condition-element
   -negated-condition-element
-->
   action
   action)
```

A condition element is a list. The first symbol is the class. Attributes follow the `^` marker, with spaces around it:

```scheme
(card ^ suit <suit> ^ number <num>)
```

Variables are atoms in angle brackets: `<suit>`, `<num>`. The first use of a variable binds it. Later uses test equality with the bound value, unless a predicate says otherwise. The predicates are `=`, `<>`, `<`, `<=`, `>`, `>=`, and `<=>`. For example `^ at <> <p>` means "the `at` attribute is not equal to the value bound to `<p>`."

Curly braces `{` and `}` mark a condition-element variable, which binds a whole fact so the RHS can refer to it. In this Scheme port the braces must be written as quoted strings, `"{"` and `"}"`, because the reader would otherwise treat them specially.

The RHS actions are `make`, `modify`, `ops-remove`, `ops-write`, `bind`, `cbind`, `halt`, and `compute`. `make` adds a fact. `modify` changes one field of an existing fact and is how the engine updates state. `ops-write` prints, with `(crlf)` for a newline. `halt` stops the run.

With the language in hand, we can read the two example programs that ship with the system.

## The Example Programs

The directory holds two `.ops` files. They are the data this system runs on, and they show two different uses of OPS5: one does pattern finding, the other does planning.

### draw.ops: finding pairs in a poker hand

This program looks at a hand of cards and finds pairs and three of a kind. The facts are simple: a goal flag and one fact per card. Here is the complete file.

```scheme
;; Sample OPS5 program for Draw

(i-g-v)

(p look-for-pairs
   (goal start)
   (card <suit> <num>)
  -(three)
   (card "{" <suit2> <> <suit> "}" <num>)
  -(pair <suit2> <suit> <num>)
-->
   (make pair <suit> <suit2> <num>)
   (ops-write (crlf) found a pair <suit> <num> <suit2>))

(p look-for-three-of-a-kind
   (goal start)
   (card <suit> <num>)
  -(four)
   (card "{" <suit2> <> <suit> "}" <num>)
   (card <suit3> "{" <> <suit2> "}" )
  -(three <any-suit1> <any-suit2> <any-suit3> "{" <num10> >= <num> "}" )
-->
   (make three <suit> <suit2> <suit3> <num>)
   (ops-write three of a kind <suit> <suit2> <suit3> <num>))

(make goal start)
(make card heart 10)
(make card diamond 10)
(make card club 10)
(make card diamond 4)
```

Read the first production, `look-for-pairs`. Its LHS needs five things, in order: a `goal` fact equal to `start`; a `card` fact that binds `<suit>` and `<num>`; the absence of any `three` fact (the `-` prefix negates the element); a second `card` whose suit is not equal to `<suit>` (the `<> <suit>` test) but whose number equals `<num>`; and the absence of a `pair` fact already recording this pair. The negated elements stop the rule from finding the same pair twice or repeating work a three-of-a-kind rule already covered.

When all five conditions hold, the RHS makes a `pair` fact and prints the find. The new `pair` fact feeds back into the negated condition, so that combination will not fire again. This feedback, plus refraction, is how the system converges to "no production true" and stops.

The second production, `look-for-three-of-a-kind`, extends the same idea to three cards. It binds three suits, tests that the second and third differ from the first and from each other, and guards against re-deriving a `three` that already exists.

The five `make` calls at the end seed working memory with the goal and the four cards. When you run the program, the rules fire and the hand gets analyzed.

### monkey.ops: the monkey and the bananas

The second program is the classic monkey-and-bananas planning problem. A monkey sits on a couch. Bananas hang on the ceiling out of reach. A light ladder lies on the floor elsewhere. The monkey must form a plan: get off the couch, walk to the ladder, pick it up, carry it under the bananas, climb it, and grab the bananas.

The program declares three classes with `literalize`:

```scheme
(literalize start )
(literalize monkey
	at
	on
	holds)

(literalize object
	name
	at
	weight
	on)

(literalize goal
	status
	type
	object
	to)
```

A `monkey` fact says where the monkey is (`at`), what it sits on (`on`), and what it holds (`holds`). An `object` fact names a thing, locates it, gives its weight, and says what it rests on. A `goal` fact drives the action: it has a `status` (active or satisfied), a `type` (holds, move, walk-to, on), the `object` it concerns, and a destination `to`.

The rules chain through subgoals. Here is the first production, which reacts to wanting something on the ceiling.

```scheme
(p mb1
	(goal ^ status active ^ type holds ^ object <w>)
	(object ^ name <w> ^ at <p> ^ on ceiling)
    -->
	(ops-write (crlf)  Since the <w> are on the ceiling at position <p> )
	(ops-write (crlf)  I would like to move  the ladder under them.)
	(make goal ^ status active ^ type move ^ object ladder ^ to <p>))
```

Read the LHS. It needs an active goal of type `holds` for some object `<w>`, and an object fact proving `<w>` is on the ceiling at position `<p>`. The RHS prints a line and makes a new active goal: move the ladder to `<p>`. That new goal triggers later rules.

The grabbing rule, `mb4`, is the payoff production. It fires when the monkey is on the ladder, under the bananas, with empty hands.

```scheme
(p mb4
	(goal ^ status active ^ type holds ^ object <w>)
	(object ^ name <w> ^ at <p> ^ on ceiling)
	(object ^ name ladder ^ at <p>)
	(monkey ^ on ladder ^ holds nil)
    -->
	(ops-write (crlf) I have the <w>  in hand)
		(modify 4 ^ holds <w>)
		(modify 1 ^ status satified))
```

The `(modify 4 ^ holds <w>)` action changes the fourth matched fact, the monkey fact, so the monkey now holds the bananas. The `(modify 1 ^ status satified)` action marks the goal satisfied. (The original source spells this `satified`; the code is unchanged from the historical program.)

The movement rules use the brace condition-element variable and inequality tests. This rule walks the monkey to a destination when it is on the floor, not already there, and holding nothing.

```scheme
(p mb12
	(goal ^ status active ^ type walk-to ^ object <p>)
	(monkey ^ on floor ^ at "{"  <c> <> <p>  "}"  ^ holds nothing)
    -->
	(ops-write (crlf) I will walk over to <p>)
		(modify 2 ^ at <p>)
		(modify 1 ^ status satisfied))
```

The `"{" <c> <> <p> "}"` part binds the monkey's current position to the whole-fact variable `<c>` while testing that it is not equal to the goal location `<p>`. The RHS moves the monkey and satisfies the walk goal.

A carrying rule, `mb13`, is the partner: if the monkey is holding something when it walks, the carried object moves with it. There are rules for jumping down to the floor (`mb14`), for needing free hands to climb (`mb16`, `mb17`), and for dropping a held object to free the hands (`mb18`). Each rule makes or satisfies a goal, and the goals drive the next rule.

The program ends with a starter production, `t1`, that creates the initial world when it sees a `start` fact.

```scheme
(p t1
	(start 1)
    -->
	(make monkey ^ at 5-7 ^ on couch ^ holds nothing)
	(ops-write (crlf) I am a monkey lying on the couch)
	(make object ^ name couch ^ at 5-7 ^ weight heavy)
	(ops-write (crlf)  "... a heavy couch")
	(make object ^ name bananas ^ on ceiling ^ at 2-2)
	(ops-write (crlf) there are some bananas on the ceiling at position 2-2)
	(make object ^ name ladder ^ on floor ^ at 9-5 ^ weight light)
	(ops-write (crlf) there is a ladder on the floor at position 9-5)

	(make goal ^ status active ^ type holds ^ object bananas)
	(ops-write (crlf) I sure would like those bananas )
	(ops-write (crlf) (crlf) "The action begins:" (crlf)))
```

The monkey starts at `5-7` on the couch. The bananas are on the ceiling at `2-2`. The ladder is on the floor at `9-5`. The first active goal is to hold the bananas. You trigger `t1` by making a `start` fact, and the plan unfolds from there.

The full `monkey.ops` file has 18 productions (`mb1` through `mb18`) plus the starter. Each follows the same shape as the three shown here: match a goal and some world facts, print a line, and make or satisfy a subgoal.

## The Racket Conversion: One File and a Namespace

The original OPS5-in-Scheme code was split across six files: a compatibility layer, the top-level commands, the LHS compiler, the Rete network, the RHS actions, and the literalize support. A driver loaded them in order into a namespace.

The Racket version keeps that structure but ships it as one file, `OPS5_all.rkt`. The six pieces sit inside a string, separated by section markers, and the driver writes that string to a temporary file and loads it into a dedicated namespace. Here is how the file sets that up.

```scheme
#lang racket
;; OPS5_all.rkt -- the complete OPS5-in-Racket system in one file.

(require racket/load)

(define ops5-namespace (make-base-namespace))

(define (load-file-into-ns! file)
  (parameterize ([current-namespace ops5-namespace])
    (load file)))

(define (load-string-into-ns! s)
  (define tmp (make-temporary-file "ops5-~a.rkt"))
  (call-with-output-file tmp
    (lambda (out) (display s out))
    #:exists 'replace)
  (load-file-into-ns! tmp)
  (delete-file tmp))

(define ops5-source #<<__OPS5_SOURCE_END__
;; ... the six source sections follow here (about 3600 lines, elided) ...
__OPS5_SOURCE_END__
)

(load-string-into-ns! ops5-source)
```

Why a separate namespace? The original code relies on sequential, top-level loading. It redefines names, mutates pairs in place, and uses `eval` to build and run forms at runtime. Loading into a fresh base namespace preserves those semantics and keeps the OPS5 names out of the Racket module that holds the driver.

Why load a temp file instead of evaluating the string form by form? The embedded source begins with `require` forms. Racket's module and load machinery processes those forms correctly only when it loads a file. Writing the string to a temp file and calling `load` gives the `require` forms the same treatment the old driver gave them.

### The compatibility layer

The first section inside the string is `compat.rkt`. It bridges MIT Scheme and Racket. The original code assumes mutable pairs, a `t` and `nil` that differ from Racket's `#t` and `'()`, and a set of list functions with lenient semantics. Racket's pairs are immutable, so the layer imports mutable pairs from the `r5rs` language.

```scheme
(require (except-in r5rs eval lambda)
                  )
(require (only-in r5rs
                  cons car cdr set-car! set-cdr! pair? null? list list? reverse
                  caar cadr cdar cddr
                  ;; ... about 30 more imported bindings elided ...
                  ))

(define t #t)
(define nil '())

(define (atom? x) (not (pair? x)))
(define proper-list? list?)
(define listp list?)
(define symbolp symbol?)
```

The list functions need lenient versions because the OPS5 code passes `#f` and atoms where Racket's strict versions would raise contract errors. Each lenient function is defined under an implementation name and then aliased, so the recursive calls inside the body bind to the lenient version, not to a previously imported strict one.

```scheme
(define (mapcar-impl f l)
  (cond ((null? l) '())
        ((pair? l) (cons (f (car l)) (mapcar-impl f (cdr l))))
        (else #f)))
(define mapcar mapcar-impl)

(define (member-impl x l)
  (cond ((pair? l) (if (equal? x (car l)) l (member-impl x (cdr l))))
        (else #f)))
(define member member-impl)
```

A `while` macro handles a subtle truthiness gap. MIT Scheme treats the empty list as false. Racket treats the empty list as true. So `while` must stop not only on `#f` but also on `'()`.

```scheme
(define (mit-true? x) (and x (not (null? x))))
(define-syntax-rule (while test body ...)
  (let loop () (when (mit-true? test) body ... (loop))))
```

This gap, between `'()` as false and `'()` as true, is the single most common source of bugs when porting old Scheme to Racket. The compatibility layer localizes the fix. You will see the same care in the compiler and network code, where comments note each place the original relied on MIT's empty-list-is-false rule.

## The Top-Level Commands as Macros

The user-facing OPS5 commands are `p`, `make`, `modify`, `run`, `wm`, and `strategy`. In the original code these were MIT `macro` forms. In Racket they become `define-syntax` transformers that build their expansions as data and quote the arguments. This preserves the key behavior: a production form reaches the compiler unevaluated, so the compiler sees the literal pattern.

```scheme
(define-syntax p
  (lambda (stx)
    (syntax-case stx ()
      [(_ . rest)
       (datum->syntax stx
         (list 'old-p (list 'quote (syntax->datum stx))))])))
```

The `p` macro rewrites `(p name lhs --> rhs)` into `(old-p '(p name lhs --> rhs))`. The whole production, unevaluated, goes to `old-p`. The other macros follow the same pattern. `make` quotes each argument and hands them to `old-make`.

```scheme
(define-syntax make
  (lambda (stx)
    (syntax-case stx ()
      [(_ . args)
       (datum->syntax stx
         (cons 'old-make
               (map (lambda (a) (list 'quote a))
                    (syntax->datum #'args))))])))

(define (old-make . l)
  (!reset)
  (eval-args l)
  (!assert))
```

`old-make` is the runtime half. It resets the result array that builds a new fact, evaluates the arguments into that array, and asserts the assembled fact into working memory. We will see `!reset`, `eval-args`, and `!assert` when we reach the RHS.

## Compiling a Production into the Network

The LHS compiler turns a production's pattern into Rete nodes. The entry point is `old-p`, which prints the production name, finishes any pending `literalize` declarations, and calls `compile-production`.

```scheme
(define (old-p z)
  (write (car z)) (newline)
  (set! z (cdr z))
  (finish-literalize)
  (write '*)
  (let ((flag nil) (temp nil))
    (set! temp (compile-production (car z) (cdr z)))
    (set! flag t)
    (display "compiled") (display  (car z))))
```

`compile-production` records the production name and calls `cmp-p`, the real compiler.

```scheme
(define (cmp-p name matrix)
  (let ((m nil) (bakptrs nil))
        (cond ((or (null? name) (proper-list? name))
	       (%error "Illegal production name" name)))
        (prepare-lex matrix)
        (excise-p name)
        (set! bakptrs nil)
        (set! *pcount* (+ 1 *pcount*))
        (set! *feature-count* 0)
        (set! *ce-count* 0)
        (set! *vars* nil)
        (set! *ce-vars* nil)
        (set! *rhs-bound-vars* nil)
        (set! *rhs-bound-ce-vars* nil)
        (set! *last-branch* nil)
        (set! m *matrix*)
        (while (not (equal? '--> (peek-lex)))
         (begin
          (and (atom? *matrix*) (%error "No '-->' in production" m))
          (cmp-prin)
          (set! bakptrs (cons *last-branch* bakptrs))))
        (lex)
        (check-rhs *matrix*)
        (link-new-node (list '&p
                             *feature-count*
                             name
                             (encode-dope)
                             (encode-ce-dope)
                             (eval (cons 'lambda (cons nil *matrix*)))))
        (putprop name (cdr (reverse bakptrs)) 'backpointers)
        (putprop name *last-node* 'topnode)))
```

Read the loop. It reads tokens until it hits `-->`. For each condition element it calls `cmp-prin`, which builds alpha test nodes for that element and, for every element after the first, a beta node that joins it to the previous ones. Each `*last-branch*` is the first node of one condition element's subnetwork; the list of these becomes the production's backpointers, used later by the `(matches)` debug command.

After `-->`, the compiler checks the RHS, then links a terminal `&p` node. That node carries the production name, the variable dope (which field each variable came from), the condition-element dope, and the RHS itself wrapped in a lambda. The lambda is the code the engine runs when this production fires.

The single-condition-element compiler, `cmp-ce`, reads the element and walks its fields. For each field it dispatches to a node builder based on what the field is.

```scheme
(define (cmp-element)
        (and (equal? (car *curcond*) '^) (cmp-tab))
        (cond ((equal? (car *curcond*) leftcurly) (cmp-product))
              (t (cmp-atomic-or-any))))

(define (cmp-atomic-or-any)
        (cond ((equal? (car *curcond*) '<<) (cmp-any))
              (t (cmp-atomic))))

(define (cmp-atomic)
  (let ((test nil) (x (car *curcond*)))
        (cond ((eq? x '=)   (set! test 'eq) (sublex))
              ((eq? x '<>)  (set! test 'ne) (sublex))
              ((eq? x '<)   (set! test 'lt) (sublex))
              ((eq? x '<=)  (set! test 'le) (sublex))
              ((eq? x '>)   (set! test 'gt) (sublex))
              ((eq? x '>=)  (set! test 'ge) (sublex))
              ((eq? x '<=>) (set! test 'xx) (sublex))
              (t (set! test 'eq)))
        (cmp-symbol test)))
```

A field with a predicate like `<>` sets the test type and reads on. Then `cmp-symbol` decides whether the field is a variable, a number, or a constant, and links the right alpha node. Constants and numbers become one-argument test nodes; variables become either a binding (first use) or a two-argument test node that compares two fields (later use).

The node names are built by `concat`, which packs the test type, the comparison kind, and the operand type into one symbol. `teqa` means "test equal atom," `tnea` means "test not-equal atom," `tnen` means "test not-equal number," and so on. The first letter group is the test, the middle is the operator, the last is the operand kind: `a` for atom, `n` for number, `s` for a field-to-field (same) test, `b` for a field-to-field beta test.

Node sharing happens in the linker. Before creating a node, the compiler checks the parent's existing children for an equivalent one.

```scheme
(define (link-left pred succ)
  (let ((a (left-outs pred)) (r nil))
        (set! r (find-equiv-node succ a))
        (if r
            r
            (begin
                (set! *real-cnt* (add1 *real-cnt*))
                (attach-left pred succ)
                succ))))
```

If an equivalent node already exists, the compiler reuses it and does not increment the real-node count. This is how two rules that share `(card <suit> <num>)` end up sharing the alpha nodes for that pattern.

## The Network Interpreter

When a fact enters or leaves working memory, the engine calls `match` to push the change into the network from the top.

```scheme
(define (match flag wme)
  (sendto flag (list wme) 'left (list *first-node*)))
```

`*first-node*` is a `&bus` node, the root that fans every fact out to all top-level alpha subnetworks. The `&bus` node unpacks the fact into the global field registers `*c1*`, `*c2*`, and so on, then evaluates its child nodes. Alpha test nodes read those registers.

A constant-equality test node is short. `teqa` reads its register, compares to the constant, and, if equal, forwards the fact to its outputs.

```scheme
(define (teqa outs register constant)
  (and (equal? (local-eval register) constant) (eval-nodelist outs)))
```

`local-eval` looks up a register symbol like `*c2*` in the current namespace to get the field value. When the test passes, `eval-nodelist` runs the child nodes, which are themselves functions stored as data in the node list.

A memory node stores the tokens that pass through it, so joins can read them later.

```scheme
(define (&mem left-outs right-outs memory-list)
  (let ((fp #f) (dp #f))
       (cond (*sendtocall*
              (set! fp *flag-part*)
              (set! dp *data-part*))
             (t
              (set! fp *alpha-flag-part*)
              (set! dp *alpha-data-part*)))
       (sendto fp dp 'left left-outs)
       (add-token memory-list fp dp #f)
       (sendto fp dp 'right right-outs)))
```

The flag part says whether this is an add (`new`) or a remove (`#f` or `old`). The data part is the token, the list of facts matched so far. `add-token` updates the memory list under that flag: `new` inserts, `#f` removes, `old` is a no-op marker. The node then forwards the token to both its left and right outputs.

The join node, `&and`, does the real beta work. It takes a new token on one side and scans the opposite memory for tokens that pass the inter-condition tests.

```scheme
(define (&and outs lpred rpred tests)
  (let ((mem #f))
       (cond ((eq? *side* 'right) (set! mem (memory-part lpred)))
             (t (set! mem (memory-part rpred))))
       (cond ((not mem) #f)
             ((eq? *side* 'right) (and-right outs mem tests))
             (t (and-left outs mem tests)))))
```

If the new token arrived on the right side, the join scans the left parent's memory, and vice versa. For each stored token, it runs the tests. A test is a beta predicate like `teqb` applied to two fields pulled from the two tokens by `gelm`, which decodes a packed (condition-element, field) index. When all tests pass, the join concatenates the two tokens into a longer one and forwards it downstream. That longer token is a fuller partial match. It flows to the next join, or to the terminal node.

The terminal `&p` node is where a complete match becomes a conflict-set entry.

```scheme
(define (&p rating name var-dope ce-var-dope rhs)
  (let ((fp #f) (dp #f))
        (cond (*sendtocall*
               (set! fp *flag-part*)
               (set! dp *data-part*))
              (t
               (set! fp *alpha-flag-part*)
               (set! dp *alpha-data-part*)))
        (and (memq fp '(#f old)) (removecs name dp))
        (and fp (insertcs name dp rating))))
```

On an add, `insertcs` adds the instantiation to the conflict set. On a remove, `removecs` takes it out. The instantiation is the production name plus the data part, the list of matched facts that will bind the RHS variables.

## Working Memory

Working memory is a hash of fact lists. The hash key is the first symbol in the fact, found by `wm-hash`, so facts cluster by class. Each fact carries a time tag, a number that records when it was added. Time tags drive LEX and MEA conflict resolution.

Adding a fact runs the network with the `new` flag and records the change for undo.

```scheme
(define (add-to-wm wme override)
  (let ((fa #f) (z #f) (part #f) (timetag #f) (port #f))
    (set! *critical* t)
    (set! *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (set! *max-wm* *current-wm*))
    (set! *action-count* (1+ *action-count*))
    (set! fa (wm-hash wme))
    (or (memq fa *wmpart-list*)
        (set! *wmpart-list* (cons fa *wmpart-list*)))
    (set! part (get fa 'wmpart*))
    (cond ((and override (not (null? override)))
             (set! timetag override))
          (t (set! timetag *action-count*)))
    (set! z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (set! *critical* #f)
    (cond ((and *in-rhs* *wtrace*)
           (newline)
           (write "Adding to WM: ")
           (write wme)
           (newline)))))
```

The fact gets a time tag (the current action count, unless an override is given, as in `refresh`). It is stored under its class key. Then `(match 'new wme)` pushes it into the Rete network, so every production that now matches it gets a conflict-set entry. Removing a fact does the reverse: it calls `(match #f wme)` to withdraw matches, then deletes the fact from its class list.

The `(wm)` command prints working memory by mapping over the class buckets.

```scheme
(define (old-wm a)
  (mapc (lambda (z) (ppelm z))
        (get-wm a)))

(define (get-wm z)
  (set! *wm-filter* z)
  (set! *wm* #f)
  (mapwm get-wm2)
  (let ((temp *wm*))
    (set! *wm* #f)
    temp))
```

Each fact prints as `(time-tag (fact))`. The optional argument filters by time tag. With no argument, `wm` prints every fact.

## The Recognize-Act Loop

The `run` command sets the cycle budget and calls `do-continue`, which processes pending changes and calls `main`.

```scheme
(define (old-run z)
  (set! *remaining-cycles* z)
  (do-continue #f))

(define (do-continue wmi)
    (cond (*critical*
           (newline)
           (write "Warning: network may be inconsistent")))
    (process-changes wmi #f)
    (print-times (main)))
```

`main` is the cycle. It picks an instantiation, fires it, and loops.

```scheme
(define (main)
  (let ((instance #f) (r #f))

     (define (loop)
        (set! *phase* 'conflict-resolution)
	       (cond ((and #f (equal? (peek-char) 13))  ;; skip this logic because of #F clause
               (set! *halt-flag* t)
               (set!  *break-flag* t)
               (read-char)
               (newline)
               (display "Interrupted by a keystroke")
               (newline))
              (t
                (cond (*halt-flag*
                       (set! r "End -- explicit halt")
                       (finis))
                      ((zero? *remaining-cycles*)
                       (set! r "***break***")
                       (set! *break-flag* t)
                       (finis))
                      (*break-flag*
                       (set! r "***break***")
                       (finis))
                      (t
                        (set! *remaining-cycles* (-1+ *remaining-cycles*))
                        (set! instance (conflict-resolution))
                        (cond ((not instance)
                               (set! r "End -- no production true")
                               (finis))
                              (t
                                (set! *phase* (car instance))
                                (accum-stats)
                                (eval-rhs (car instance) (cdr instance))
                                (check-limits)
                                (and
                                 (broken (car instance))
                                 (set! *break-flag* t))
                                (loop))))))))
  (define (finis)
     (set! *p-name* #f)
     r)

  (set! *halt-flag* #f)
  (set! *break-flag* #f)
  (set! instance #f)
  (loop)))
```

Each iteration calls `conflict-resolution` to pick the best instantiation. If there is none, the loop ends with "End -- no production true." If a halt flag is set, it ends with "End -- explicit halt." Otherwise it runs the RHS and loops.

Conflict resolution is a tournament. `best-of` walks the conflict set and keeps the winner under the strategy's comparison.

```scheme
(define (conflict-resolution)
  (let ((best #f) (len (length *conflict-set*)) (temp #f))
    (cond ((> len *max-cs*) (set! *max-cs* len)))
    (set! *total-cs* (+ *total-cs* len))
    (cond ((pair? *conflict-set*)
           (set! best (best-of *conflict-set*))
           (set! *conflict-set* (delq best *conflict-set*))
           (set! temp (pname-instantiation best)))
          (t temp #f))
    temp))
```

Each entry in the conflict set is `((p-name . data) (sorted time tags) rating)`. The `order-tags` function builds the sorted time-tag list, and it differs by strategy.

```scheme
(define (order-tags dat)
  (let ((tags #f))
    (while (and (not (atom? dat)) (not (null? dat)))
       (begin
           (set! tags (cons (creation-time (safe-car dat)) tags))
           (set! dat (cdr dat))))
    (cond ((eq? *strategy* 'mea)
           (cons (safe-car tags) (dsort (safe-cdr tags))))
          (t (dsort tags)))))
```

Under LEX, all time tags are sorted and compared as a list. Under MEA, the first condition element's time tag is pulled out and compared first, and only the rest are sorted for tie-breaking. That first tag is the goal condition's tag, so MEA favors the instantiation whose goal fact is newest. The chosen entry is removed from the conflict set so it will not be picked again this pass; refraction will keep it out if the facts have not changed.

## Running the RHS

When an instantiation fires, `eval-rhs` binds the matched facts to their variables and runs the production's RHS lambda.

```scheme
(define (eval-rhs pname data)
  (let ((node nil) (port nil) (eval-expression nil))
    (cond (*ptrace*
            (newline) (display *cycle-count*) (display ". ")
            (display pname) (time-tag-print data)))
    (set! *data-matched* data)
    (set! *p-name* pname)
    (set! *last* nil)
    (set! node (get pname 'topnode))
    (init-var-mem (cadddr node))
    (init-ce-var-mem (cadr (cdddr node)))
    (begin-record pname data)
    (set! *in-rhs* t)
    (set! eval-expression (caddr (cdddr node)))
    (eval-expression)
    (set! *in-rhs* nil)
    (end-record)))
```

`init-var-mem` reads the variable dope and builds an association list mapping each variable to the field value pulled from the matched data. The RHS lambda then runs. Inside it, `!varbind` looks up variables in that association list. `*in-rhs*` is set true so that `make`, `modify`, and `ops-write` know they are running inside a firing, not at the top level.

A `make` action assembles a new fact in a result array and asserts it. `!value` fills the next slot; `!assert` turns the array into a list and adds it to working memory.

```scheme
(define (!value v)
  (cond ((> *next-index* *size-result-array*)
         (%warn "Index too large" *next-index*))
        (t
         (and (> *next-index* *max-index*)
              (set! *max-index* *next-index*))
         (putvector *result-array* *next-index* v)
         (set! *next-index* (add1 *next-index*)))))

(define (!assert)
  (set! *last* (use-result-array))
  (add-to-wm *last* nil))
```

A `modify` action reads the bound condition-element fact, removes it from working memory, copies its fields into the result array, applies the changes, and asserts the result. This remove-then-add is what makes `modify` propagate through the network: the removal withdraws old matches, and the add creates new ones. That propagation is why firing one rule can trigger the next.

## Running the Code

You need Racket. No packages are required; the code uses only the standard library and the `r5rs` language that ships with Racket.

Run the driver from the example directory. Give it a `.ops` file to load before the REPL starts.

```
racket OPS5_all.rkt draw.ops
```

The system prints its banner and the compiled production names, then drops you at the `OPS5>` prompt. Type `(run)` to fire the productions.

### draw.ops

```
$ racket OPS5_all.rkt draw.ops
******* Beta test of OPS5 *******
Note: the Scheme version of OPS5 requires curly brakets { and }
to have surrounding double quotes.  Place spaces around the ^tab character.
Copyright 1986, Mark Watson
p
*compiledlook-for-pairsp
*compiledlook-for-three-of-a-kind
OPS5 Scheme interpreter (Racket conversion)
Type OPS5 expressions, e.g.:
  (load "draw.ops")   load a program file
  (i-g-v)              initialize (or reset) OPS5
  (p name lhs --> rhs) define a production
  (make class ...)     add a working-memory element
  (run)                run the productions
  (wm)                 print working memory
  (exit)               leave the REPL

OPS5> (run)
 three of a kind heart diamond diamond 10 three of a kind diamond heart diamond 10
three of a kind diamond club diamond 10 three of a kind heart club diamond 10
three of a kind club diamond diamond 10 three of a kind club heart diamond 10
found a pair club 10 heart
found a pair club 10 diamond
found a pair diamond 10 club
...
found a pair diamond 10 heart
End -- no production true

(2 productions (42 // 42 nodes))(28 firings (33 RHS actions))
(18 Mean working memory size (33 maximum))
(16 mean conflict set size (30 maximum))
(98 mean token memory size (123 maximum))OPS5>
```

Before running, you can inspect the facts with `(wm)`.

```
OPS5> (wm)
(1 (goal start))
(2 (card heart 10))
(3 (card diamond 10))
(4 (card club 10))
(5 (card diamond 4))
```

The four cards are three tens and a four. The two rules find the pairs among the tens and announce three of a kind in several suit orderings. The output is noisy because the three-of-a-kind rule fires for each ordering of the three suits, and the pair rule fires for each ordering of each pair. The negated guards and refraction eventually stop every rule, and the engine ends with "End -- no production true."

### monkey.ops

Start the monkey program and trigger the starter production with `(make start 1)`.

```
$ racket OPS5_all.rkt monkey.ops
... banner and 19 compiled production names ...

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
since the ladder is light I can move it
I think I will walk over to 9-5 to get the ladder
since I need to be on the floor to walk
I better get to the floor
I will jump onto the floor
I will walk over to 9-5
I picked the ladder off the floor
since I can move the ladder to 2-2 I will
I will carry ladder to 2-2
With the ladder at 2-2
I climb onto the ladder to get the bananas .
I will need free hands to climb the ladder
and it is where I want it
since I need my hands free I will put ladder down
I will now climb onto ladder
what I want to do now is get the bananas
End -- no production true

(19 productions (212 // 212 nodes))(16 firings (42 RHS actions))
(10 Mean working memory size (14 maximum))
(2 mean conflict set size (3 maximum))
(48 mean token memory size (60 maximum))OPS5>
```

Run each example in a fresh session. The README warns about two quirks. `(i-g-v)` excises all loaded productions, so after a reset you must reload the `.ops` file before `(run)`. And loading a second `.ops` file without resetting keeps the old working memory, so rules can fire against stale facts. The simple habit is to `(exit)` and relaunch with the next file.

## Interpreting the Results

The monkey trace is a plan. Read it top to bottom. The monkey wants the bananas. Because they are on the ceiling, it decides to move the ladder under them. Because the ladder is light, it can move it, but first it must pick it up, so it decides to walk to the ladder. To walk it must be on the floor, so it jumps off the couch. It walks to the ladder, picks it up, carries it to `2-2`, and climbs it. The last firing, `mb3`, sets a sub-goal to free the monkey's hands before the grab. No later rule satisfies that sub-goal in this setup, so the engine stops one step short of the grab and ends with no production true. Each line is one production firing, and the chain of goals drives the next line. This is forward chaining producing goal-directed behavior: there is no search tree and no backtracking. The goal facts and the rule order do the work, and when no rule matches the current goal, the run stops.

The final line of each run is a statistics report. It has four parts.

```
(19 productions (212 // 212 nodes))(16 firings (42 RHS actions))
(10 Mean working memory size (14 maximum))
(2 mean conflict set size (3 maximum))
(48 mean token memory size (60 maximum))
```

- **Productions and nodes**: `19 productions` fired here. `212 // 212 nodes` gives virtual nodes and real nodes. Virtual is every node the compiler built. Real is the unique nodes after sharing. They are equal in this program because no two productions share enough to merge nodes. In a larger program with shared patterns, real would be smaller than virtual.
- **Firings and RHS actions**: `16 firings` means 16 productions fired. `42 RHS actions` counts the individual `make`, `modify`, `ops-write`, and other actions those firings ran. One firing can run several actions, so the action count is higher than the firing count.
- **Working memory size**: `10 Mean working memory size (14 maximum)`. On average 10 facts were in memory across cycles, and at peak there were 14. The monkey program keeps a small world, so these numbers stay low.
- **Conflict set size**: `2 mean conflict set size (3 maximum)`. On average 2 instantiations competed each cycle, peaking at 3. A small conflict set means the strategy had little to choose from, which fits a planning program that mostly has one sensible next step.
- **Token memory size**: `48 mean token memory size (60 maximum)`. Tokens are the partial matches stored in beta memories. This is the memory cost of Rete. It is larger than the working memory size because one fact can participate in many partial matches.

Compare the two programs. The draw program has a large, noisy conflict set (mean 16, max 30) because many suit orderings match at once. The monkey program has a small one (mean 2, max 3) because the goal chain keeps the choices tight. The token memory tells the same story: draw averages 98 tokens, monkey averages 48, even though monkey has far more productions. More rules do not mean more memory when the working memory stays small and the goals channel the matching.

## Wrap Up

This chapter walked through a working OPS5 implementation in Racket. The core ideas are independent of the host language.

A production system keeps facts in working memory and runs rules in a match-resolve-act loop. Forward chaining fires rules whose conditions hold and lets the new facts trigger more rules. The cost of matching is the hard problem, and the Rete algorithm solves it by compiling each rule's pattern into a network of alpha and beta nodes that store partial matches and update incrementally. Conflict resolution picks one instantiation per cycle, using recency (LEX) or a goal-first variant (MEA), and refraction stops repeats. The RHS assembles new facts in a result array and asserts them, and `modify` does a remove-then-add that pushes changes back through the network.

The Racket conversion did not change any of this. It ported the MIT Scheme dialect to Racket with a compatibility layer that supplies mutable pairs, lenient list functions, and the empty-list-is-false truthiness the original code assumes. It wrapped the six source sections in one file and loaded them into a dedicated namespace to preserve the sequential loading and runtime `eval` the code relies on. The user-facing commands became Racket macros that pass forms to the compiler unevaluated.

The two examples show the range. `draw.ops` uses OPS5 for pattern finding over a small fixed set of facts, and the noisy output shows what happens when many orderings match. `monkey.ops` uses OPS5 for planning, where a chain of goal facts drives a sequence of actions from a couch toward a bunch of bananas, stopping one step short of the grab. The statistics line in each run ties the behavior back to the algorithm: node counts reflect sharing, the conflict set reflects how much the strategy had to choose from, and token memory reflects the cost Rete pays to keep matching cheap.

## Optional Practice Problems

These exercises build on the example code in this directory. Run each in a fresh session with `racket OPS5_all.rkt your-file.ops`.

1. **Four of a kind.** The `draw.ops` program finds pairs and three of a kind but stops there. Add a production `look-for-four-of-a-kind` that fires when four cards share a number. Seed working memory with four cards of the same number and run it. Use the existing `look-for-three-of-a-kind` production as a template, and add a `-(four)` guard to the three-of-a-kind rule so the new rule takes over first.

2. **Detect a flush.** Write a production that fires when all cards in the hand share one suit. Add a `(make card heart 9)` to the seed data so the hand has four hearts, and confirm the flush rule fires. Think about how to test "every card has suit `<s>`" with the condition elements and variables OPS5 gives you.

3. **Fix the grab.** In `monkey.ops`, the run stops at `mb3` with the monkey on the ladder under the bananas, but `mb4` never fires, so the monkey never grabs them. Read `mb3` and `mb4`: `mb3` makes a goal of type `holds` with `object nil`, while `mb4` needs `object <w>` bound to a real object. Explain why `mb4` does not match after `mb3`. Then change `mb3` or add a production so the monkey grabs the bananas, and add a `(halt)` action so the run ends with "End -- explicit halt."

4. **Change the strategy.** The default strategy is LEX. Add `(strategy mea)` near the top of `monkey.ops` after `(i-g-v)`, run the monkey program, and compare the firing order and the trace to the LEX run. Report which lines change order and why the first condition element's time tag matters.

5. **A heavy ladder.** The ladder is `weight light`, which is why `mb8` can move it. Change the seed in `t1` so the ladder is `weight heavy`. Predict what happens, run it, and explain why the plan stalls. Then add a new object and production that lets the monkey move a heavy object with a tool of your invention.

6. **Blocks world.** Write a new `.ops` file for a simple blocks world. Declare a `block` class with `name`, `on`, and `clear` attributes. Seed three blocks stacked on a table. Write productions that move the top clear block onto the table or onto another clear block, driven by a goal fact. Run it and confirm the stack unstacks.

7. **Thermostat.** Write a `.ops` file that models a thermostat. Declare a `temperature` class and a `setting` class. Seed a current temperature and a desired setting. Write one production that makes a `turn-on-heat` fact when the temperature is below the setting, and one that makes a `turn-off-heat` fact when it is at or above. Run it and inspect working memory with `(wm)`.

8. **Read the stats.** Add several duplicate conditions across two productions in `draw.ops` so that node sharing kicks in. Run it and read the `virtual // real` node counts from the statistics line. Confirm that real is smaller than virtual, and explain which nodes were shared.

9. **Trace a firing.** Turn on production tracing with `(watch 1)` before `(run)` in the monkey program. The engine prints each firing with its cycle number and time tags. Pick one firing from the trace, identify which production fired and which facts matched, and explain why the conflict resolver chose that instantiation over any competitor in the same cycle.

10. **Negation.** The `draw.ops` pair rule uses `-(pair ...)` to avoid re-deriving a pair. Remove that negated condition, run the program, and observe what happens to the firing count and the conflict set size. Explain how refraction alone does or does not prevent the explosion.
