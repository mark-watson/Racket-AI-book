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
;; ponytail: plain depth-first search, so the plan found is not guaranteed
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
