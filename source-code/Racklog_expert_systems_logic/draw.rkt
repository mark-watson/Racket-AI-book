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
