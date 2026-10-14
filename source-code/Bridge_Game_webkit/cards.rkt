#lang racket
;; cards.rkt -- players, cards, deck, and hand evaluation for contract bridge.
;;
;; Suits are the integers 0..3 (clubs, diamonds, hearts, spades) so that
;; suit order comparisons are just numeric comparisons. Ranks are 2..14
;; (14 = Ace). Players are the symbols North, East, South, West.

(provide (all-defined-out))

;; ---------------------------------------------------------------------------
;; Players
;; ---------------------------------------------------------------------------

(define players '(North East South West))

(define (partner p)
  (case p [(North) 'South] [(South) 'North] [(East) 'West] [(West) 'East]))

(define (next-player p)
  (case p [(North) 'East] [(East) 'South] [(South) 'West] [(West) 'North]))

;; Side 0 is North-South, side 1 is East-West
(define (side p) (if (memq p '(North South)) 0 1))

;; ---------------------------------------------------------------------------
;; Suits, ranks and cards
;; ---------------------------------------------------------------------------

(define CLUBS 0)
(define DIAMONDS 1)
(define HEARTS 2)
(define SPADES 3)
(define all-suits '(0 1 2 3))

(define JACK 11)
(define QUEEN 12)
(define KING 13)
(define ACE 14)

(struct card (suit rank) #:transparent)

(define (suit-letter s) (string-ref "CDHS" s))
(define (suit-name s) (vector-ref #("Clubs" "Diamonds" "Hearts" "Spades") s))

(define (rank->string r)
  (case r [(14) "A"] [(13) "K"] [(12) "Q"] [(11) "J"] [else (number->string r)]))

;; Card codes used by the UI: "AS", "10D", "7C", ...
(define (card->string c)
  (string-append (rank->string (card-rank c)) (string (suit-letter (card-suit c)))))

(define (string->card str)
  (define s (string-upcase (string-trim str)))
  (define n (string-length s))
  (and (>= n 2)
       (let ([suit (case (string-ref s (sub1 n))
                     [(#\C) CLUBS] [(#\D) DIAMONDS] [(#\H) HEARTS] [(#\S) SPADES]
                     [else #f])]
             [rank (match (substring s 0 (sub1 n))
                     ["A" ACE] ["K" KING] ["Q" QUEEN] ["J" JACK]
                     [r (let ([v (string->number r)]) (and v (<= 2 v 10) v))])])
         (and suit rank (card suit rank)))))

;; ---------------------------------------------------------------------------
;; Deck and dealing
;; ---------------------------------------------------------------------------

(define (make-deck)
  (for*/list ([s all-suits] [r (in-range 2 15)]) (card s r)))

;; Sort for display: spades first, high cards first within a suit
(define (sort-hand cards)
  (sort cards (lambda (a b)
                (or (> (card-suit a) (card-suit b))
                    (and (= (card-suit a) (card-suit b))
                         (> (card-rank a) (card-rank b)))))))

;; Deal a 52-card deck round robin. Returns an immutable hash player -> hand.
(define (deal-hands deck)
  (for/hash ([p players] [i (in-naturals)])
    (values p (sort-hand (for/list ([c deck] [j (in-naturals)] #:when (= (modulo j 4) i)) c)))))

;; ---------------------------------------------------------------------------
;; Hand evaluation
;; ---------------------------------------------------------------------------

(define (card-hcp c)
  (case (card-rank c) [(14) 4] [(13) 3] [(12) 2] [(11) 1] [else 0]))

(define (hand-hcp hand) (for/sum ([c hand]) (card-hcp c)))

(define (hand-suit-cards hand suit)
  (filter (lambda (c) (= (card-suit c) suit)) hand))

(define (suit-length suit hand) (length (hand-suit-cards hand suit)))

;; Suit lengths sorted longest first, e.g. '(5 3 3 2)
(define (hand-shape-sorted hand)
  (sort (for/list ([s all-suits]) (suit-length s hand)) >))

(define (balanced? hand)
  (and (member (hand-shape-sorted hand) '((4 3 3 3) (4 4 3 2) (5 3 3 2))) #t))

(define (hand-has-major? hand min-len)
  (or (>= (suit-length HEARTS hand) min-len)
      (>= (suit-length SPADES hand) min-len)))

;; Longest suit among SUITS; ties go to the higher-ranking suit
(define (longest-suit-of hand suits)
  (for/fold ([best #f]) ([s (sort suits >)])
    (if (or (not best) (> (suit-length s hand) (suit-length best hand))) s best)))

(define (hand-longest-suit hand) (longest-suit-of hand all-suits))

;; Number of cards ten or higher in a suit
(define (suit-honor-count hand suit)
  (count (lambda (c) (>= (card-rank c) 10)) (hand-suit-cards hand suit)))
