#lang racket
;; play.rkt -- trick taking rules and heuristic card play for the AI players.
;;
;; A trick is a list of cards in PLAY ORDER (leader's card first).
;; TRUMP is a suit 0..3, or #f for notrump.

(require "cards.rkt")
(provide (all-defined-out))

;; Must follow the led suit when possible
(define (legal-plays hand lead-suit)
  (define in-suit (if lead-suit (hand-suit-cards hand lead-suit) '()))
  (if (null? in-suit) hand in-suit))

(define (players-from leader)
  (list leader (next-player leader)
        (next-player (next-player leader))
        (next-player (next-player (next-player leader)))))

;; Trumps beat the led suit, which beats everything else
(define (card-score c lead-suit trump)
  (cond [(and trump (= (card-suit c) trump)) (+ 100 (card-rank c))]
        [(= (card-suit c) lead-suit) (+ 50 (card-rank c))]
        [else (card-rank c)]))

(define (best-score trick trump)
  (define lead-suit (card-suit (car trick)))
  (apply max (map (lambda (c) (card-score c lead-suit trump)) trick)))

(define (trick-winner trick leader trump)
  (if (null? trick)
      leader
      (let ([lead-suit (card-suit (car trick))])
        (cdr (argmax (lambda (pc) (card-score (car pc) lead-suit trump))
                     (map cons trick (take (players-from leader) (length trick))))))))

(define (lowest-card cards) (argmin card-rank cards))
(define (highest-card cards) (argmax card-rank cards))

(define (partner-winning? trick leader trump me)
  (and (pair? trick) (= (side (trick-winner trick leader trump)) (side me))))

;; Cards from IN-SUIT that would currently win the trick
(define (winning-cards in-suit trick trump)
  (if (null? trick)
      in-suit
      (let ([lead-suit (card-suit (car trick))]
            [to-beat (best-score trick trump)])
        (filter (lambda (c) (> (card-score c lead-suit trump) to-beat)) in-suit))))

;; Play the cheapest card that wins, otherwise the lowest card
(define (cheapest-winner in-suit trick trump)
  (define winners (winning-cards in-suit trick trump))
  (lowest-card (if (null? winners) in-suit winners)))

(define (longest-non-trump hand trump)
  (longest-suit-of hand (if trump (remove trump all-suits) all-suits)))

;; Discard the lowest card of the longest side suit
(define (best-discard cards trump)
  (define in-longest (hand-suit-cards cards (longest-non-trump cards trump)))
  (lowest-card (if (null? in-longest) cards in-longest)))

;; ---------------------------------------------------------------------------
;; Opening leads
;; ---------------------------------------------------------------------------

(define (high-to-low cards) (sort cards > #:key card-rank))

;; Top of a three card sequence headed by the jack or better (e.g. KQJ)
(define (lead-top-of-sequence hand trump)
  (for/first ([s (if trump (remove trump all-suits) all-suits)]
              #:when (match (high-to-low (hand-suit-cards hand s))
                       [(list* c1 c2 c3 _)
                        (and (>= (card-rank c1) JACK)
                             (= (card-rank c1) (add1 (card-rank c2)))
                             (= (card-rank c2) (add1 (card-rank c3))))]
                       [_ #f]))
    (car (high-to-low (hand-suit-cards hand s)))))

;; Fourth highest card of the longest side suit
(define (lead-fourth-best hand trump)
  (define cards (high-to-low (hand-suit-cards hand (longest-non-trump hand trump))))
  (and (>= (length cards) 4) (list-ref cards 3)))

;; Top card of the shortest side suit, hoping for a ruff later
(define (lead-short-suit hand trump)
  (and trump
       (let ([suits (filter (lambda (s) (positive? (suit-length s hand)))
                            (remove trump all-suits))])
         (and (pair? suits)
              (highest-card (hand-suit-cards hand (argmin (lambda (s) (suit-length s hand))
                                                          suits)))))))

(define (ai-select-lead hand trump me declarer)
  (define defending? (not (= (side me) (side declarer))))
  (define trumps (if trump (hand-suit-cards hand trump) '()))
  (cond
    [defending?
     (or (lead-top-of-sequence hand trump)
         (lead-fourth-best hand trump)
         (lead-short-suit hand trump)
         (best-discard hand trump))]
    ;; Declarer side: draw trumps when holding three or more including the queen+
    [(and (>= (length trumps) 3) (ormap (lambda (c) (>= (card-rank c) QUEEN)) trumps))
     (highest-card trumps)]
    [else
     (or (lead-top-of-sequence hand trump)
         (lead-fourth-best hand trump)
         (car hand))]))

;; ---------------------------------------------------------------------------
;; Following to a trick
;; ---------------------------------------------------------------------------

(define (ai-select-follow hand trick leader trump me)
  (define lead-suit (card-suit (car trick)))
  (define legal (legal-plays hand lead-suit))
  (define partner-wins? (partner-winning? trick leader trump me))
  (cond
    [(= (length legal) 1) (car legal)]
    [(= (card-suit (car legal)) lead-suit)
     ;; Fourth hand ducks when partner already has the trick won
     (if (and (= (length trick) 3) partner-wins?)
         (lowest-card legal)
         (cheapest-winner legal trick trump))]
    [else
     ;; Void in the led suit: ruff if partner isn't winning, else discard
     (define trumps (if trump (hand-suit-cards legal trump) '()))
     (define others (if trump (filter (lambda (c) (not (= (card-suit c) trump))) legal) legal))
     (cond [(and (not partner-wins?) (pair? trumps))
            (let ([ruffs (winning-cards trumps trick trump)])
              (if (pair? ruffs) (lowest-card ruffs) (best-discard (if (pair? others) others legal) trump)))]
           [else (best-discard (if (pair? others) others legal) trump)])]))

;; Main entry point: HAND is the player's cards, TRICK the cards played so far
(define (ai-select-card hand trick leader trump me declarer)
  (define choice
    (if (null? trick)
        (ai-select-lead hand trump me declarer)
        (ai-select-follow hand trick leader trump me)))
  (define legal (legal-plays hand (and (pair? trick) (card-suit (car trick)))))
  (if (member choice legal) choice (car legal)))
