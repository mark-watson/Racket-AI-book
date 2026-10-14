#lang racket
;; engine.rkt -- the deal state machine: dealing, the auction, and card play.
;;
;; game is an immutable struct; apply-bid and apply-card-play return a new
;; game. Phases: 'Bidding -> 'Playing -> 'Scoring, or 'Done for a passed-out deal.

(require "cards.rkt" "bidding.rkt" "play.rkt")
(provide (all-defined-out))

(struct game
  (hands            ; immutable hash: player -> list of cards
   original-hands
   dealer
   vulnerability    ; "None", "NsOnly", "EwOnly" or "Both"
   human            ; the seat the person plays (always South here)
   bid-history      ; newest first: (cons player bid)
   contract         ; suit-bid or #f
   declarer
   doubled          ; 0, 1 or 2
   dummy
   current-trick    ; newest first: (cons player card)
   trick-lead       ; who led (or will lead) the current trick
   tricks-ns
   tricks-ew
   tricks-played
   trump            ; suit 0..3 or #f for notrump
   phase)
  #:transparent)

(define (new-game dealer vulnerability human [deck (shuffle (make-deck))])
  (define hands (deal-hands deck))
  (game hands hands dealer vulnerability human
        '() #f #f 0 #f '() dealer 0 0 0 #f 'Bidding))

(define (hand-of g p) (hash-ref (game-hands g) p))

;; Whose turn is it? #f once the deal is over.
(define (current-actor g)
  (case (game-phase g)
    [(Bidding) (match (game-bid-history g)
                 ['() (game-dealer g)]
                 [(cons (cons who _) _) (next-player who)])]
    [(Playing) (match (game-current-trick g)
                 ['() (game-trick-lead g)]
                 [(cons (cons who _) _) (next-player who)])]
    [else #f]))

;; Cards of the current trick in play order
(define (trick-cards g) (map cdr (reverse (game-current-trick g))))

(define (lead-suit g)
  (match (trick-cards g) ['() #f] [(cons c _) (card-suit c)]))

;; The human plays South, and also North when North is dummy for N-S
(define (human-controls? g p)
  (or (eq? p (game-human g))
      (and (eq? p (game-dummy g))
           (game-declarer g)
           (= (side (game-declarer g)) (side (game-human g))))))

(define (legal-cards-for g p) (legal-plays (hand-of g p) (lead-suit g)))

;; ---------------------------------------------------------------------------
;; Bidding
;; ---------------------------------------------------------------------------

(define (apply-bid b g)
  (define actor (current-actor g))
  (define history (cons (cons actor b) (game-bid-history g)))
  (cond
    [(not (bidding-complete? history)) (struct-copy game g [bid-history history])]
    [(extract-contract history)
     => (match-lambda
          [(list contract declarer doubled)
           ;; When partner North declares, swap the North and South hands so the
           ;; person sits in the declarer's chair and North becomes dummy.
           (define swap? (and (eq? declarer (partner (game-human g)))
                              (eq? (game-human g) 'South)))
           (define hands
             (if swap?
                 (hash-set* (game-hands g)
                            'North (hand-of g 'South)
                            'South (hand-of g 'North))
                 (game-hands g)))
           (define eff-declarer (if swap? 'South declarer))
           (define strain (suit-bid-strain contract))
           (struct-copy game g
                        [hands hands]
                        [bid-history history]
                        [contract contract]
                        [declarer eff-declarer]
                        [doubled doubled]
                        [dummy (partner eff-declarer)]
                        [trump (and (< strain NT) strain)]
                        [trick-lead (next-player eff-declarer)]
                        [phase 'Playing])])]
    [else (struct-copy game g [bid-history history] [phase 'Done])]))

;; Let the computer bid for every seat until it is the human's turn
(define (run-ai-bidding g)
  (define actor (current-actor g))
  (if (and (eq? (game-phase g) 'Bidding) (not (eq? actor (game-human g))))
      (run-ai-bidding
       (apply-bid (ai-select-bid (hand-of g actor) (game-bid-history g) actor) g))
      g))

;; ---------------------------------------------------------------------------
;; Card play
;; ---------------------------------------------------------------------------

(define (apply-card-play c g)
  (define actor (current-actor g))
  (define hands (hash-update (game-hands g) actor (lambda (h) (remove c h))))
  (define trick (cons (cons actor c) (game-current-trick g)))
  (cond
    [(< (length trick) 4)
     (struct-copy game g [hands hands] [current-trick trick])]
    [else
     (define winner (trick-winner (map cdr (reverse trick)) (game-trick-lead g) (game-trump g)))
     (define ns? (= (side winner) 0))
     (define played (add1 (game-tricks-played g)))
     (struct-copy game g
                  [hands hands]
                  [current-trick '()]
                  [trick-lead winner]
                  [tricks-ns (+ (game-tricks-ns g) (if ns? 1 0))]
                  [tricks-ew (+ (game-tricks-ew g) (if ns? 0 1))]
                  [tricks-played played]
                  [phase (if (= played 13) 'Scoring 'Playing)])]))

;; The computer's choice of card for the seat whose turn it is
(define (ai-card g)
  (define actor (current-actor g))
  (ai-select-card (hand-of g actor) (trick-cards g) (game-trick-lead g)
                  (game-trump g) actor (game-declarer g)))
