#lang racket
;; tests.rkt -- unit tests plus a headless simulation that plays complete
;; rubbers through the same commands the web UI sends. No window is opened.

(require rackunit "cards.rkt" "bidding.rkt" "play.rkt" "scoring.rkt" "engine.rkt"
         "bridge-api.rkt")

(define (cards . codes) (map string->card codes))

;; Cards
(check-equal? (card->string (card HEARTS 10)) "10H")
(check-equal? (string->card "as") (card SPADES ACE))
(check-false (string->card "1S"))
(check-equal? (hand-hcp (cards "AS" "KH" "QD" "JC" "2C")) 10)
(let ([hands (deal-hands (make-deck))])
  (check-equal? (for/sum ([(p h) hands]) (length h)) 52)
  (check-true (andmap (lambda (p) (= 13 (length (hash-ref hands p)))) players)))

;; Bids
(check-equal? (string->bid "3nt") (suit-bid 3 NT))
(check-equal? (string->bid "XX") 'redouble)
(check-true (bid-higher? (suit-bid 2 CLUBS) (suit-bid 1 NT)))
(check-equal? (bid->string (suit-bid 4 SPADES)) "4S")

;; Auction end and contract extraction (histories are newest first)
(let ([h (list (cons 'West 'pass) (cons 'South 'pass) (cons 'East 'pass)
               (cons 'North (suit-bid 1 HEARTS)))])
  (check-true (bidding-complete? h))
  (check-equal? (extract-contract h) (list (suit-bid 1 HEARTS) 'North 0)))
(let ([h (list (cons 'East 'pass) (cons 'North 'pass) (cons 'West 'pass)
               (cons 'South 'redouble) (cons 'East 'double) (cons 'North (suit-bid 1 HEARTS)))])
  (check-equal? (third (extract-contract h)) 2))
(check-true (legal-call? 'double 'East (list (cons 'North (suit-bid 1 HEARTS)))))
(check-false (legal-call? 'double 'South (list (cons 'North (suit-bid 1 HEARTS)))))

;; AI bidding
(check-equal? (ai-opening-bid (cards "AS" "KS" "QS" "AH" "KH" "QH" "AD" "KD" "QD"
                                     "AC" "2C" "3C" "4C"))
              (suit-bid 2 CLUBS))
(check-equal? (ai-opening-bid (cards "AS" "KS" "5S" "AH" "4H" "3H" "KD" "QD" "2D"
                                     "JC" "4C" "3C" "2C"))
              (suit-bid 1 NT))

;; Trick taking
(check-equal? (trick-winner (cards "2H" "AH" "3S" "KH") 'North SPADES) 'South)
(check-equal? (trick-winner (cards "2H" "AH" "3S" "KH") 'North #f) 'East)
(check-equal? (legal-plays (cards "AS" "2H" "3H") HEARTS) (cards "2H" "3H"))

;; Scoring: 4S made exactly, not vulnerable, wins a game
(let-values ([(rs score) (score-deal 4 SPADES 10 'South 0 (new-rubber))])
  (check-equal? score 120)
  (check-equal? (rubber-ns-games rs) 1)
  (check-true (rubber-ns-vul? rs)))
;; 3NT doubled, vulnerable, down two
(let-values ([(rs score) (score-deal 3 NT 7 'East 1
                                     (struct-copy rubber (new-rubber) [ew-vul? #t]))])
  (check-equal? score -500)
  (check-equal? (rubber-ns-above rs) 500))

;; ---------------------------------------------------------------------------
;; Simulation: South bids and plays randomly-but-legally via handle-command
;; ---------------------------------------------------------------------------

(define (human-bid state)
  (define highest
    (for/fold ([hi #f]) ([e (hash-ref state 'bidHistory)])
      (define b (string->bid (hash-ref e 'beBid)))
      (if (suit-bid? b) b hi)))
  (define options (cons "Pass" (for*/list ([l (in-range 1 8)] [s (in-range 5)]
                                           #:when (or (not highest)
                                                      (bid-higher? (suit-bid l s) highest)))
                                 (bid->string (suit-bid l s)))))
  ;; Mostly pass so auctions end, sometimes bid low
  (if (< (random) 0.7) "Pass" (list-ref options (min 3 (sub1 (length options))))))

(define (play-rubber)
  (reset-session!)
  (let loop ([state (handle-command "get-state" (hasheq))] [steps 0])
    (check-false (hash-ref state 'error #f) (format "~a" state))
    (when (> steps 5000) (error "simulation did not terminate"))
    (match (hash-ref state 'phase)
      ["Done" state]
      ["Scoring" (loop (handle-command "next-deal" (hasheq)) (add1 steps))]
      ["Bidding"
       (check-equal? (hash-ref state 'activeActor) "South")
       (loop (handle-command "bid" (hasheq 'bid (human-bid state))) (add1 steps))]
      ["Playing"
       (define legal (hash-ref state 'legalCards))
       (loop (if (null? legal)
                 (handle-command "ai-play-single" (hasheq))
                 (handle-command "play-card" (hasheq 'card (list-ref legal (random (length legal))))))
             (add1 steps))])))

(for ([i 20])
  (define final (play-rubber))
  (define rs (hash-ref final 'rubberState))
  (check-true (or (>= (hash-ref rs 'nsGames) 2) (>= (hash-ref rs 'ewGames) 2))))

;; Illegal requests are rejected with an error, not an exception
(reset-session!)
(check-true (hash-has-key? (handle-command "play-card" (hasheq 'card "AS")) 'error))
(check-true (hash-has-key? (handle-command "bid" (hasheq 'bid "9Z")) 'error))
(check-true (hash-has-key? (handle-command "nonsense" (hasheq)) 'error))
