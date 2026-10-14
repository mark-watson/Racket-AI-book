#lang racket
;; scoring.rkt -- rubber bridge scoring.

(require "cards.rkt" "bidding.rkt")
(provide (all-defined-out))

(struct rubber (ns-below ew-below ns-above ew-above ns-games ew-games
                ns-vul? ew-vul? dealer deals-played)
  #:transparent)

(define (new-rubber) (rubber 0 0 0 0 0 0 #f #f 'North 0))

;; Points per trick bid and made
(define (trick-value strain doubled)
  (* (if (< strain HEARTS) 20 30)
     (case doubled [(1) 2] [(2) 4] [else 1])))

(define (contract-trick-score level strain doubled)
  (+ (* level (trick-value strain doubled))
     (if (= strain NT) (case doubled [(1) 20] [(2) 40] [else 10]) 0)))

(define (undertrick-penalty down doubled vul?)
  (define doubled-penalty
    (if vul?
        (+ 200 (* 300 (sub1 down)))
        (case down [(1) 100] [(2) 300] [(3) 500] [else (+ 500 (* 300 (- down 3)))])))
  (case doubled
    [(0) (* down (if vul? 100 50))]
    [(1) doubled-penalty]
    [else (* 2 doubled-penalty)]))

;; Score one deal. Returns (values new-rubber-state score-for-declarer).
(define (score-deal level strain tricks-won declarer doubled rs)
  (define ns? (= (side declarer) 0))
  (define vul? (if ns? (rubber-ns-vul? rs) (rubber-ew-vul? rs)))
  (define overtricks (- tricks-won (+ level 6)))
  (cond
    [(>= overtricks 0)
     (define below (contract-trick-score level strain doubled))
     (define overtrick-value
       (case doubled [(0) (trick-value strain 0)] [(1) (if vul? 200 100)] [else (if vul? 400 200)]))
     (define above (+ (* overtricks overtrick-value)
                      (case doubled [(1) 50] [(2) 100] [else 0])
                      (case level [(6) (if vul? 750 500)] [(7) (if vul? 1500 1000)] [else 0])))
     (define rs1
       (if ns?
           (struct-copy rubber rs [ns-below (+ (rubber-ns-below rs) below)]
                        [ns-above (+ (rubber-ns-above rs) above)])
           (struct-copy rubber rs [ew-below (+ (rubber-ew-below rs) below)]
                        [ew-above (+ (rubber-ew-above rs) above)])))
     ;; 100 points below the line wins a game; part scores are wiped out.
     ;; Below-the-line points also count toward the rubber total, so they are
     ;; carried above the line when the game ends.
     (define game? (>= (if ns? (rubber-ns-below rs1) (rubber-ew-below rs1)) 100))
     (define rs2
       (if game?
           (struct-copy rubber rs1
                        [ns-above (+ (rubber-ns-above rs1) (rubber-ns-below rs1))]
                        [ew-above (+ (rubber-ew-above rs1) (rubber-ew-below rs1))]
                        [ns-below 0] [ew-below 0]
                        [ns-games (+ (rubber-ns-games rs1) (if ns? 1 0))]
                        [ew-games (+ (rubber-ew-games rs1) (if ns? 0 1))]
                        [ns-vul? (or ns? (rubber-ns-vul? rs1))]
                        [ew-vul? (or (not ns?) (rubber-ew-vul? rs1))])
           rs1))
     (values rs2 (+ below above))]
    [else
     (define penalty (undertrick-penalty (- overtricks) doubled vul?))
     (values (if ns?
                 (struct-copy rubber rs [ew-above (+ (rubber-ew-above rs) penalty)])
                 (struct-copy rubber rs [ns-above (+ (rubber-ns-above rs) penalty)]))
             (- penalty))]))

(define (rubber-complete? rs)
  (or (>= (rubber-ns-games rs) 2) (>= (rubber-ew-games rs) 2)))

;; Returns (values bonus winner) where winner is "N-S", "E-W" or #f
(define (rubber-bonus rs)
  (cond [(>= (rubber-ns-games rs) 2) (values (if (zero? (rubber-ew-games rs)) 700 500) "N-S")]
        [(>= (rubber-ew-games rs) 2) (values (if (zero? (rubber-ns-games rs)) 700 500) "E-W")]
        [else (values 0 #f)]))
