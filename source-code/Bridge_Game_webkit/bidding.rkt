#lang racket
;; bidding.rkt -- auction rules and a heuristic Standard American bidder.
;;
;; A bid is either a contract bid (suit-bid level strain) or one of the
;; symbols 'pass, 'double, 'redouble. A strain is a suit 0..3 or NT (4).
;;
;; Bid histories are lists of (cons player bid) stored NEWEST FIRST,
;; which makes "what was the last bid?" a simple car.

(require "cards.rkt")
(provide (all-defined-out))

(define NT 4)

(struct suit-bid (level strain) #:transparent)

(define (strain->string s) (if (= s NT) "NT" (string (suit-letter s))))

(define (bid->string b)
  (match b
    [(suit-bid l s) (format "~a~a" l (strain->string s))]
    ['pass "Pass"]
    ['double "Dbl"]
    ['redouble "Rdbl"]))

;; Parse user input such as "1H", "3NT", "pass", "X", "XX"
(define (string->bid raw)
  (define s (string-upcase (string-trim raw)))
  (cond
    [(member s '("PASS" "P")) 'pass]
    [(member s '("DOUBLE" "DBL" "X")) 'double]
    [(member s '("REDOUBLE" "RDBL" "XX")) 'redouble]
    [(and (>= (string-length s) 2) (char<=? #\1 (string-ref s 0) #\7))
     (define strain (string->strain (substring s 1)))
     (and strain (suit-bid (- (char->integer (string-ref s 0)) 48) strain))]
    [else #f]))

(define (string->strain s)
  (cond [(member s '("C" "CLUBS")) CLUBS]
        [(member s '("D" "DIAMONDS")) DIAMONDS]
        [(member s '("H" "HEARTS")) HEARTS]
        [(member s '("S" "SPADES")) SPADES]
        [(member s '("N" "NT" "NOTRUMP" "NO TRUMP")) NT]
        [else #f]))

;; ---------------------------------------------------------------------------
;; Auction rules
;; ---------------------------------------------------------------------------

;; 1C = 0, 1D = 1, ..., 7NT = 34
(define (bid-index b) (+ (* 5 (sub1 (suit-bid-level b))) (suit-bid-strain b)))

(define (bid-higher? b1 b2)
  (and (suit-bid? b1) (suit-bid? b2) (> (bid-index b1) (bid-index b2))))

(define (last-suit-bid history)
  (for/first ([e history] #:when (suit-bid? (cdr e))) (cdr e)))

(define (passed-out? history)
  (and (= (length history) 4) (andmap (lambda (e) (eq? (cdr e) 'pass)) history)))

;; Auction ends after four opening passes, or three passes following any bid
(define (bidding-complete? history)
  (define bids (map cdr history))
  (or (passed-out? history)
      (and (>= (length bids) 4)
           (equal? (take bids 3) '(pass pass pass))
           (ormap suit-bid? bids))))

;; Is a double / redouble legal for PLAYER right now?
(define (legal-call? b player history)
  (define last-call (for/first ([e history] #:unless (eq? (cdr e) 'pass)) e))
  (match b
    ['pass #t]
    [(suit-bid _ _) (let ([hi (last-suit-bid history)]) (or (not hi) (bid-higher? b hi)))]
    ['double (and last-call (suit-bid? (cdr last-call))
                  (not (= (side (car last-call)) (side player))))]
    ['redouble (and last-call (eq? (cdr last-call) 'double)
                    (not (= (side (car last-call)) (side player))))]))

;; Returns (list contract declarer doubled) or #f for a passed-out deal.
;; doubled is 0 (none), 1 (doubled) or 2 (redoubled).
(define (extract-contract history)
  (define last (for/first ([e history] #:when (suit-bid? (cdr e))) e))
  (and last
       (let* ([contract (cdr last)]
              [strain (suit-bid-strain contract)]
              [decl-side (side (car last))]
              ;; Declarer: first player of the winning side to name the strain
              [declarer (or (for/first ([e (reverse history)]
                                        #:when (and (suit-bid? (cdr e))
                                                    (= (suit-bid-strain (cdr e)) strain)
                                                    (= (side (car e)) decl-side)))
                              (car e))
                            (car last))]
              ;; The most recent double or redouble after the final contract bid wins
              [doubled (or (for/first ([e history]
                                       #:break (suit-bid? (cdr e))
                                       #:when (memq (cdr e) '(double redouble)))
                             (if (eq? (cdr e) 'double) 1 2))
                           0)])
         (list contract declarer doubled))))

;; First contract bid made by a given player, in auction order
(define (first-suit-bid-by history who)
  (for/first ([e (reverse history)] #:when (and (eq? (car e) who) (suit-bid? (cdr e))))
    (cdr e)))

;; ---------------------------------------------------------------------------
;; AI bidding: opening bids
;; ---------------------------------------------------------------------------

(define (ai-opening-bid hand)
  (define hcp (hand-hcp hand))
  (define bal (balanced? hand))
  (define (opening-suit)
    (cond [(>= (suit-length SPADES hand) 5) SPADES]
          [(>= (suit-length HEARTS hand) 5) HEARTS]
          [(> (suit-length DIAMONDS hand) (suit-length CLUBS hand)) DIAMONDS]
          [else CLUBS]))
  (define seven-card-suits
    (for/list ([s all-suits] #:when (and (>= (suit-length s hand) 7)
                                         (>= (suit-honor-count hand s) 2)))
      s))
  (define weak-two-suits
    (for/list ([s (list DIAMONDS HEARTS SPADES)] #:when (>= (suit-length s hand) 6)) s))
  (cond
    [(>= hcp 22) (suit-bid 2 CLUBS)]
    [(and bal (<= 20 hcp 21)) (suit-bid 2 NT)]
    [(and bal (<= 15 hcp 17)) (suit-bid 1 NT)]
    [(<= 13 hcp 21) (suit-bid 1 (opening-suit))]
    [(and (< hcp 13) (pair? seven-card-suits)) (suit-bid 3 (car seven-card-suits))]
    [(and (<= 5 hcp 12) (pair? weak-two-suits)) (suit-bid 2 (car weak-two-suits))]
    [else #f]))

;; ---------------------------------------------------------------------------
;; AI bidding: responses to partner's opening bid
;; ---------------------------------------------------------------------------

(define (ai-responding-bid hand partner-bid)
  (define hcp (hand-hcp hand))
  (define (len s) (suit-length s hand))
  (match-define (suit-bid p-level p-strain) partner-bid)
  (cond
    ;; Partner opened 1NT (15-17)
    [(and (= p-strain NT) (= p-level 1))
     (cond
       [(<= hcp 7) (cond [(>= (len SPADES) 5) (suit-bid 2 SPADES)]
                         [(>= (len HEARTS) 5) (suit-bid 2 HEARTS)]
                         [(>= (len DIAMONDS) 5) (suit-bid 2 DIAMONDS)]
                         [else #f])]
       [(or (>= (len HEARTS) 4) (>= (len SPADES) 4)) (suit-bid 2 CLUBS)] ; Stayman
       [(<= hcp 9) (suit-bid 2 NT)]
       [(and (<= hcp 15) (>= (len SPADES) 6)) (suit-bid 4 SPADES)]
       [(and (<= hcp 15) (>= (len HEARTS) 6)) (suit-bid 4 HEARTS)]
       [(and (<= hcp 15) (>= (len SPADES) 5)) (suit-bid 3 SPADES)]
       [(and (<= hcp 15) (>= (len HEARTS) 5)) (suit-bid 3 HEARTS)]
       [(<= hcp 15) (suit-bid 3 NT)]
       [else (suit-bid 4 NT)])]
    ;; Partner opened 2NT (20-21)
    [(= p-strain NT)
     (cond
       [(and (<= hcp 4) (andmap (lambda (s) (< (len s) 5)) all-suits)) #f]
       [(and (>= hcp 4) (or (>= (len HEARTS) 4) (>= (len SPADES) 4))) (suit-bid 3 CLUBS)]
       [(>= hcp 5) (suit-bid 3 NT)]
       [else #f])]
    ;; Partner opened one of a major
    [(and (= p-level 1) (>= p-strain HEARTS))
     (cond
       [(<= hcp 5) #f]
       [(<= hcp 10) (cond [(>= (len p-strain) 3) (suit-bid 2 p-strain)]
                          [(and (= p-strain HEARTS) (>= (len SPADES) 4)) (suit-bid 1 SPADES)]
                          [else (suit-bid 1 NT)])]
       [(<= hcp 12) (cond [(>= (len p-strain) 3) (suit-bid 3 p-strain)]
                          [(> (hand-longest-suit hand) p-strain)
                           (suit-bid 2 (hand-longest-suit hand))]
                          [else (suit-bid 2 NT)])]
       [(>= (len p-strain) 3) (suit-bid 4 p-strain)]
       [else (suit-bid 3 NT)])]
    ;; Partner opened one of a minor
    [(= p-level 1)
     (cond
       [(< hcp 6) #f]
       [(or (>= (len HEARTS) 4) (>= (len SPADES) 4))
        (if (or (and (>= (len HEARTS) 5) (>= (len SPADES) 5)) (< (len HEARTS) 4))
            (suit-bid 1 SPADES)
            (suit-bid 1 HEARTS))]
       [(<= hcp 10) (suit-bid 1 NT)]
       [(<= hcp 12) (if (>= (len p-strain) 4) (suit-bid 3 p-strain) (suit-bid 2 NT))]
       [(<= hcp 15) (if (>= (len p-strain) 4) (suit-bid 5 p-strain) (suit-bid 3 NT))]
       [else (suit-bid 4 NT)])]
    ;; Partner opened a strong 2C
    [(= p-strain CLUBS)
     (define bal (balanced? hand))
     (cond
       [(< hcp 8) (suit-bid 2 DIAMONDS)]
       [(and (not bal) (>= (len SPADES) 5)) (suit-bid 2 SPADES)]
       [(and (not bal) (>= (len HEARTS) 5)) (suit-bid 2 HEARTS)]
       [(and (not bal) (>= (len DIAMONDS) 5)) (suit-bid 3 DIAMONDS)]
       [(and (not bal) (>= (len CLUBS) 5)) (suit-bid 3 CLUBS)]
       [else (suit-bid 2 NT)])]
    ;; Partner opened a weak two
    [(= p-level 2)
     (cond [(and (>= (len p-strain) 3) (>= hcp 14)) (suit-bid 4 p-strain)]
           [(and (>= (len p-strain) 3) (>= hcp 10)) (suit-bid 3 p-strain)]
           [else #f])]
    ;; Partner made a three-level preempt
    [(and (>= (len p-strain) 3) (>= hcp 14))
     (suit-bid (if (>= p-strain HEARTS) 4 5) p-strain)]
    [else #f]))

;; ---------------------------------------------------------------------------
;; AI bidding: opener's rebid, including Stayman, Blackwood and Gerber replies
;; ---------------------------------------------------------------------------

(define (count-rank hand rank) (count (lambda (c) (= (card-rank c) rank)) hand))

(define (ai-opener-rebid hand my-opening partner-response)
  (define hcp (hand-hcp hand))
  (define (len s) (suit-length s hand))
  (match-define (suit-bid my-level my-strain) my-opening)
  (match-define (suit-bid p-level p-strain) partner-response)
  (define (gerber-reply level n)
    (case n
      [(0) (suit-bid level DIAMONDS)] [(1) (suit-bid level HEARTS)]
      [(2) (suit-bid level SPADES)] [(3) (suit-bid level NT)]
      [else (suit-bid (add1 level) CLUBS)]))
  (cond
    ;; Stayman: 2C over 1NT or 3C over 2NT asks for a four-card major
    [(and (= my-strain NT) (= p-strain CLUBS) (= p-level (add1 my-level)) (<= my-level 2))
     (define lvl p-level)
     (cond [(>= (len HEARTS) 4) (suit-bid lvl HEARTS)]
           [(>= (len SPADES) 4) (suit-bid lvl SPADES)]
           [else (suit-bid lvl DIAMONDS)])]
    ;; Blackwood 4NT asks for aces, 5NT for kings (step responses)
    [(equal? partner-response (suit-bid 4 NT))
     (suit-bid 5 (modulo (count-rank hand ACE) 5))]
    [(equal? partner-response (suit-bid 5 NT))
     (suit-bid 6 (modulo (count-rank hand KING) 5))]
    ;; Gerber 4C over notrump asks for aces, 5C for kings
    [(and (equal? partner-response (suit-bid 4 CLUBS)) (= my-strain NT))
     (gerber-reply 4 (count-rank hand ACE))]
    [(equal? partner-response (suit-bid 5 CLUBS))
     (gerber-reply 5 (count-rank hand KING))]
    ;; Partner responded in notrump
    [(= p-strain NT)
     (cond [(and (< my-strain NT) (>= (len my-strain) 6)) (suit-bid (add1 p-level) my-strain)]
           [(<= hcp 15) #f]
           [(<= hcp 17) (suit-bid (add1 p-level) NT)]
           [else (suit-bid (+ 2 p-level) NT)])]
    ;; Partner raised my suit
    [(and (< my-strain NT) (= p-strain my-strain))
     (cond [(<= hcp 15) #f]
           [(and (>= hcp 19) (>= my-strain HEARTS)) (suit-bid 4 NT)] ; Blackwood
           [else (suit-bid (if (>= my-strain HEARTS) 4 5) my-strain)])]
    ;; Partner bid a new suit
    [(>= (len p-strain) 4) (suit-bid (add1 p-level) p-strain)]
    [(and (< my-strain NT) (>= (len my-strain) 6)) (suit-bid 2 my-strain)]
    [(and (<= hcp 15) (balanced? hand)) (suit-bid (if (>= p-level 2) p-level 1) NT)]
    [(and (< my-strain NT) (>= (len my-strain) 5)) (suit-bid 2 my-strain)]
    [else (suit-bid 1 NT)]))

;; ---------------------------------------------------------------------------
;; AI bidding: top level
;; ---------------------------------------------------------------------------

(define (ai-overcall hand)
  (define hcp (hand-hcp hand))
  (cond [(and (>= hcp 13) (hand-has-major? hand 5))
         (suit-bid 1 (if (>= (suit-length SPADES hand) 5) SPADES HEARTS))]
        [(and (<= 15 hcp 18) (balanced? hand)) (suit-bid 1 NT)]
        [else #f]))

;; Choose a bid for player ME. Anything that would be insufficient becomes a pass.
(define (ai-select-bid hand history me)
  (define highest (last-suit-bid history))
  (define partner-bid (first-suit-bid-by history (partner me)))
  (define my-bid (first-suit-bid-by history me))
  (define raw
    (cond [(and my-bid partner-bid) (ai-opener-rebid hand my-bid partner-bid)]
          [partner-bid (ai-responding-bid hand partner-bid)]
          [(not highest) (ai-opening-bid hand)]
          [else (ai-overcall hand)]))
  (if (and raw (or (not highest) (bid-higher? raw highest)))
      raw
      'pass))
