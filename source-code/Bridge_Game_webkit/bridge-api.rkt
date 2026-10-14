#lang racket
;; bridge-api.rkt -- the commands the web UI sends, and the JSON it gets back.
;;
;; This module knows nothing about WebKit: (handle-command name payload)
;; takes a command string and a jsexpr payload and returns a jsexpr. That
;; keeps the whole game playable and testable without a window.

(require json "cards.rkt" "bidding.rkt" "play.rkt" "scoring.rkt" "engine.rkt")
(provide handle-command reset-session!)

;; ---------------------------------------------------------------------------
;; Session state (one rubber in progress)
;; ---------------------------------------------------------------------------

(struct session
  (game
   rubber
   last-trick         ; the last completed trick, as (cons player card) in play order
   rubber-tricks-ns   ; tricks won in finished deals of this rubber
   rubber-tricks-ew)
  #:transparent)

(define (vulnerability-of rs)
  (match* ((rubber-ns-vul? rs) (rubber-ew-vul? rs))
    [(#t #t) "Both"] [(#t #f) "NsOnly"] [(#f #t) "EwOnly"] [(#f #f) "None"]))

(define (deal-for rs)
  (run-ai-bidding (new-game (rubber-dealer rs) (vulnerability-of rs) 'South)))

(define (fresh-session)
  (define rs (new-rubber))
  (session (deal-for rs) rs '() 0 0))

(define current (fresh-session))

(define (reset-session!) (set! current (fresh-session)))

;; Start the next deal, banking this deal's trick counts
(define (next-deal s)
  (define g (session-game s))
  (session (deal-for (session-rubber s)) (session-rubber s) '()
           (+ (session-rubber-tricks-ns s) (game-tricks-ns g))
           (+ (session-rubber-tricks-ew s) (game-tricks-ew g))))

;; Play one card for the current actor, scoring the deal if it was the last
(define (play-and-score s c)
  (define g (session-game s))
  (define actor (current-actor g))
  (define trick (append (reverse (game-current-trick g)) (list (cons actor c))))
  (define g2 (apply-card-play c g))
  (define last-trick (if (= (length trick) 4) trick '()))
  (cond
    [(eq? (game-phase g2) 'Scoring)
     (match-define (suit-bid level strain) (game-contract g2))
     (define declarer (game-declarer g2))
     (define tricks-won (if (= (side declarer) 0) (game-tricks-ns g2) (game-tricks-ew g2)))
     (define-values (rs _score)
       (score-deal level strain tricks-won declarer (game-doubled g2) (session-rubber s)))
     (define rs2 (struct-copy rubber rs
                              [deals-played (add1 (rubber-deals-played rs))]
                              [dealer (next-player (rubber-dealer rs))]))
     (struct-copy session s [game g2] [rubber rs2] [last-trick last-trick])]
    [else (struct-copy session s [game g2] [last-trick last-trick])]))

;; ---------------------------------------------------------------------------
;; JSON marshalling (field names match the JavaScript in index.html)
;; ---------------------------------------------------------------------------

(define (sym->json x) (if x (symbol->string x) (json-null)))

(define (hands->json hands)
  (hasheq 'hlNorth (map card->string (hash-ref hands 'North))
          'hlEast (map card->string (hash-ref hands 'East))
          'hlSouth (map card->string (hash-ref hands 'South))
          'hlWest (map card->string (hash-ref hands 'West))))

(define (played->json pcs)
  (for/list ([pc pcs])
    (hasheq 'pcPlayer (symbol->string (car pc)) 'pcCard (card->string (cdr pc)))))

(define (rubber->json rs)
  (hasheq 'nsBelow (rubber-ns-below rs) 'ewBelow (rubber-ew-below rs)
          'nsAbove (rubber-ns-above rs) 'ewAbove (rubber-ew-above rs)
          'nsGames (rubber-ns-games rs) 'ewGames (rubber-ew-games rs)
          'nsVulnerable (rubber-ns-vul? rs) 'ewVulnerable (rubber-ew-vul? rs)
          'currentDealer (symbol->string (rubber-dealer rs))
          'dealsPlayed (rubber-deals-played rs)))

(define (state->json s)
  (define g (session-game s))
  (define rs (session-rubber s))
  (define actor (current-actor g))
  (define phase (game-phase g))
  (define human-turn? (and actor (eq? phase 'Playing) (human-controls? g actor)))
  (hasheq
   ;; The UI's "Done" phase means the rubber is over
   'phase (symbol->string (if (and (eq? phase 'Scoring) (rubber-complete? rs)) 'Done phase))
   'dealer (symbol->string (game-dealer g))
   'vulnerability (game-vulnerability g)
   'humanPlayer (symbol->string (game-human g))
   'activeActor (sym->json actor)
   'hands (hands->json (game-hands g))
   'originalHands (hands->json (game-original-hands g))
   'bidHistory (for/list ([e (reverse (game-bid-history g))])
                 (hasheq 'bePlayer (symbol->string (car e)) 'beBid (bid->string (cdr e))))
   'contract (if (game-contract g) (bid->string (game-contract g)) (json-null))
   'declarer (sym->json (game-declarer g))
   'doubled (game-doubled g)
   'dummy (sym->json (game-dummy g))
   ;; Dummy is shown after the opening lead, or when the human is about to play it
   'dummyVisible (and (eq? phase 'Playing)
                      (or (> (game-tricks-played g) 0)
                          (pair? (game-current-trick g))
                          human-turn?))
   'currentTrick (played->json (reverse (game-current-trick g)))
   'lastCompletedTrick (played->json (session-last-trick s))
   'trickLead (symbol->string (game-trick-lead g))
   'tricksNs (game-tricks-ns g)
   'tricksEw (game-tricks-ew g)
   'tricksPlayed (game-tricks-played g)
   'trumpSuit (if (game-trump g) (suit-name (game-trump g)) (json-null))
   'rubberState (rubber->json rs)
   'legalCards (if human-turn? (map card->string (legal-cards-for g actor)) '())
   ;; deals-played is bumped at scoring time, so don't count the finished deal twice
   'dealNum (+ (rubber-deals-played rs) (if (eq? phase 'Scoring) 0 1))
   'totalTricksNs (+ (session-rubber-tricks-ns s) (game-tricks-ns g))
   'totalTricksEw (+ (session-rubber-tricks-ew s) (game-tricks-ew g))))

(define (error-json fmt . args) (hasheq 'error (apply format fmt args)))

;; ---------------------------------------------------------------------------
;; Command handlers
;; ---------------------------------------------------------------------------

(define (do-bid payload)
  (define g (session-game current))
  (define text (and (hash? payload) (hash-ref payload 'bid #f)))
  (define b (and (string? text) (string->bid text)))
  (cond
    [(not (eq? (game-phase g) 'Bidding)) (error-json "The auction is over.")]
    [(not (eq? (current-actor g) 'South)) (error-json "It is not your turn to bid.")]
    [(not b) (error-json "Failed to parse bid: ~a" text)]
    [(not (legal-call? b 'South (game-bid-history g))) (error-json "Bid ~a is not legal." text)]
    [else
     (define g2 (run-ai-bidding (apply-bid b g)))
     (set! current
           (if (eq? (game-phase g2) 'Done)
               ;; Passed out: the deal moves to the next dealer
               (let ([rs (struct-copy rubber (session-rubber current)
                                      [dealer (next-player (rubber-dealer (session-rubber current)))])])
                 (struct-copy session current [game (deal-for rs)] [rubber rs] [last-trick '()]))
               (struct-copy session current [game g2] [last-trick '()])))
     (state->json current)]))

(define (do-play-card payload)
  (define g (session-game current))
  (define actor (current-actor g))
  (define text (and (hash? payload) (hash-ref payload 'card #f)))
  (define c (and (string? text) (string->card text)))
  (cond
    [(not (eq? (game-phase g) 'Playing)) (error-json "No card play is in progress.")]
    [(not (human-controls? g actor)) (error-json "It is ~a's turn, not yours." actor)]
    [(not (and c (member c (legal-cards-for g actor))))
     (error-json "Card ~a is not a legal play." text)]
    [else (set! current (play-and-score current c))
          (state->json current)]))

(define (do-ai-play-single)
  (define g (session-game current))
  (define actor (current-actor g))
  (cond
    [(not (eq? (game-phase g) 'Playing)) (error-json "No card play is in progress.")]
    [(human-controls? g actor) (error-json "It is your turn to play, not the computer's.")]
    [else (set! current (play-and-score current (ai-card g)))
          (state->json current)]))

(define (handle-command command payload)
  (match command
    ["get-state" (state->json current)]
    ["bid" (do-bid payload)]
    ["play-card" (do-play-card payload)]
    ["ai-play-single" (do-ai-play-single)]
    ["next-deal" (set! current (next-deal current)) (state->json current)]
    ["reset-game" (reset-session!) (state->json current)]
    [_ (error-json "Unknown command: ~a" command)]))
