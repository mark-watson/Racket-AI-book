#lang racket/base
;; main.rkt -- Rubber bridge in a native macOS WebKit window.
;;
;; You play South against three computer players. Run with:
;;     make            ; builds libwebkit_racket.dylib
;;     racket main.rkt

(require racket/file racket/runtime-path "webkit.rkt" "bridge-api.rkt")

(define-runtime-path index-html "index.html")

(module+ main
  (printf "Starting Racket rubber bridge...\n")
  (define app (make-webkit-app "Rubber Bridge — Racket WebKit" 1320 960))
  (for ([command '("get-state" "bid" "play-card" "ai-play-single" "next-deal" "reset-game")])
    (register-handler! app command
                       (lambda (payload)
                         (printf "[Racket] ~a ~a\n" command payload)
                         (flush-output)
                         (handle-command command payload))))
  (load-html! app (file->string index-html))
  (run-webkit-app! app)
  (printf "Bridge window closed.\n"))
