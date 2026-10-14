#lang racket/base
;; webkit.rkt -- a tiny Racket FFI binding to libwebkit_racket.dylib, which
;; opens a native macOS Cocoa window holding a WKWebView.
;;
;; JavaScript in the page calls
;;     window.webkit_racket.invoke(command, payload)  ;; returns a Promise
;; and the Racket handler registered with register-handler! answers with a
;; jsexpr, which is sent back to JavaScript as JSON.

(require ffi/unsafe ffi/unsafe/define racket/runtime-path json)
(provide make-webkit-app register-handler! load-html! eval-js! run-webkit-app!)

(define-runtime-path lib-path "libwebkit_racket")

(define-ffi-definer define-wk
  (ffi-lib lib-path
           #:fail (lambda ()
                    (error 'webkit "cannot load libwebkit_racket.dylib; run `make` first"))))

;; const char* callback(const char* command, const char* payload, void* userdata)
;; The C side frees the returned string, so it must come from malloc.
(define _bridge-callback (_fun _string/utf-8 _string/utf-8 _pointer -> _pointer))

(define-wk wkr_create (_fun _string/utf-8 _int _int -> _pointer))
(define-wk wkr_run (_fun _pointer -> _void))
(define-wk wkr_load_html (_fun _pointer _string/utf-8 -> _void))
(define-wk wkr_eval_js (_fun _pointer _string/utf-8 -> _void))
(define-wk wkr_set_bridge_callback (_fun _pointer _bridge-callback _pointer -> _void))

(struct webkit-app (handle handlers [callback #:mutable]))

;; Copy a Racket string into malloc'd memory for C to own and free
(define (string->c-string s)
  (define bs (string->bytes/utf-8 s))
  (define n (bytes-length bs))
  (define p (malloc (add1 n) 'raw))
  (memcpy p bs n)
  (ptr-set! p _byte n 0)
  p)

(define (make-webkit-app title width height)
  (define handle (wkr_create title width height))
  (unless handle (error 'webkit "failed to create the Cocoa/WebKit window"))
  (define app (webkit-app handle (make-hash) #f))
  ;; Exceptions must never unwind into Objective-C, so every error becomes JSON
  (define (dispatch command payload _userdata)
    (define result
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         (hasheq 'error (if (exn? e) (exn-message e) (format "~a" e))))])
        (define handler (hash-ref (webkit-app-handlers app) command #f))
        (if handler
            (handler (with-handlers ([exn:fail? (lambda (_) (json-null))])
                       (string->jsexpr payload)))
            (hasheq 'error (format "Unknown command: ~a" command)))))
    (string->c-string (jsexpr->string result)))
  ;; Keep the callback reachable so the GC never frees it while C holds it
  (set-webkit-app-callback! app dispatch)
  (wkr_set_bridge_callback handle dispatch #f)
  app)

(define (register-handler! app command handler)
  (hash-set! (webkit-app-handlers app) command handler))

(define (load-html! app html) (wkr_load_html (webkit-app-handle app) html))
(define (eval-js! app js) (wkr_eval_js (webkit-app-handle app) js))

;; Runs the Cocoa event loop; returns when the window is closed
(define (run-webkit-app! app) (wkr_run (webkit-app-handle app)))
