#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Non-interactive demo of the custom tools in custom-tools.rkt.
;;;
;;; Run with:  OLLAMA_MODEL=qwen3.5:4b racket demo-custom.rkt

(require "tools.rkt")
(require "custom-tools.rkt")

(register-custom-tools)

(define (section title)
  (displayln (string-append "--- " title " ---")))

(section "calculate")
(displayln (call-ollama-with-tools "What is 12.5% of 640?" '("calculate")))
(newline)

(section "notes")
(displayln (call-ollama-with-tools
            "Please save a note that my dentist appointment is on Tuesday at 3pm, then list my notes back to me."
            '("save_note" "list_notes")))
(newline)

(section "multi-tool")
(displayln (call-ollama-with-tools
            "What day and time is it, and what is the weather in Flagstaff Arizona?"
            '("get_current_datetime" "get_weather")))

