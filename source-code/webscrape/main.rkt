#lang racket/base

;;; main.rkt — Re-export web scraping utilities
;;; Copyright (C) 2022-2025 Mark Watson
;;; Apache 2 License

(require "webscrape.rkt")

(provide web-uri->xexp
         web-uri->text
         web-uri->links
         web-uri->html-headers)