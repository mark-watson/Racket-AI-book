#lang racket

;;; info.rkt — Package definition for the RAG system
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(define name "rag")
(define version "1.0.0")
(define description
  "Agentic RAG (Retrieval-Augmented Generation) using Gemini")

(define collection "rag")

(define deps
  '("base"
    "net-lib"
    "http-easy"))

(define build-deps
  '("rackunit"))
