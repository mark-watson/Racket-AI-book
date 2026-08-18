#lang racket

;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Licensed under the GNU Affero General Public License v3.0 (AGPL-3.0)
;;; See LICENSE file for details
;;;
;;; interrupt.rkt -- shared task-interrupt flag
;;; Lives in its own module (no dependencies) so approval.rkt, tools.rkt,
;;; fireworks-ai.rkt, and agent.rkt can require it statically without
;;; creating a circular require.

(provide task-interrupted?
         task-interrupted-set!
         task-interrupted-clear!)

;; Thread-safe via a semaphore.
(define task-interrupted-box (box #f))
(define task-interrupted-sema (make-semaphore 1))

(define (task-interrupted?)
  (call-with-semaphore task-interrupted-sema
    (lambda () (unbox task-interrupted-box))))

(define (task-interrupted-set!)
  (call-with-semaphore task-interrupted-sema
    (lambda () (set-box! task-interrupted-box #t))))

(define (task-interrupted-clear!)
  (call-with-semaphore task-interrupted-sema
    (lambda () (set-box! task-interrupted-box #f))))
