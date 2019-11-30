#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; util.rkt module
;;
;; This module contains utility types for the project
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; A (Pair a b) is a pair of values
;;
(define-struct (Pair A B)
  ([fst : A]
   [snd : B]))

;; A (Triple a b c) is a struct of three of values
;;
(define-struct (Triple A B C)
  ([fst : A]
   [snd : B]
   [thd : C]))

;; An (Option T) is either 'None or (Some v), where v
;; has type T
(define-type (Option T) (U 'None (Some T)))

(define-struct (Some T)
  ([value : T]))

(: option-map : (All (A B) (A -> B) (Option A) -> (Option B)))
;; map the function f over a optional value
(define (option-map f opt-v)
  (match opt-v
    ['None 'None]
    [(Some v) (Some (f v))]))

;; Exports
;;;;;;;;;;

(provide (struct-out Pair)
         (struct-out Triple)
         Option
         (struct-out Some))

(provide option-map)
