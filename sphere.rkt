#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; sphere.rkt module
;; <Xinyu Liu>
;;
;; This module contains the implementation of the sphere object
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "object.rkt")
(require "material.rkt")
(require "color.rkt")

(: make-sphere : Float3 Float Material -> Object)
;; take a position, radius and material and return
;; an object that represents the sphere
(define (make-sphere posi rad mat)
  (Object
   (lambda ([ray : Ray] [min-t : Float] [max-t : Float])
     (local
       {;; the definitions of these six variables are
        ;; provided in the write up.
        (define P : Float3 (Ray-origin ray))
        (define V : Float3 (fl3- P posi))
        (define D : Float3 (Ray-dir ray))
        (define b : Float (* 2.0 (fl3-dot V D)))
        (define c : Float (- (fl3-dot V V) (* rad rad)))
        (define d : Float (- (* b b) (* 4.0 c)))}
       (if (< d 0.0)
           'Miss
           (local
             {(define t : Float (/ (- (- b) (sqrt d)) 2.0))
              (define Q : Float3 (fl3+ P (fl3-scale t D)))
              ;; N is the unit normal vector at the intersection.
              (define N : Float3 (fl3-normalize (fl3- Q posi)))}
             (if (<= min-t t max-t)
                 (Hit t Q N mat)
                 'Miss)))))))

;; run tests
(check-expect
 (object-hit? (make-sphere (Float3 1.0 3.0 5.0) 1.0
                           (flat-material (RGB 1.0 0.0 0.0)))
              (make-ray fl3-zero (Float3 1.0 1.0 1.0)) 0.0 +inf.0) 'Miss)
(check-within
 (Hit-t
  (test-helper
   (object-hit? (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                             (flat-material (RGB 1.0 0.0 0.0)))
                (make-ray fl3-zero (Float3 0.0 0.0 -2.0)) 0.0 +inf.0)))
 1.5 0.1)
(check-within
 (Hit-pt
  (test-helper
   (object-hit? (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                             (flat-material (RGB 1.0 0.0 0.0)))
                (make-ray fl3-zero (Float3 0.0 0.0 -2.0)) 0.0 +inf.0)))
 (Float3 0.0 0.0 -1.5) 0.1)
(check-within
 (Hit-norm
  (test-helper
   (object-hit? (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                             (flat-material (RGB 1.0 0.0 0.0)))
                (make-ray fl3-zero (Float3 0.0 0.0 -2.0)) 0.0 +inf.0)))
 (Float3 0.0 0.0 1.0) 0.1)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-sphere)
