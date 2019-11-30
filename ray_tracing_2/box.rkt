#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; box.rkt module
;; <Xinyu Liu>
;;
;; This module contains the implementation of the box object
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

(: select-axis : Float3 Axis -> Float3)
;; permute the components of the Float3 so that the component
;; corresponding to the specified axis is swapped with the
;; first (x) component.
(define (select-axis float3 axis)
  (match* (float3 axis)
    [((Float3 x y z) 'X) (Float3 x y z)]
    [((Float3 x y z) 'Y) (Float3 y x z)]
    [((Float3 x y z) 'Z) (Float3 z y x)]))

;; run tests
(check-within (select-axis (Float3 1.0 2.0 3.0) 'X)
              (Float3 1.0 2.0 3.0)
              0.0001)
(check-within (select-axis (Float3 1.0 2.0 3.0) 'Y)
              (Float3 2.0 1.0 3.0)
              0.0001)
(check-within (select-axis (Float3 1.0 2.0 3.0) 'Z)
              (Float3 3.0 2.0 1.0)
              0.0001)

(: point-in-rect? : Float3 Float3 Float3 -> Boolean)
;; a generic test that takes an intersection point, the center of
;; a rectangle, and the half-widths of the rectangle and tests to
;; see if the point is in the rectangle.
(define (point-in-rect? pot center half-wid)
  (and (<= (abs (- (Float3-y pot) (Float3-y center))) (Float3-y half-wid))
       (<= (abs (- (Float3-z pot) (Float3-z center))) (Float3-z half-wid))))

;; run tests
(check-expect (point-in-rect? (Float3 5.0 5.0 5.0)
                              (Float3 6.0 6.0 6.0)
                              (Float3 1.1 1.1 1.1)) #t)
(check-expect (point-in-rect? (Float3 5.0 5.0 5.0)
                              (Float3 6.0 6.0 6.0)
                              (Float3 0.9 1.1 1.1)) #t)
(check-expect (point-in-rect? (Float3 5.0 5.0 5.0)
                              (Float3 6.0 6.0 6.0)
                              (Float3 1.1 0.9 1.1)) #f)
(check-expect (point-in-rect? (Float3 5.0 5.0 5.0)
                              (Float3 6.0 6.0 6.0)
                              (Float3 1.1 1.1 0.9)) #f)

(: rect-pair-hit? : Ray Interval Float3 Float3 Axis Material -> Maybe-Hit)
;; a hit test for a pair of parallel rectangles.
(define (rect-pair-hit? ray min-max-t center half-wid axis mat)
  (local
    {;; the following variables are defined according to the
     ;; formula in the write-up.
     (define C : Float3 (select-axis center axis))
     (define W : Float3 (select-axis half-wid axis))
     (define Cw : Float (Float3-x C))
     (define Ww : Float (Float3-x W))
     (define Dw : Float (Ray1-delta (ray-project ray axis)))
     (define Pw : Float (Ray1-origin (ray-project ray axis)))
     (: get-hit-info : Float -> Maybe-Hit)
     ;; given the x-coordinate of the normal vector (+1.0 or -1.0),
     ;; return the hit information if there is a valid hit.
     (define (get-hit-info Nw)
       (local
         {;; the formula of a and t are provided in the write-up
          (define a : Float (+ Cw (* Nw Ww)))
          (define t : Float (/ (- a Pw) Dw))}
         (if (within? t min-max-t)
             (local
               {;; define Rt locally to avoid multiple calculations
                (define Rt : Float3 (ray-point-at ray t))}
               (if (point-in-rect? (select-axis Rt axis) C W)
                   (Hit t Rt (select-axis (Float3 Nw 0.0 0.0) axis) mat)
                   'Miss))
             'Miss)))}
    (cond
      [(< (abs Dw) 0.001) 'Miss]
      [(< Pw (- Cw Ww)) (if (> Dw 0.0) (get-hit-info -1.0) 'Miss)]
      [(> Pw (+ Cw Ww)) (if (< Dw 0.0) (get-hit-info 1.0) 'Miss)]
      [else 'Miss])))

;; run tests
(: convert : Maybe-Hit -> Hit)
;; define a helper function to convert (Option Hit-Info) into (Hit-Info)
;; the function is specifically defined for testing purpose
(define (convert hit)
  (if (Hit? hit) hit (error "Not a hit")))
;; define a test-ray and a test-hit for testing purpose
(define test-ray : Ray (Ray fl3-zero (Float3 1.0 0.0 0.0)))
(define test-hit : Hit
  (convert (rect-pair-hit? test-ray (Pair 0.0 +inf.0) (Float3 2.0 0.0 0.0)
                           (Float3 0.5 0.5 0.5) 'X normal-material)))
(define test-miss : Maybe-Hit
  (rect-pair-hit? test-ray (Pair 0.0 +inf.0) (Float3 2.0 2.0 2.0)
                  (Float3 0.5 0.5 0.5) 'X normal-material))
(check-within (Hit-t test-hit) 1.5 0.1)
(check-within (Hit-pt test-hit) (Float3 1.5 0.0 0.0) 0.1)
(check-within (Hit-norm test-hit) (Float3 -1.0 0.0 0.0) 0.1)
(check-expect test-miss 'Miss)

(: make-box : Float3 Float3 Material -> Object)
;; take the center of the box, a vector of half-widths, the surface
;; material for the box, and returns an Object representing the box.
(define (make-box center half-wid mat)
  (Object
   (lambda ([ray : Ray] [min-max-t : Interval])
     (local
       {;; define the Maybe-Hit value on the X axis.
        (define hit-or-miss-x : Maybe-Hit
          (rect-pair-hit? ray min-max-t center half-wid 'X mat))}
       (if (Hit? hit-or-miss-x) hit-or-miss-x
           (local
             {;; define the Maybe-Hit value on the Y axis.
              (define hit-or-miss-y : Maybe-Hit
                (rect-pair-hit? ray min-max-t center half-wid 'Y mat))}
             (if (Hit? hit-or-miss-y) hit-or-miss-y
                 (local
                   {;; define the Maybe-Hit value on the Z axis.
                    (define hit-or-miss-z : Maybe-Hit
                      (rect-pair-hit? ray min-max-t center half-wid 'Z mat))}
                   (if (Hit? hit-or-miss-z) hit-or-miss-z 'Miss)))))))))

;; run tests
;; define several test-objects/test rays/test hits for testing purposes.
(define test-object-1 : Object 
  (make-box (Float3 2.0 0.0 0.0) (Float3 0.5 0.5 0.5) normal-material))
(: test-object-hit?-1 : Ray Interval -> Maybe-Hit)
;; extract the hit test function in the test-object-1
(define (test-object-hit?-1 ray interval)
  (object-hit? test-object-1 ray interval))
(define test-hit2 : Hit
  (convert (test-object-hit?-1 test-ray (Pair 0.0 +inf.0))))
(define test-ray-2 : Ray (Ray fl3-zero (Float3 0.0 1.0 0.0)))
(define test-object-2 : Object 
  (make-box (Float3 0.0 2.0 0.0) (Float3 0.5 0.5 0.5) normal-material))
(: test-object-hit?-2 : Ray Interval -> Maybe-Hit)
;; extract the hit test function in the test-object-2
(define (test-object-hit?-2 ray interval)
  (object-hit? test-object-2 ray interval))
(define test-hit3 : Hit
  (convert (test-object-hit?-2 test-ray-2 (Pair 0.0 +inf.0))))
(define test-ray-3 : Ray (Ray fl3-zero (Float3 0.0 0.0 1.0)))
(define test-object-3 : Object 
  (make-box (Float3 0.0 0.0 2.0) (Float3 0.5 0.5 0.5) normal-material))
(: test-object-hit?-3 : Ray Interval -> Maybe-Hit)
;; extract the hit test function in the test-object-2
(define (test-object-hit?-3 ray interval)
  (object-hit? test-object-3 ray interval))
(define test-hit4 : Hit
  (convert (test-object-hit?-3 test-ray-3 (Pair 0.0 +inf.0))))
(check-within (Hit-t test-hit2) 1.5 0.1)
(check-within (Hit-pt test-hit2) (Float3 1.5 0.0 0.0) 0.1)
(check-within (Hit-norm test-hit2) (Float3 -1.0 0.0 0.0) 0.1)
(check-within (Hit-t test-hit3) 1.5 0.1)
(check-within (Hit-pt test-hit3) (Float3 0.0 1.5 0.0) 0.1)
(check-within (Hit-norm test-hit3) (Float3 0.0 -1.0 0.0) 0.1)
(check-within (Hit-t test-hit4) 1.5 0.1)
(check-within (Hit-pt test-hit4) (Float3 0.0 0.0 1.5) 0.1)
(check-within (Hit-norm test-hit4) (Float3 0.0 0.0 -1.0) 0.1)
(check-expect (test-object-hit?-2 test-ray (Pair 0.0 +inf.0)) 'Miss)
(check-expect (test-object-hit?-3 test-ray (Pair 0.0 +inf.0)) 'Miss)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-box)
