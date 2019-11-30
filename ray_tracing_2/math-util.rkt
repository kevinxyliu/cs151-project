#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; math-util.rkt module
;;
;; This module contains utility types and functions for 3D math.
;;
;; It is implemented using Typed Rackets unsafe flonum operations
;; for better performance.  Note that you should NOT use these
;; operations in your own code.
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; include the Flonum bindings for sqrt, etc.
;;
(require typed/racket/flonum)

;; include the unsafe Flonum operations for performance.
;; WARNING: you should not be using these operations directly
;; in your own code!!!!
;;
(require racket/unsafe/ops)

;; project modules
;;
(require "util.rkt")

;;;;;;;;;; Float operations

(: fl-lerp : Float Float Float -> Float)
;; linear interpolation for floats
;;
(define (fl-lerp a t b)
  (unsafe-fl+ (unsafe-fl* (unsafe-fl- 1.0 t) a) (unsafe-fl* t b)))

;;;;;;;;;; Float3 operations

;; A Float3 is a triple of floating-point numbers that can
;; represent a 3D point or vector.
;;
;; Note: we use Float instead of Real for performance reasons
;;
(define-struct Float3
  ([x : Float]
   [y : Float]
   [z : Float]))

;; the origin or zero-length 3D vector
(define fl3-zero : Float3 (Float3 0.0 0.0 0.0))

(: fl3-negate : Float3 -> Float3)
;; negate a 3D vector
(define (fl3-negate v)
  (match v [(Float3 x y z) (Float3 (- x) (- y) (- z))]))

(: fl3+ : Float3 Float3 -> Float3)
;; add two vectors u and v
(define (fl3+ u v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (Float3 (unsafe-fl+ x1 x2) (unsafe-fl+ y1 y2) (unsafe-fl+ z1 z2))]))

(: fl3- : Float3 Float3 -> Float3)
;; subtract the vector v from u
(define (fl3- u v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (Float3 (unsafe-fl- x1 x2) (unsafe-fl- y1 y2) (unsafe-fl- z1 z2))]))

(: fl3-scale : Float Float3 -> Float3)
;; multiply the vector v by the scalar s
(define (fl3-scale s v)
  (match v
    [(Float3 x y z) (Float3 (unsafe-fl* s x) (unsafe-fl* s y) (unsafe-fl* s z))]))

(: fl3-scale+ : Float3 Float Float3 -> Float3)
;; Add the first argument to the scaled second argument
(define (fl3-scale+ u s v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (Float3 (unsafe-fl+ x1 (unsafe-fl* s x2))
             (unsafe-fl+ y1 (unsafe-fl* s y2))
             (unsafe-fl+ z1 (unsafe-fl* s z2)))]))

(: fl3-dot : Float3 Float3 -> Float)
;; compute the dot product of u and v
(define (fl3-dot u v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (unsafe-fl+ (unsafe-fl* x1 x2)
                 (unsafe-fl+ (unsafe-fl* y1 y2)
                             (unsafe-fl* z1 z2)))]))

(: fl3-cross : Float3 Float3 -> Float3)
;; compute the cross product of u and v
(define (fl3-cross u v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (Float3 (unsafe-fl- (unsafe-fl* y1 z2) (unsafe-fl* z1 y2))
             (unsafe-fl- (unsafe-fl* z1 x2) (unsafe-fl* x1 z2))
             (unsafe-fl- (unsafe-fl* x1 y2) (unsafe-fl* y1 x2)))]))

(: fl3-length : Float3 -> Float)
;; compute the length of v
(define (fl3-length v)
  (flsqrt (fl3-dot v v)))

(: fl3-normalize : Float3 -> Float3)
;; normalize v; return fl3-zero if the length of v is close to 0
(define (fl3-normalize v)
  (local
   {(define len2 : Float (fl3-dot v v))}
   (if (unsafe-fl<= len2 0.000001)
       fl3-zero
       (fl3-scale (unsafe-fl/ 1.0 (unsafe-flsqrt len2)) v))))

(: fl3-reflect : Float3 Float3 -> Float3)
;; given a unit vector and a unit normal that defines a surface, return the
;; unit reflection vector
(define (fl3-reflect v norm)
  (fl3- v (fl3-scale (unsafe-fl* 2.0 (fl3-dot v norm)) norm)))

(: fl3-lerp : Float3 Float Float3 -> Float3)
;; linear interpolation: (1-t)*u + t*v
;;
(define (fl3-lerp u t v)
  (match* (u v)
    [((Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (Float3 (fl-lerp x1 t x2)
             (fl-lerp y1 t y2)
             (fl-lerp z1 t z2))]))

(: fl3-rotate-x : Float -> Float3 -> Float3)
;; rotate the point around the X axis by the angle specified in degrees
(define (fl3-rotate-x angle)
  (local
    {(define theta (degrees->radians angle))
     (define sin-theta (sin theta))
     (define cos-theta (cos theta))}
    (lambda [(v : Float3)]
      (match v
        [(Float3 x y z) (Float3 x
                                (- (* cos-theta y) (* sin-theta z))
                                (+ (* sin-theta y) (* cos-theta z)))]))))

(: fl3-rotate-y : Float -> Float3 -> Float3)
;; rotate the point around the Y axis by the angle specified in degrees
(define (fl3-rotate-y angle)
  (local
    {(define theta (degrees->radians angle))
     (define sin-theta (sin theta))
     (define cos-theta (cos theta))}
    (lambda [(v : Float3)]
      (match v
        [(Float3 x y z) (Float3 (+ (* cos-theta x) (* sin-theta z))
                                y
                                (- (* cos-theta z) (* sin-theta x)))]))))

(: fl3-rotate-z : Float -> Float3 -> Float3)
;; rotate the point around the Z axis by the angle specified in degrees
(define (fl3-rotate-z angle)
  (local
    {(define theta (degrees->radians angle))
     (define sin-theta (sin theta))
     (define cos-theta (cos theta))}
    (lambda [(v : Float3)]
      (match v
        [(Float3 x y z) (Float3 (- (* cos-theta x) (* sin-theta y))
                                (+ (* sin-theta x) (* cos-theta y))
                                z)]))))

(: random-point-in-sphere : -> Float3)
;; generate a random point in the unit sphere
;;
(define (random-point-in-sphere)
  (local
    {(: rand-float3 : -> Float3)
     ;; generate a random point in the 2x2x2 cube at the origin
     (define (rand-float3)
       (Float3 (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0)
               (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0)
               (unsafe-fl- (unsafe-fl* 2.0 (random)) 1.0)))
     (: lp : -> Float3)
     ;; generate random points until one lands in the unit sphere
     (define (lp)
       (local
         {(define pt : Float3 (rand-float3))}
         (if (< (fl3-dot pt pt) 1.0) pt (lp))))}
    (lp)))

;;;;;;;;;; Ray operations

;; A (Ray origin dir) represents a ray in 3D space, where origin
;; is the origin of the ray and dir is a unit vector specifying
;; the direction.
;;
(define-struct Ray
  ([origin : Float3]  ;; the origin of the ray
   [dir : Float3]))   ;; the direction of the ray (unit vector)

(: make-ray : Float3 Float3 -> Ray)
;; make a ray with the given origin and direction vector.
;; This function normalizes the direction to a unit vector
(define (make-ray pt dir)
  (Ray pt (fl3-normalize dir)))

(: ray-point-at : Ray Float -> Float3)
;; return the point on the ray at the given distance
;; from the origin
;;
(define (ray-point-at ray t)
  (match ray
    [(Ray origin dir) (fl3+ origin (fl3-scale t dir))]))

;;;;;;;;;; Intervals

;; An interval on the real number line
(define-type Interval (Pair Float Float))

(: within? : Float Interval -> Boolean)
;; is the first argument within the interval specified by the second argument?
;;
(define (within? x interval)
  (<= (Pair-fst interval) x (Pair-snd interval)))

(: interval-merge : Interval Interval -> Interval)
;; given two intervals, return an interval that covers both of them
;;
(define (interval-merge i1 i2)
  (match* (i1 i2)
    [((Pair min1 max1) (Pair min2 max2))
     (Pair (min min1 min2) (max max1 max2))]))

(: interval-intersect : Interval Interval -> Interval)
;; given two intervals, return an interval that is the intersection.
;; if the intervals are disjoint, then the result is undefined
;;
(define (interval-intersect i1 i2)
  (match* (i1 i2)
    [((Pair min1 max1) (Pair min2 max2))
     (Pair (max min1 min2) (min max1 max2))]))

(: interval-width : Interval -> Float)
;; return the width of an interval
;;
(define (interval-width i)
  (unsafe-fl- (Pair-snd i) (Pair-fst i)))

(: interval-center : Interval -> Float)
;; return the center of an interval
;;
(define (interval-center i)
  (unsafe-fl* 0.5 (unsafe-fl+ (Pair-fst i) (Pair-snd i))))

;;;;;;;;;; Axis projection support

;; An Axis is one of 'X, 'Y, or 'Z
;;
(define-type Axis (U 'X 'Y 'Z))

;; A Ray1 is the projection of a Ray onto an Axis
;;
(define-struct Ray1
  ([origin : Float]
   [delta : Float]))

(: fl3-project : Float3 Axis -> Float)
;; project a Float3 onto one of the principal axes
;;
(define (fl3-project v axis)
  (match* (v axis)
    [((Float3 x _ _) 'X) x]
    [((Float3 _ y _) 'Y) y]
    [((Float3 _ _ z) 'Z) z]))

(: ray-project : Ray Axis -> Ray1)
;; project a Ray onto one of the principal axes
;;
(define (ray-project ray axis)
  (Ray1 (fl3-project (Ray-origin ray) axis)
        (fl3-project (Ray-dir ray) axis)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Float3)
         (struct-out Ray)
         Interval
         Axis
         (struct-out Ray1))

(provide
 ->fl
 fl-lerp
 (rename-out (unsafe-flsqrt fl-sqrt))
 fl3-zero
 fl3-negate
 fl3+
 fl3-
 fl3-scale
 fl3-scale+
 fl3-dot
 fl3-cross
 fl3-length
 fl3-normalize
 fl3-reflect
 fl3-lerp
 fl3-rotate-x
 fl3-rotate-y
 fl3-rotate-z
 random-point-in-sphere
 make-ray
 ray-point-at
 within?
 interval-merge
 interval-intersect
 interval-width
 interval-center
 fl3-project
 ray-project)
