#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; <Xinyu Liu>
;;
;; object.rkt module
;; This module contains the definition of the graphical objects in the scene
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; include the Flonum bindings for sqrt, etc.
;;
(require typed/racket/flonum)

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "color.rkt")
(require "material.rkt")

;; a Maybe-Hit is either 'Miss or a (Hit ...) record.  It is used
;; to represent the result of an intersection test between a ray
;; and an object.
;;
(define-type Maybe-Hit (U 'Miss Hit))

;; An (Object hit?) represents a collection of geometric objects as
;; a hit-test function
;;
(define-struct Object
  ([hit? : (Ray Interval -> Maybe-Hit)]))    ;; hit test for object

(: object-hit? : Object Ray Interval -> Maybe-Hit)
;; invoke the hit-test function of the object on the given ray
(define (object-hit? obj ray min-max-t)
  ((Object-hit? obj) ray min-max-t))

;; an empty object cannot be hit by rays
(define empty-object : Object
  (Object
   (lambda ([ray : Ray] [min-max-t : Interval]) 'Miss)))

(: closer-hit : Maybe-Hit Maybe-Hit -> Maybe-Hit)
;; return the closer of two Maybe-Hit values (misses are at infinity)
(define (closer-hit hit1 hit2)
  (match* (hit1 hit2)
    [((Hit t1 _ _ _) (Hit t2 _ _ _)) (if (< t1 t2) hit1 hit2)]
    [((Hit t1 _ _ _) 'Miss) hit1]
    [('Miss (Hit t2 _ _ _)) hit2]
    [(_ _) 'Miss]))

;; run tests
(: test-helper : Maybe-Hit -> Hit)
;; a helper function to convert Maybe-Hit to Hit for testing purpose
(define (test-helper hit)
  (if (Hit? hit) hit (error "Not a Hit type")))
;; define two hits for testing
(define test-hit1 (Hit 5.0 (Float3 5.0 5.0 5.0)
                       (Float3 1.0 0.0 0.0)
                       (flat-material rgb-red)))
(define test-hit2 (Hit 6.0 (Float3 4.0 4.0 4.0)
                       (Float3 0.0 1.0 0.0)
                       normal-material))
(check-within (Hit-t (test-helper (closer-hit test-hit1 test-hit2)))
              5.0 0.1)
(check-within (Hit-pt (test-helper (closer-hit test-hit1 test-hit2)))
              (Float3 5.0 5.0 5.0) 0.1)
(check-within (Hit-norm (test-helper (closer-hit test-hit1 test-hit2)))
              (Float3 1.0 0.0 0.0) 0.1)
(check-expect (closer-hit 'Miss 'Miss) 'Miss)

(: list->object : (Listof Object) -> Object)
;; make an object from a list of objects.
(define (list->object xs)
  (local
    {(: closer-hit-aux : Object Object -> Object)
     ;; a helper function to make an object from two objects.
     (define (closer-hit-aux obj1 obj2)
       (Object
        (lambda ([ray : Ray] [min-max-t : Interval])
          (closer-hit (object-hit? obj1 ray min-max-t)
                      (object-hit? obj2 ray min-max-t)))))}
    (foldl closer-hit-aux empty-object xs)))

;; run tests
;; define some objects for testing purpose
(define obj1 : Object
  (Object (lambda ([ray : Ray] [min-max-t : Interval])
            (Hit 0.5 (Float3 1.0 1.0 1.0) (Float3 0.5 0.5 0.7)
                 (flat-material rgb-red)))))
(define obj2 : Object
  (Object (lambda ([ray : Ray] [min-max-t : Interval])
            (Hit 0.6 (Float3 2.0 2.0 2.0) (Float3 0.6 0.6 0.7)
                 (flat-material rgb-green)))))
(define obj3 : Object
  (Object (lambda ([ray : Ray] [min-max-t : Interval])
            (Hit 0.7 (Float3 1.5 1.5 1.5) (Float3 0.6 0.7 0.7)
                 (flat-material rgb-blue)))))
(check-within (Hit-t (test-helper
                      ((Object-hit?
                        (list->object (list obj1 obj2 obj3)))
                       (make-ray fl3-zero fl3-zero) (Pair 0.0 +inf.0)))) 0.5 0.01)
(check-within (Hit-pt (test-helper
                       ((Object-hit?
                         (list->object (list obj1 obj2 obj3)))
                        (make-ray fl3-zero fl3-zero) (Pair 0.0 +inf.0))))
              (Float3 1.0 1.0 1.0) 0.01)
(check-within (Hit-norm (test-helper
                         ((Object-hit?
                           (list->object (list obj1 obj2 obj3)))
                          (make-ray fl3-zero fl3-zero) (Pair 0.0 +inf.0))))
              (Float3 0.5 0.5 0.7) 0.01)
(check-expect ((Object-hit? (list->object
                             (list empty-object empty-object)))
               (make-ray fl3-zero fl3-zero) (Pair 0.0 +inf.0)) 'Miss)

(: obj-translate : Float3 Object -> Object)
;; take a vector offset and an object, and translates the object by
;; the offset.
(define (obj-translate offset obj)
  (Object
   (lambda ([ray : Ray] [min-max-t : Interval])
     (local
       {;; define the new ray after the translation.
        (define new-ray : Ray
          (Ray (fl3- (Ray-origin ray) offset) (Ray-dir ray)))}
       (match (object-hit? obj new-ray min-max-t)
         [(Hit t pt norm mat) (Hit t (fl3+ pt offset) norm mat)]
         ['Miss 'Miss])))))

;; run tests
;; define two objects after translation for testing purpose
(define obj-translate-1 : Object
  (obj-translate (Float3 1.0 1.0 1.0) obj1))
(define obj-translate-2 : Object
  (obj-translate (Float3 3.0 3.0 3.0) empty-object))
(check-within (Hit-t (test-helper
                      (object-hit? obj-translate-1
                                   (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                   (Pair 0.0 +inf.0)))) 0.5 0.1)
(check-within (Hit-pt (test-helper
                       (object-hit? obj-translate-1
                                    (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                    (Pair 0.0 +inf.0))))
              (Float3 2.0 2.0 2.0) 0.1)
(check-within (Hit-norm (test-helper
                         (object-hit? obj-translate-1
                                      (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                      (Pair 0.0 +inf.0))))
              (Float3 0.5 0.5 0.7) 0.1)
(check-expect (object-hit? obj-translate-2
                           (Ray fl3-zero (Float3 -2.0 -2.0 -2.0))
                           (Pair 0.0 +inf.0)) 'Miss)

(: obj-rotate-x : Float Object -> Object)
;; take an angle and an object and return the rotated object by 'X axis.
(define (obj-rotate-x theta obj)
  (local
    {(: rotate-by-x-pos : Float3 -> Float3)
     ;; partially apply the fl3-rotate-x function to
     ;; the angle outside of the hit test (positive theta)
     (define rotate-by-x-pos (fl3-rotate-x theta))
     (: rotate-by-x-neg : Float3 -> Float3)
     ;; partially apply the fl3-rotate-x function to
     ;; the angle outside of the hit test (negative theta)
     (define rotate-by-x-neg (fl3-rotate-x (- theta)))}
    (Object
     (lambda ([ray : Ray] [min-max-t : Interval])
       (local
         {(define new-ray : Ray
            (Ray (rotate-by-x-neg (Ray-origin ray))
                 (rotate-by-x-neg (Ray-dir ray))))}
         (match (object-hit? obj new-ray min-max-t)
           [(Hit t pt norm mat)
            (Hit t (rotate-by-x-pos pt) (rotate-by-x-pos norm) mat)]
           ['Miss 'Miss]))))))

;; run tests
;; define two objects after rotation for testing purpose
(define obj-rotate-x-1 : Object
  (obj-rotate-x 30.0 obj1))
(define obj-rotate-x-2 : Object
  (obj-rotate-x 45.0 empty-object))
(check-within (Hit-t (test-helper
                      (object-hit? obj-rotate-x-1
                                   (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                   (Pair 0.0 +inf.0)))) 0.5 0.1)
(check-within (Hit-pt (test-helper
                       (object-hit? obj-rotate-x-1
                                    (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                    (Pair 0.0 +inf.0))))
              (Float3 1.0 0.37 1.37) 0.1)
(check-within (Hit-norm (test-helper
                         (object-hit? obj-rotate-x-1
                                      (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                      (Pair 0.0 +inf.0))))
              (Float3 0.5 0.08 0.86) 0.1)
(check-expect (object-hit? obj-rotate-x-2
                           (Ray fl3-zero (Float3 -2.0 -2.0 -2.0))
                           (Pair 0.0 +inf.0)) 'Miss)

(: obj-rotate-y : Float Object -> Object)
;; take an angle and an object and return the rotated object by 'Y axis.
(define (obj-rotate-y theta obj)
  (local
    {(: rotate-by-y-pos : Float3 -> Float3)
     ;; partially apply the fl3-rotate-y function to
     ;; the angle outside of the hit test (positive theta)
     (define rotate-by-y-pos (fl3-rotate-y theta))
     (: rotate-by-y-neg : Float3 -> Float3)
     ;; partially apply the fl3-rotate-y function to
     ;; the angle outside of the hit test (negative theta)
     (define rotate-by-y-neg (fl3-rotate-y (- theta)))}
    (Object
     (lambda ([ray : Ray] [min-max-t : Interval])
       (local
         {(define new-ray : Ray
            (Ray (rotate-by-y-neg (Ray-origin ray))
                 (rotate-by-y-neg (Ray-dir ray))))}
         (match (object-hit? obj new-ray min-max-t)
           [(Hit t pt norm mat)
            (Hit t (rotate-by-y-pos pt) (rotate-by-y-pos norm) mat)]
           ['Miss 'Miss]))))))

;; run tests
;; define two objects after rotation for testing purpose
(define obj-rotate-y-1 : Object
  (obj-rotate-y 30.0 obj1))
(define obj-rotate-y-2 : Object
  (obj-rotate-y 45.0 empty-object))
(check-within (Hit-t (test-helper
                      (object-hit? obj-rotate-y-1
                                   (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                   (Pair 0.0 +inf.0)))) 0.5 0.1)
(check-within (Hit-pt (test-helper
                       (object-hit? obj-rotate-y-1
                                    (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                    (Pair 0.0 +inf.0))))
              (Float3 1.37 1.0 0.37) 0.1)
(check-within (Hit-norm (test-helper
                         (object-hit? obj-rotate-y-1
                                      (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                      (Pair 0.0 +inf.0))))
              (Float3 0.78 0.5 0.36) 0.1)
(check-expect (object-hit? obj-rotate-y-2
                           (Ray fl3-zero (Float3 -2.0 -2.0 -2.0))
                           (Pair 0.0 +inf.0)) 'Miss)

(: obj-rotate-z : Float Object -> Object)
;; take an angle and an object and return the rotated object by 'Z axis.
(define (obj-rotate-z theta obj)
  (local
    {(: rotate-by-z-pos : Float3 -> Float3)
     ;; partially apply the fl3-rotate-z function to
     ;; the angle outside of the hit test (positive theta)
     (define rotate-by-z-pos (fl3-rotate-z theta))
     (: rotate-by-z-neg : Float3 -> Float3)
     ;; partially apply the fl3-rotate-z function to
     ;; the angle outside of the hit test (negative theta)
     (define rotate-by-z-neg (fl3-rotate-z (- theta)))}
    (Object
     (lambda ([ray : Ray] [min-max-t : Interval])
       (local
         {(define new-ray : Ray
            (Ray (rotate-by-z-neg (Ray-origin ray))
                 (rotate-by-z-neg (Ray-dir ray))))}
         (match (object-hit? obj new-ray min-max-t)
           [(Hit t pt norm mat)
            (Hit t (rotate-by-z-pos pt) (rotate-by-z-pos norm) mat)]
           ['Miss 'Miss]))))))

;; run tests
;; define two objects after rotation for testing purpose
(define obj-rotate-z-1 : Object
  (obj-rotate-z 30.0 obj1))
(define obj-rotate-z-2 : Object
  (obj-rotate-z 45.0 empty-object))
(check-within (Hit-t (test-helper
                      (object-hit? obj-rotate-z-1
                                   (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                   (Pair 0.0 +inf.0)))) 0.5 0.1)
(check-within (Hit-pt (test-helper
                       (object-hit? obj-rotate-z-1
                                    (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                    (Pair 0.0 +inf.0))))
              (Float3 0.37 1.37 1.0) 0.1)
(check-within (Hit-norm (test-helper
                         (object-hit? obj-rotate-z-1
                                      (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                      (Pair 0.0 +inf.0))))
              (Float3 0.18 0.68 0.7) 0.1)
(check-expect (object-hit? obj-rotate-z-2
                           (Ray fl3-zero (Float3 -2.0 -2.0 -2.0))
                           (Pair 0.0 +inf.0)) 'Miss)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Object
         Maybe-Hit)

(provide object-hit?
         test-helper
         empty-object
         list->object
         obj-translate
         obj-rotate-x
         obj-rotate-y
         obj-rotate-z)