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
  ([hit? : (Ray Float Float -> Maybe-Hit)]))    ;; hit test for object

(: object-hit? : Object Ray Float Float -> Maybe-Hit)
;; invoke the hit-test function of the object on the given ray
(define (object-hit? obj ray min-t max-t)
  ((Object-hit? obj) ray min-t max-t))

;; an empty object cannot be hit by rays
(define empty-object : Object
  (Object
   (lambda ([ray : Ray] [min-t : Float] [max-t : Float]) 'Miss)))

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
        (lambda ([ray : Ray] [min-t : Float] [max-t : Float])
          (closer-hit (object-hit? obj1 ray min-t max-t)
                      (object-hit? obj2 ray min-t max-t)))))}
    (foldl closer-hit-aux empty-object xs)))

;; run tests
;; define some objects for testing purpose
(define obj1 : Object
  (Object (lambda ([ray : Ray] [min-t : Float] [max-t : Float])
            (Hit 0.5 (Float3 1.0 1.0 1.0) (Float3 0.5 0.5 0.7)
                 (flat-material rgb-red)))))
(define obj2 : Object
  (Object (lambda ([ray : Ray] [min-t : Float] [max-t : Float])
            (Hit 0.6 (Float3 2.0 2.0 2.0) (Float3 0.6 0.6 0.7)
                 (flat-material rgb-green)))))
(define obj3 : Object
  (Object (lambda ([ray : Ray] [min-t : Float] [max-t : Float])
            (Hit 0.7 (Float3 1.5 1.5 1.5) (Float3 0.6 0.7 0.7)
                 (flat-material rgb-blue)))))
(check-within (Hit-t (test-helper
                      ((Object-hit?
                        (list->object (list obj1 obj2 obj3)))
                       (make-ray fl3-zero fl3-zero) 0.0 +inf.0))) 0.5 0.01)
(check-within (Hit-pt (test-helper
                       ((Object-hit?
                         (list->object (list obj1 obj2 obj3)))
                        (make-ray fl3-zero fl3-zero) 0.0 +inf.0)))
              (Float3 1.0 1.0 1.0) 0.01)
(check-within (Hit-norm (test-helper
                         ((Object-hit?
                           (list->object (list obj1 obj2 obj3)))
                          (make-ray fl3-zero fl3-zero) 0.0 +inf.0)))
              (Float3 0.5 0.5 0.7) 0.01)
(check-expect ((Object-hit? (list->object
                             (list empty-object empty-object)))
               (make-ray fl3-zero fl3-zero) 0.0 +inf.0) 'Miss)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Object
         Maybe-Hit)

(provide object-hit?
         test-helper
         empty-object
         list->object)
