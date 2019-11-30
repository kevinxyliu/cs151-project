#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; scene.rkt module
;;
;; This module contains the definition of scene objects and the scene
;; type.  We leave the mapping of a scene (list of scene objects) to
;; an Object to the kd-tree.rkt module.
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
(require "sphere.rkt")
(require "box.rkt")

;; A sphere description
(define-struct Sphere
  ([center : Float3]            ;; the coordinates of the center of the sphere
   [radius : Float]             ;; the radius of the sphere
   [surface : Material]))       ;; the surface material of the sphere

;; A box description
(define-struct Box
  ([center : Float3]            ;; center of axis-aligned box
   [half-dim : Float3]          ;; distance from center to sides in X, Y, and Z
   ;; directions.
   [surface : Material]))       ;; the surface material of the box

;; boxes that have been rotated and translated
(define-type Oriented-Box (U Rotate-Box Translate-Box Box))

(define-struct Translate-Box
  ([offset : Float3]            ;; the amount of translation
   [box : Oriented-Box]))       ;; the box being rotated

(define-struct Rotate-Box
  ([axis : Axis]                ;; the axis of rotation
   [angle : Float]              ;; counter-clockwise rotation angle in degrees
   [box : Oriented-Box]))       ;; the box being rotated

;; the basic objects in a scene are either spheres or boxes
(define-type Scene-Object (U Sphere Oriented-Box))

;; A scene is a list of objects
(define-type Scene (Listof Scene-Object))

(: scene-object->object : Scene-Object -> Object)
;; make the object for an Instance
(define (scene-object->object obj)
  (match obj
    [(Sphere center rad mat) (make-sphere center rad mat)]
    [(Box center half-wid mat) (make-box center half-wid mat)]
    [(Translate-Box offset box) (obj-translate offset (scene-object->object box))]
    [(Rotate-Box axis angle box)
     (match axis
       ['X (obj-rotate-x angle (scene-object->object box))]
       ['Y (obj-rotate-y angle (scene-object->object box))]
       ['Z (obj-rotate-z angle (scene-object->object box))])]))

;; run tests
(: convert : Maybe-Hit -> Hit)
;; a helper function to convert a Maybe-Hit value into
;; a Hit value
(define (convert hit)
  (if (Hit? hit) hit (error "Not a Hit type")))
;; define several scene-objects for testing purpose
(define scene-obj-1 : Sphere
  (Sphere fl3-zero 1.0 normal-material))
(define scene-obj-2 : Box
  (Box (Float3 0.0 0.0 0.0) (Float3 1.0 1.0 1.0) normal-material))
(define scene-obj-3 : Translate-Box
  (Translate-Box (Float3 -3.0 -3.0 -3.0) scene-obj-2))
(define scene-obj-4 : Rotate-Box
  (Rotate-Box 'X 60.0 scene-obj-2))
;; define a hit information.
(define hit-1 : Hit
  (convert (object-hit? (scene-object->object scene-obj-1)
                        (Ray fl3-zero (Float3 1.0 0.0 0.0))
                        (Pair -inf.0 +inf.0))))
(check-within (Hit-t hit-1) -1.0 0.1)
(check-within (Hit-pt hit-1) (Float3 -1.0 0.0 0.0) 0.1)
(check-within (Hit-norm hit-1) (Float3 -1.0 0.0 0.0) 0.1)
(check-expect (object-hit? (scene-object->object scene-obj-2)
                           (Ray fl3-zero (Float3 1.0 0.0 0.0))
                           (Pair -inf.0 +inf.0)) 'Miss)
(check-expect (object-hit? (scene-object->object scene-obj-3)
                           (Ray fl3-zero (Float3 1.0 0.0 0.0))
                           (Pair -inf.0 +inf.0)) 'Miss)
(check-expect (object-hit? (scene-object->object scene-obj-4)
                           (Ray fl3-zero (Float3 1.0 0.0 0.0))
                           (Pair -inf.0 +inf.0)) 'Miss)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Sphere)
         (struct-out Box)
         Oriented-Box
         (struct-out Translate-Box)
         (struct-out Rotate-Box)
         Scene-Object
         Scene)

(provide scene-object->object)
