#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; material.rkt module
;; <Xinyu Liu>
;;
;; This module contains the definition of the Hit and Material types
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
(require "color.rkt")

;; A (Hit ...) records an intersection (aka "hit") between a Ray R
;; and an object.
(define-struct Hit
  ([t : Float]              ;; value of t such that R(t) is the point
   ;; where the ray R intersects the object
   [pt : Float3]            ;; The point of intersection
   [norm : Float3]          ;; Unit-length normal vector at pt
   [material : Material]))  ;; The material of the object at pt

;; A (Hit-Info rgb vec) value represents the information about a ray's hit on the
;; surface of an object.  The rgb value is the attenuation factor and the ray
;; is the reflection/scatter ray for the surface
(define-struct Hit-Info
  ([aten : RGB]             ;; attenuation factor
   [reflect-ray : Ray]))    ;; ray that is scattered or reflected from surface

;; A Material is a struct containing a scatter function that takes a
;; Ray and a Hit record for the Ray and returns 'None if there is no
;; color contribution and returns some hit info otherwise
;;
(define-struct Material
  ([scatter : (Ray Hit -> (Option Hit-Info))]))

(: get-hit-info : Hit Ray -> (Option Hit-Info))
;; give a hit record and a ray, get the optional hit info for the
;; hit record's material
(define (get-hit-info hit ray)
  (match hit
    [(Hit _ _ _ (Material scat)) (scat ray hit)]))

(: make-info : RGB Ray -> (Option Hit-Info))
;; helper function that builds some hit info
(define (make-info rgb ray) (Some (Hit-Info rgb ray)))

(: flat-material : RGB -> Material)
;; flat shading with the given color
(define (flat-material rgb)
  (Material
   (lambda ([ray : Ray] [hit : Hit])
     (make-info rgb (Ray fl3-zero fl3-zero)))))

(: normal-material : Material)
;; a normal material
;; compute the surface color by mapping the <x, y, z> components of
;; the normal vector to the red, green, and blue channels of an RGB value.
(define normal-material
  (Material
   (lambda ([ray : Ray] [hit : Hit])
     (local
       {(define r : Float (* 0.5 (+ 1.0 (Float3-x (Hit-norm hit)))))
        (define g : Float (* 0.5 (+ 1.0 (Float3-y (Hit-norm hit)))))
        (define b : Float (* 0.5 (+ 1.0 (Float3-z (Hit-norm hit)))))}
       (make-info (RGB r g b) (Ray fl3-zero fl3-zero))))))

;; run tests
;; define some hit values for testing purpose
(define test-hit1 : Hit
  (Hit 1.0 (Float3 1.0 1.0 1.0)
       (Float3 -0.5 -0.5 -0.7)
       (flat-material (RGB 1.0 0.0 0.0))))
(define test-hit2 : Hit
  (Hit 2.0 (Float3 1.5 1.0 0.5)
       (Float3 0.5 0.7 0.7)
       (flat-material (RGB 1.0 0.0 0.0))))
;; define some rays for testing purpose
(define test-ray1 : Ray
  (make-ray fl3-zero (Float3 1.0 1.0 1.0)))
(define test-ray2 : Ray
  (make-ray fl3-zero (Float3 1.5 1.5 1.0)))
(check-within ((Material-scatter normal-material) test-ray1 test-hit1)
              (Some (Hit-Info (RGB 0.25 0.25 0.15)
                              (Ray fl3-zero fl3-zero)))
              0.01)
(check-within ((Material-scatter normal-material) test-ray1 test-hit2)
              (Some (Hit-Info (RGB 0.75 0.85 0.85)
                              (Ray fl3-zero fl3-zero)))
              0.01)
(check-within ((Material-scatter normal-material) test-ray2 test-hit1)
              (Some (Hit-Info (RGB 0.25 0.25 0.15)
                              (Ray fl3-zero fl3-zero)))
              0.01)
(check-within ((Material-scatter normal-material) test-ray2 test-hit2)
              (Some (Hit-Info (RGB 0.75 0.85 0.85)
                              (Ray fl3-zero fl3-zero)))
              0.01)

(: lambertian-material : RGB -> Material)
;; A Lambertian surface of the given color
(define (lambertian-material rgb)
  (Material
   (lambda ([ray : Ray] [hit : Hit])
     (match hit
       [(Hit _ pt norm _)
        (local
          {(define S : Float3 (fl3+ norm (random-point-in-sphere)))
           (define R : Ray (make-ray pt S))}
          (make-info rgb R))]))))

;; run tests
(: convert : (Option Hit-Info) -> (Some Hit-Info))
;; define a helper function to convert (Option Hit-Info) into (Hit-Info)
;; the function is specifically defined for testing purpose
(define (convert hit)
  (match hit
    [(Some info) hit]
    [_ (error "Not a Hit-Info")]))
(check-expect (Hit-Info-aten (Some-value
                              (convert
                               ((Material-scatter (lambertian-material rgb-red))
                                test-ray1 test-hit1)))) rgb-red)
(check-expect (Hit-Info-aten (Some-value
                              (convert
                               ((Material-scatter (lambertian-material rgb-white))
                                test-ray1 test-hit1)))) rgb-white)
(check-within (Ray-origin (Hit-Info-reflect-ray
                           (Some-value
                            (convert
                             ((Material-scatter (lambertian-material rgb-red))
                              test-ray1 test-hit1))))) (Float3 1.0 1.0 1.0) 0.1)
(check-within (Ray-origin (Hit-Info-reflect-ray
                           (Some-value
                            (convert
                             ((Material-scatter (lambertian-material rgb-red))
                              test-ray1 test-hit2))))) (Float3 1.5 1.0 0.5) 0.1)

(: metal-material : RGB Float -> Material)
;; a metalic surface
(define (metal-material rgb fuzz)
  (Material
   (lambda ([ray : Ray] [hit : Hit])
     (match hit
       [(Hit _ pt norm _)
        (local
          {(define R : Float3 (fl3-reflect (Ray-dir ray) norm))
           (define Q : Float3 (random-point-in-sphere))
           (define S : Float3 (fl3+ R (fl3-scale fuzz Q)))}
          (if (> (fl3-dot norm S) 0)
              (make-info rgb (make-ray pt S))
              'None))]))))
;; run tests
(check-expect
 ((Material-scatter (metal-material rgb-red 0.5))
  test-ray1 test-hit2) 'None)
(check-within (Ray-origin (Hit-Info-reflect-ray
                           (Some-value
                            (convert
                             ((Material-scatter (metal-material rgb-red 0.5))
                              test-ray1 test-hit1))))) (Float3 1.0 1.0 1.0) 0.1)
(check-expect (Hit-Info-aten
               (Some-value
                (convert
                 ((Material-scatter (metal-material rgb-red 0.5))
                  test-ray1 test-hit1)))) rgb-red)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Hit)
         (struct-out Hit-Info)
         (struct-out Material))

(provide get-hit-info
         flat-material
         normal-material
         lambertian-material
         metal-material)
