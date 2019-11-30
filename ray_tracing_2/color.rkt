#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; color.rkt module
;;
;; This module implements the RGB type
;;
;; It is implemented using Typed Rackets unsafe flonum operations
;; for better performance.  You should NOT use these operations
;; in your own code.
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

;; include the unsafe Flonum operations for performance.
;; WARNING: you should not be using these operations directly
;; in your own code!!!!
;;
(require racket/unsafe/ops)

;; project modules
;;
(require "math-util.rkt")

;; An (RGB r g b) is a representation of color/light intensity
;; where r, g, and b are in the interval [0..1]
;;
(define-struct RGB
  ([r : Float]          ;; red channel
   [g : Float]          ;; green channel
   [b : Float]))        ;; blue channel

;; some useful color values
(define rgb-white (RGB 1.0 1.0 1.0))
(define rgb-black (RGB 0.0 0.0 0.0))
(define rgb-red   (RGB 1.0 0.0 0.0))
(define rgb-blue  (RGB 0.0 0.0 1.0))
(define rgb-green (RGB 0.0 1.0 0.0))

(: rgb-gray : Float -> RGB)
;; Gray value of given intensity
(define (rgb-gray lum) (RGB lum lum lum))

(: rgb+ : RGB RGB -> RGB)
;; Add two RGB values
(define (rgb+ rgb1 rgb2)
  (match* (rgb1 rgb2)
    [((RGB r1 g1 b1) (RGB r2 g2 b2))
     (RGB (unsafe-fl+ r1 r2) (unsafe-fl+ g1 g2) (unsafe-fl+ b1 b2))]))

(: rgb* : RGB RGB -> RGB)
;; Pointwise multiplication of two RGB values
(define (rgb* rgb1 rgb2)
  (match* (rgb1 rgb2)
    [((RGB r1 g1 b1) (RGB r2 g2 b2))
     (RGB (unsafe-fl* r1 r2) (unsafe-fl* g1 g2) (unsafe-fl* b1 b2))]))

(: rgb-scale : Float RGB -> RGB)
;; Multiply a scalar and an RGB value
(define (rgb-scale s rgb)
  (match rgb
    [(RGB r g b) (RGB (unsafe-fl* s r) (unsafe-fl* s g) (unsafe-fl* s b))]))

(: rgb-scale+ : RGB Float RGB -> RGB)
;; Add the first argument to the scaled second argument
(define (rgb-scale+ rgb1 s rgb2)
  (match* (rgb1 rgb2)
    [((RGB r1 g1 b1) (RGB r2 g2 b2))
     (RGB (unsafe-fl+ r1 (unsafe-fl* s r2))
          (unsafe-fl+ g1 (unsafe-fl* s g2))
          (unsafe-fl+ b1 (unsafe-fl* s b2)))]))

(: rgb-lerp : RGB Float RGB -> RGB)
;; linear interpolation of RGB values
;;
(define (rgb-lerp u t v)
  (match* (u v)
    [((RGB r1 g1 b1) (RGB r2 g2 b2))
     (RGB (fl-lerp r1 t r2)
	  (fl-lerp g1 t g2)
	  (fl-lerp b1 t b2))]))

(: rgb->color : RGB -> Color)
;; convert an RGB value to an image color value without gamma correction
(define (rgb->color rgb)
  (local
    {(: ->byte : Float -> Byte)
     (define (->byte x)
       (local
         {(define b (exact-floor (unsafe-fl* 255.99 x)))}
         (cond
           [(<= b 0) 0]
           [(<= 255 b) 255]
           [else b])))}
    (match rgb
      [(RGB r g b) (color (->byte r) (->byte g) (->byte b))])))

(: gamma-rgb->color : RGB -> Color)
;; convert an RGB value to an image color value with a gamma correction of 1/2
(define (gamma-rgb->color rgb)
  (local
    {(: ->byte : Float -> Byte)
     (define (->byte x)
       (local
         {(define b (if (unsafe-fl<= x 0.0)
                        0
                        (exact-floor (unsafe-fl* 255.99 (unsafe-flsqrt x)))))}
         (cond
           [(< b 0) 0]
           [(< 255 b) 255]
           [else b])))}
    (match rgb
      [(RGB r g b) (color (->byte r) (->byte g) (->byte b))])))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out RGB))

(provide rgb-white
         rgb-black
         rgb-red
         rgb-blue
         rgb-green
         rgb-gray)

(provide rgb+
         rgb*
         rgb-scale
         rgb-scale+
         rgb-lerp
         rgb->color
         gamma-rgb->color)
