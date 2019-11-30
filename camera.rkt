#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; camera.rkt module
;; <Xinyu Liu>
;;
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load math module
;;
(require "math-util.rkt")

;; load color module
;;
(require "color.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; the representation of a Camera for the ray tracer.  The camera
;; is assumed to be at the origin looking down the -Z axis.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (focal-len : Float)])     ;; distance to image plane (in -Z direction)

;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; iterate over the pixels in the image rectangle, use the pixel
;; renderer to compute a color at every pixel, and combine
;; the list of colors at the pixels into an image.
;; return the empty image if the width or height is zero.
;; the width and height information of the image rectangle is given
;; by the camera.
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _)
     (cond
       [(or (= wid 0) (= ht 0)) empty-image]
       [else
        (local
          {(: color-list-aux : Natural Natural (Listof Color) -> (Listof Color))
           ;; a helper function that uses accumulators to track the list of
           ;; colors, and return the list of colors given the width and height.
           (define (color-list-aux col row acc)
             (if (= col 0)
                 (if (= row 0)
                     (cons (pixel-renderer 0 0) acc)
                     (color-list-aux (- wid 1) (- row 1)
                                     (cons (pixel-renderer row 0) acc)))
                 (color-list-aux (- col 1) row
                                 (cons (pixel-renderer row col) acc))))}
          (color-list->bitmap (color-list-aux (- wid 1) (- ht 1) '()) wid ht))])]))
;; Note: the check-expect tests for foreach-pixel are written after the
;; make-pixel-renderer function.

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) -> Pixel-Renderer)
;; take a function that computes an RGB value for a pixel, and another
;; RGB to Color conversion function
;; compose them to produce a pixel renderer function
(define (make-pixel-renderer pixel->RGB RGB->color)
  (lambda ([r : Natural] [c : Natural]) (RGB->color (pixel->RGB r c))))
 
;; run tests
;; define a camera for testing purpose
(define test-camera : Camera
  (Camera 400 200 1 0.0))
;; define a pixel-renderer for testing purpose
(define test-pixel-renderer
  (make-pixel-renderer 
   (lambda ([r : Natural] [c : Natural])
     (RGB (/ (->fl c) (->fl 400))
          (/ (->fl (- 200 r)) (->fl 200))
          0.2))
   rgb->color))
;; define an image for testing purpose
(define test-image : Image
  (local
    {(define wid : Natural 400)
     (define ht : Natural 200)}
    (foreach-pixel
     (Camera wid ht 1 0.0)
     (make-pixel-renderer 
      (lambda ([r : Natural] [c : Natural])
        (RGB (/ (->fl c) (->fl wid))
             (/ (->fl (- ht r)) (->fl ht))
             0.2))
      rgb->color))))
"== eyeball test foreach-pixel"
test-image
(check-expect (image-width test-image) 400)
(check-expect (image-height test-image) 200)
(check-expect (foreach-pixel (Camera 0 200 1 0.0) test-pixel-renderer)
              empty-image)
(check-expect (foreach-pixel (Camera 400 0 1 0.0) test-pixel-renderer)
              empty-image)

(: ray-for-pixel : Camera -> (Natural Natural -> Ray))
;; take a camera and return a function for generating a ray
;; for a pixel specified by its row and column.
(define (ray-for-pixel cam)
  (local
    {;; the definition of the following five variables is
     ;; provided in the write up.
     (define w : Float (->fl (Camera-wid cam)))
     (define h : Float (->fl (Camera-ht cam)))
     (define pw : Float (/ 2.0 w))
     (define x0 : Float (- (* pw 0.5) 1.0))
     (define y0 : Float (- (/ h w) (* pw 0.5)))
     (: center-coordinate : Natural Natural -> Float3)
     ;; the center of coordinate (formula given in the write up),
     ;; given the position of the pixel
     (define (center-coordinate r c)
       (Float3 (+ x0 (* c pw))
               (- y0 (* r pw))
               (- (Camera-focal-len cam))))}
    (lambda ([r : Natural] [c : Natural])
      (make-ray fl3-zero (center-coordinate r c)))))

;; run tests
(check-expect (Ray-origin ((ray-for-pixel test-camera) 0 0)) fl3-zero)
(check-within (Ray-dir ((ray-for-pixel test-camera) 0 0))
              (Float3 -0.89
                      0.44
                      0.00)
              0.1)
(check-expect (Ray-origin ((ray-for-pixel test-camera) 100 100)) fl3-zero)
(check-within (Ray-dir ((ray-for-pixel test-camera) 100 100))
              (Float3 -1.00
                      0.00
                      0.00)
              0.1)

(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam ray->RGB)
  (local
    {(: ray-tracing : Natural Natural -> Ray)
     (define ray-tracing (ray-for-pixel cam))}
    (lambda ([r : Natural] [c : Natural]) (ray->RGB (ray-tracing r c)))))
;; Note: the check-expect tests for pixel->rgb are written after the
;; ray->rgb function.

(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation. It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

;; run tests
(check-within ((pixel->rgb test-camera ray->rgb) 0 0)
              (RGB 0.63 0.78 1.00)
              0.1)
(check-within ((pixel->rgb test-camera ray->rgb) 100 100)
              (RGB 0.75 0.85 1.00)
              0.1)
(check-within ((pixel->rgb test-camera ray->rgb) 200 200)
              (RGB 1.00 1.00 1.00)
              0.1)
"== background"
(define cam-400x200x1 (Camera 400 200 1 0.25))
(foreach-pixel cam-400x200x1 (make-pixel-renderer
                              (pixel->rgb cam-400x200x1 ray->rgb)
                              rgb->color))
"== test ray-for-pixel"
(foreach-pixel cam-400x200x1 (make-pixel-renderer
                              (pixel->rgb cam-400x200x1
                                          (lambda ([ray : Ray])
                                            (match ray
                                              [(Ray _ dir)
                                               (RGB
                                                (* 0.5 (+ 1.0 (Float3-x dir)))
                                                (* 0.5 (+ 1.0 (Float3-y dir)))
                                                (* 0.5 (+ 1.0 (Float3-z dir))))])))
                              rgb->color))

(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera, return a function that maps pixel coordinates in
;; the image plane to a list of rays from the camera through the pixel.
;; The number of rays is determined by the n-samples field of the
;; Camera.
(define (rays-for-pixel cam)
  (local
    {(define w : Float (->fl (Camera-wid cam)))
     (define h : Float (->fl (Camera-ht cam)))
     (define xulc : Float (->fl (- 1)))
     (define yulc : Float (/ h w))
     (define pw : Float (/ 2.0 w))
     (: center-coordinate : Natural Natural -> Float3)
     ;; the center of coordinate (formula given in the write up),
     ;; given the row and column indices.
     (define (center-coordinate r c)
       (Float3 (+ xulc (* (+ (random) c) pw))
               (- yulc (* (+ (random) r) pw))
               (- (Camera-focal-len cam))))}
    (lambda ([r : Natural] [c : Natural])
      (build-list (Camera-n-samples cam)
                  (lambda ([int : Natural])
                    (make-ray fl3-zero (center-coordinate r c)))))))

(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
(define (antialias-pixel->rgb cam ray->RGB)
  (local
    {;; precompute the scaling factors from the camera.
     (define scale-factor : Float (if (= 0 (Camera-n-samples cam))
                                      1.0 (/ 1.0 (Camera-n-samples cam))))
     (: antialias-aux : Natural Natural -> (Listof RGB))
     ;; a helper function to return a list of RGB values in the given pixel
     (define (antialias-aux r c)
       (map ray->RGB ((rays-for-pixel cam) r c)))}
    (lambda ([r : Natural] [c : Natural])
      (rgb-scale scale-factor
                 (foldl rgb+ rgb-black (antialias-aux r c))))))

;; run tests
(check-within ((antialias-pixel->rgb test-camera ray->rgb) 0 0)
              (RGB 0.64 0.78 1.00) 0.1)
(check-within ((antialias-pixel->rgb test-camera ray->rgb) 50 50)
              (RGB 0.67 0.80 1.00) 0.1)
(check-within ((antialias-pixel->rgb test-camera ray->rgb) 100 100)
              (RGB 0.75 0.85 1.00) 0.1)
(check-within ((antialias-pixel->rgb test-camera ray->rgb) 200 200)
              (RGB 1.00 1.00 1.00) 0.1)
"== eyeball test for background (16 samples)"
(define cam-200x100x16 (Camera 200 100 16 0.25))
(foreach-pixel cam-200x100x16
               (make-pixel-renderer
                (antialias-pixel->rgb cam-200x100x16 ray->rgb)
                rgb->color))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Camera))

(provide foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
