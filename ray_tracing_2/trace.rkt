#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; trace.rkt module
;; <Xinyu Liu>
;;
;; Ray casting and recursive ray tracing
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
(require "camera.rkt")
(require "material.rkt")
(require "object.rkt")
(require "sphere.rkt")
(require "box.rkt")
(require "scene.rkt")
(require "kd-tree.rkt")

(: cast-ray-in-world : Object -> Ray -> RGB)
;; take an object and a ray and then tests the ray for intersection with
;; the object.
(define (cast-ray-in-world obj)
  (lambda ([ray : Ray])
    (local
      {(define hit-or-miss : Maybe-Hit (object-hit? obj ray (Pair 0.0 +inf.0)))}
      (cond
        [(Hit? hit-or-miss) (match (get-hit-info hit-or-miss ray)
                              ['None (ray->rgb ray)]
                              [(Some (Hit-Info rgb _)) rgb])]
        [else (ray->rgb ray)]))))

;; run tests
(define sphere-0
  (make-sphere (Float3 0.0 0.0 -2.0) 0.5 (flat-material (RGB 1.0 0.0 0.0))))
(check-within ((cast-ray-in-world sphere-0) (Ray fl3-zero (Float3 1.0 0.0 0.0)))
              (RGB 0.75 0.85 1.0) 0.1)
(check-within ((cast-ray-in-world sphere-0) (Ray fl3-zero (Float3 0.0 1.0 0.0)))
              (RGB 0.5 0.7 1.0) 0.1)
(check-within ((cast-ray-in-world sphere-0) (Ray fl3-zero (Float3 0.0 0.0 1.0)))
              (RGB 0.75 0.85 1.0) 0.1)
"== eyeball test for ray cast: flat material"
(define cam-400x200x1 (proj1-camera 400 200 1 1.0))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1 (cast-ray-in-world sphere-0))
                rgb->color))
"== eyeball test for ray cast: normal material"
(define sphere-1
  (make-sphere (Float3 0.0 0.0 -2.0) 0.5 normal-material))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1 (cast-ray-in-world sphere-1))
                rgb->color))
"== eyeball test for ray cast: normal material and two spheres"
(define sphere-2 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0 normal-material))
(define world-1 (list->object (list sphere-1 sphere-2)))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1 (cast-ray-in-world world-1))
                rgb->color))
"== eyeball test for ray cast (100 samples): normal material and two spheres"
(define cam-400x200x100 (proj1-camera 400 200 100 1.0))
(foreach-pixel cam-400x200x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-400x200x100 (cast-ray-in-world world-1))
                rgb->color))

(: trace-ray-in-world : Object Natural -> Ray -> RGB)
;; Given a world and a maximum tracing depth, this function returns
;; a function that will recursively trace a ray through the world
;; to compute a color
(define (trace-ray-in-world obj n)
  (lambda ([ray : Ray])
    (local
      {(: trace-ray-aux : Natural Ray -> RGB)
       ;; a helper function to generate a RGB value
       ;; based on the depth and a ray recursively
       (define (trace-ray-aux n ray)
         (cond
           [(= n 0) rgb-black]
           [else (local
                   {(define hit-or-miss : Maybe-Hit
                      (object-hit? obj ray (Pair 0.001 +inf.0)))}
                   (cond
                     [(Hit? hit-or-miss)
                      (match (get-hit-info hit-or-miss ray)
                        ['None (get-emission hit-or-miss)]
                        [(Some (Hit-Info rgb ray))
                         (rgb* rgb (trace-ray-aux (- n 1) ray))])]
                     [else (ray->rgb ray)]))]))}
      (trace-ray-aux n ray))))

;; run tests
(check-within
 ((trace-ray-in-world world-1 10) (make-ray fl3-zero (Float3 1.0 1.0 1.0)))
 (RGB 0.605 0.763 1.0) 0.1)
(check-within
 ((trace-ray-in-world world-1 10) (make-ray fl3-zero (Float3 0.5 0.5 1.0)))
 (RGB 0.648 0.788 1.0) 0.1)
(check-within
 ((trace-ray-in-world world-1 10) (make-ray fl3-zero (Float3 0.3 0.6 0.9)))
 (RGB 0.616 0.770 1.0) 0.1)
"== eyeball test for ray trace (100 samples): lambertian material and two spheres"
(define sphere-3 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                              (lambertian-material (rgb-gray 0.5))))
(define sphere-4 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                              (lambertian-material (rgb-gray 0.5))))
(define world-2 (list->object (list sphere-3 sphere-4)))
(define cam-200x100x100 (proj1-camera 200 100 100 1.0))
(foreach-pixel cam-200x100x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-200x100x100
                                      (trace-ray-in-world world-2 5))
                gamma-rgb->color))
"== eyeball test for ray trace (100 samples): lambertian and metal; four spheres"
(define sphere-5 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                              (lambertian-material (RGB 0.8 0.3 0.3))))
(define sphere-6 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                              (lambertian-material (RGB 0.8 0.8 0.0))))
(define sphere-7 (make-sphere (Float3 1.0 0.0 -2.0) 0.5
                              (metal-material (RGB 0.8 0.6 0.2) 1.0)))
(define sphere-8 (make-sphere (Float3 -1.0 0.0 -2.0) 0.5
                              (metal-material (RGB 0.8 0.8 0.8) 0.1)))
(define world-3 (list->object (list sphere-5 sphere-6 sphere-7 sphere-8)))
(foreach-pixel cam-200x100x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-200x100x100
                                      (trace-ray-in-world world-3 20))
                gamma-rgb->color))

(: ray-tracer : Camera Object -> Image)
;; Given a camera and world object, render a scene using a depth limit of 20.
(define (ray-tracer cam obj)
  (foreach-pixel cam
                 (make-pixel-renderer
                  (antialias-pixel->rgb cam
                                        (trace-ray-in-world obj 20))
                  gamma-rgb->color)))

"== eyeball test for ray-tracer (100 samples): lambertian and metal; four spheres"
(ray-tracer cam-200x100x100 world-3)

"== eyeball test for camera"
(local
  {(define cam (make-camera 200 100 100
                            (Float3 2.0 1.0 1.0)
                            (Float3 0.0 0.0 0.0)
                            (Float3 0.0 1.0 0.0)
                            120.0))
   (define sphere-1 (make-sphere (Float3 0.0 -100.5 -1.0) 100.0
                                 (lambertian-material (RGB 0.8 0.8 0.0))))
   (define sphere-2 (make-sphere (Float3 1.0 0.0 -1.0) 0.5
                                 (metal-material (RGB 0.8 0.6 0.2) 1.0)))
   (define sphere-3 (make-sphere (Float3 0.0 0.0 -1.0) 0.5
                                 (lambertian-material (RGB 0.8 0.3 0.3))))
   (define sphere-4 (make-sphere (Float3 -1.0 0.0 -1.0) 0.5
                                 (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (list->object (list sphere-1 sphere-2 sphere-3 sphere-4)))}
  (ray-tracer cam world))

"== eyeball test for emissive material"
(local
  {(define cam (proj1-camera 200 100 100 1.0))
   (define sphere-1 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                                 (lambertian-material (RGB 0.8 0.3 0.3))))
   (define sphere-2 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                                 (lambertian-material (RGB 0.8 0.8 0.0))))
   (define sphere-3 (make-sphere (Float3 1.0 0.0 -2.0) 0.5
                                 (diffuse-light-material (RGB 2.0 2.0 2.0))))
   (define sphere-4 (make-sphere (Float3 -1.0 0.0 -2.0) 0.5
                                 (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (list->object (list sphere-1 sphere-2 sphere-3 sphere-4)))}
  (ray-tracer cam world))

"== eyeball test for a box (ray-cast)"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box (make-box (Float3 0.0 0.0 -2.0)
                         (Float3 0.5 0.5 0.5)
                         (flat-material (RGB 1.0 0.0 0.0))))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world box))
                  rgb->color)))
"== eyeball test for a box (ray-cast; normal material)"
(local
  {(define cam (make-camera 400 200 1
                            (Float3 2.0 2.0 2.0)
                            fl3-zero
                            (Float3 0.0 1.0 0.0)
                            90.0))
   (define box (make-box fl3-zero
                         (Float3 0.5 0.5 0.5)
                         normal-material))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world box))
                  rgb->color)))
"== eyeball test for a box (ray-trace; mixed materials)"
(local
  {(define cam (proj1-camera 400 200 100 1.0))
   (define obj-1 (make-box (Float3 0.0 -100.5 -2.0)
                           (Float3 100.0 100.0 100.0)
                           (lambertian-material (RGB 0.8 0.8 0.0))))
   (define obj-2 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                              (lambertian-material (RGB 0.8 0.3 0.3))))
   (define obj-3 (make-sphere (Float3 1.0 0.0 -2.0) 0.5
                              (metal-material (RGB 0.8 0.6 0.2) 1.0)))
   (define obj-4 (make-box (Float3 -1.0 0.0 -2.0)
                           (Float3 0.5 0.5 0.5)
                           (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (list->object (list obj-1 obj-2 obj-3 obj-4)))}
  (time (ray-tracer cam world)))

"== eyeball test for translated box"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box
     (obj-translate
      (Float3 -0.75 -0.75 -2.0)
      (make-box fl3-zero (Float3 0.5 0.5 0.5) normal-material)))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world box))
                  rgb->color)))
"== eyeball test for rotated and translated box"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box
     (obj-translate
      (Float3 0.0 -0.75 -2.0)
      (obj-rotate-y 30.0
                    (make-box fl3-zero (Float3 0.5 0.5 0.5) normal-material))))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world box))
                  rgb->color)))

"== eyeball test for ray cast: flat material, using KD-trees"
(define sphere-0-new
  (Sphere (Float3 0.0 0.0 -2.0) 0.5 (flat-material (RGB 1.0 0.0 0.0))))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1
                            (cast-ray-in-world (scene->object (list sphere-0-new))))
                rgb->color))
"== eyeball test for ray cast: normal material, using KD-trees"
(define sphere-1-new
  (Sphere (Float3 0.0 0.0 -2.0) 0.5 normal-material))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1
                            (cast-ray-in-world (scene->object (list sphere-1-new))))
                rgb->color))
"== eyeball test for ray cast: normal material and two spheres, using KD-trees"
(define sphere-2-new
  (Sphere (Float3 0.0 -100.5 -2.0) 100.0 normal-material))
(define world-1-new (scene->object (list sphere-1-new sphere-2-new)))
(foreach-pixel cam-400x200x1
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1
                            (cast-ray-in-world world-1-new))
                rgb->color))
"== eyeball test for ray cast (100 samples): normal material and two spheres,
 using KD-trees"
(foreach-pixel cam-400x200x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-400x200x100
                                      (cast-ray-in-world world-1-new))
                rgb->color))
"== eyeball test for ray trace (100 samples): lambertian material and two
 spheres, using KD-trees"
(define sphere-3-new (Sphere (Float3 0.0 0.0 -2.0) 0.5
                             (lambertian-material (rgb-gray 0.5))))
(define sphere-4-new (Sphere (Float3 0.0 -100.5 -2.0) 100.0
                             (lambertian-material (rgb-gray 0.5))))
(define world-2-new (scene->object (list sphere-3-new sphere-4-new)))
(ray-tracer cam-200x100x100 world-2-new)
"== eyeball test for ray trace (100 samples): lambertian and metal;
 four spheres, using KD-trees"
(define sphere-5-new (Sphere (Float3 0.0 0.0 -2.0) 0.5
                             (lambertian-material (RGB 0.8 0.3 0.3))))
(define sphere-6-new (Sphere (Float3 0.0 -100.5 -2.0) 100.0
                             (lambertian-material (RGB 0.8 0.8 0.0))))
(define sphere-7-new (Sphere (Float3 1.0 0.0 -2.0) 0.5
                             (metal-material (RGB 0.8 0.6 0.2) 1.0)))
(define sphere-8-new (Sphere (Float3 -1.0 0.0 -2.0) 0.5
                             (metal-material (RGB 0.8 0.8 0.8) 0.1)))
(define world-3-new (scene->object
                     (list sphere-5-new sphere-6-new sphere-7-new sphere-8-new)))
(ray-tracer cam-200x100x100 world-3-new)
"== eyeball test for camera, using KD-trees"
(local
  {(define cam (make-camera 200 100 100
                            (Float3 2.0 1.0 1.0)
                            (Float3 0.0 0.0 0.0)
                            (Float3 0.0 1.0 0.0)
                            120.0))
   (define sphere-1 (Sphere (Float3 0.0 -100.5 -1.0) 100.0
                            (lambertian-material (RGB 0.8 0.8 0.0))))
   (define sphere-2 (Sphere (Float3 1.0 0.0 -1.0) 0.5
                            (metal-material (RGB 0.8 0.6 0.2) 1.0)))
   (define sphere-3 (Sphere (Float3 0.0 0.0 -1.0) 0.5
                            (lambertian-material (RGB 0.8 0.3 0.3))))
   (define sphere-4 (Sphere (Float3 -1.0 0.0 -1.0) 0.5
                            (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (scene->object (list sphere-1 sphere-2 sphere-3 sphere-4)))}
  (ray-tracer cam world))
"== eyeball test for emissive material, using KD-trees"
(local
  {(define cam (proj1-camera 200 100 100 1.0))
   (define sphere-1 (Sphere (Float3 0.0 0.0 -2.0) 0.5
                            (lambertian-material (RGB 0.8 0.3 0.3))))
   (define sphere-2 (Sphere (Float3 0.0 -100.5 -2.0) 100.0
                            (lambertian-material (RGB 0.8 0.8 0.0))))
   (define sphere-3 (Sphere (Float3 1.0 0.0 -2.0) 0.5
                            (diffuse-light-material (RGB 2.0 2.0 2.0))))
   (define sphere-4 (Sphere (Float3 -1.0 0.0 -2.0) 0.5
                            (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (scene->object (list sphere-1 sphere-2 sphere-3 sphere-4)))}
  (ray-tracer cam world))
"== eyeball test for a box (ray-cast), using KD-trees"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box (Box (Float3 0.0 0.0 -2.0)
                    (Float3 0.5 0.5 0.5)
                    (flat-material (RGB 1.0 0.0 0.0))))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world (scene->object (list box))))
                  rgb->color)))
"== eyeball test for a box (ray-cast; normal material), using KD-trees"
(local
  {(define cam (make-camera 400 200 1
                            (Float3 2.0 2.0 2.0)
                            fl3-zero
                            (Float3 0.0 1.0 0.0)
                            90.0))
   (define box (Box fl3-zero
                    (Float3 0.5 0.5 0.5)
                    normal-material))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world (scene->object (list box))))
                  rgb->color)))
"== eyeball test for a box (ray-trace; mixed materials), using KD-trees"
(local
  {(define cam (proj1-camera 400 200 100 1.0))
   (define obj-1 (Box (Float3 0.0 -100.5 -2.0)
                      (Float3 100.0 100.0 100.0)
                      (lambertian-material (RGB 0.8 0.8 0.0))))
   (define obj-2 (Sphere (Float3 0.0 0.0 -2.0) 0.5
                         (lambertian-material (RGB 0.8 0.3 0.3))))
   (define obj-3 (Sphere (Float3 1.0 0.0 -2.0) 0.5
                         (metal-material (RGB 0.8 0.6 0.2) 1.0)))
   (define obj-4 (Box (Float3 -1.0 0.0 -2.0)
                      (Float3 0.5 0.5 0.5)
                      (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (scene->object (list obj-1 obj-2 obj-3 obj-4)))}
  (time (ray-tracer cam world)))
"== eyeball test for translated box, using KD-trees"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box
     (Translate-Box
      (Float3 -0.75 -0.75 -2.0)
      (Box fl3-zero (Float3 0.5 0.5 0.5) normal-material)))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world (scene->object (list box))))
                  rgb->color)))
"== eyeball test for rotated and translated box, using KD-trees"
(local
  {(define cam (proj1-camera 400 200 1 1.0))
   (define box
     (Translate-Box
      (Float3 0.0 -0.75 -2.0)
      (Rotate-Box 'Y 30.0
                  (Box fl3-zero (Float3 0.5 0.5 0.5) normal-material))))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world (scene->object (list box))))
                  rgb->color)))

(test)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide cast-ray-in-world
         trace-ray-in-world
         ray-tracer)