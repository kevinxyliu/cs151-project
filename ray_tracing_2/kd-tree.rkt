#lang typed/racket

;; CMSC 15100 Winter 2019
;; Project Code
;; kd-tree.rkt module
;; <YOUR NAME>
;;
;; This module contains the implementation of the KD-Tree
;; spatial partitioning and its application to implement
;; an Object for a Scene.
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
(require "object.rkt")
(require "scene.rkt")
(require "material.rkt")

;; A KD-Tree is either a KD-Node representing a splitting plane
;; or a KD-Leaf containing a, possibly empty, object
;;
(define-type KD-Tree (U KD-Node KD-Leaf))

(define-struct KD-Node
  ([axis : Axis]                ;; the axis that defines the splitting plane (it is
   ;; perpendicular to the axis)
   [split-at : Float]           ;; the point on the axis where the splitting plane
   ;; is located
   [left : KD-Tree]             ;; the left partition (values <= the splitting plane)
   [right : KD-Tree]))          ;; the right partition (values > the splitting plane)

(define-struct KD-Leaf
  ([object : Object]))          ;; the object(s) in a sub volume

(: ray-split-node : Ray Interval KD-Node -> (Listof (Pair Interval KD-Tree)))
;; given a ray, interval, and k-d-tree node, determine where the line
;; segment defined by the ray and interval lie with respect to the plane
;; defined by the split point.  The result is a list of one or two pairs,
;; where each pair consists of an interval and a subtree.  The interval
;; defines the portion of the line segment that lies in the region covered
;; by the subtree.  The list is ordered by t value so that if there is a
;; hit in the first subregion, then the second does not have to be checked.
;;
(define (ray-split-node ray min-max-t node)
  (local
    {(define split-at : Float (KD-Node-split-at node))
     (define axis : Axis (KD-Node-axis node))
     (define Dw : Float (Ray1-delta (ray-project ray axis)))
     (define Pw : Float (Ray1-origin (ray-project ray axis)))
     (define left-tree : KD-Tree (KD-Node-left node))
     (define right-tree : KD-Tree (KD-Node-right node))}
    (if (< (abs Dw) 0.001) (list (Pair min-max-t (if (< Pw split-at) left-tree right-tree)))
        (local
          {;; the definition of these variables are given in the project write-up
           (define t : Float (/ (- split-at Pw) Dw))
           (define min-t : Float (Pair-fst min-max-t))
           (define max-t : Float (Pair-snd min-max-t))}
          (cond
            [(or (> t max-t) (< t 0.0))
             (list (Pair min-max-t (if (< Pw split-at) left-tree right-tree)))]
            [(= t 0.0) (list (Pair min-max-t (if (< Dw 0) left-tree right-tree)))]
            ;; the case when t=0.0 happens when the origin of the ray is on the
            ;; split plane, and we select the half-plane where Dw points towards.
            [(within? t min-max-t)
             (if (< Pw split-at)
                 (list (Pair (Pair min-t t) left-tree)
                       (Pair (Pair t max-t) right-tree))
                 (list (Pair (Pair min-t t) right-tree)
                       (Pair (Pair t max-t) left-tree)))]
            [else (list (Pair min-max-t (if (< Pw split-at) right-tree left-tree)))])))))

;; run tests
;; define some test leaves/test nodes for testing purposes.
(define test-leaf-1 : KD-Leaf
  (KD-Leaf
   (Object (lambda ([ray : Ray] [min-max-t : Interval])
             (Hit 0.5 (Float3 1.0 1.0 1.0) (Float3 0.5 0.5 0.7)
                  normal-material)))))
(define test-leaf-2 : KD-Leaf
  (KD-Leaf
   (Object (lambda ([ray : Ray] [min-max-t : Interval])
             (Hit 0.6 (Float3 2.0 2.0 2.0) (Float3 0.6 0.6 0.7)
                  normal-material)))))
(define test-node-1 : KD-Node
  (KD-Node 'X 0.0 test-leaf-1 test-leaf-2))
(define test-node-2 : KD-Node
  (KD-Node 'Y 1.0 test-leaf-2 test-leaf-1))
(check-within (Pair-fst (Pair-fst
                         (first (ray-split-node (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                                (Pair 0.0 +inf.0) test-node-1))))
              0.0 0.1)
(check-within (Pair-fst
               (first (ray-split-node (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                      (Pair 0.0 +inf.0) test-node-2)))
              (Pair 0.0 1.0) 0.1)
(check-within (Pair-fst (Pair-fst (match (ray-split-node
                                          (Ray fl3-zero (Float3 1.0 1.0 1.0))
                                          (Pair 0.0 +inf.0) test-node-2)
                                    [(list pair1 pair2) pair2])))
              1.0 0.1)

(: kd-tree->object : KD-Tree -> Object)
;; Given a KD-Tree that organizes a collection of objects, package it
;; up as an Object
(define (kd-tree->object tree)
  (Object
   (lambda ([ray : Ray] [min-max-t : Interval])
     (match tree
       [(KD-Leaf obj) (object-hit? obj ray min-max-t)]
       [(KD-Node _ _ _ _)
        (match (ray-split-node ray min-max-t tree)
          [(list (Pair interval subtree))
           (object-hit? (kd-tree->object subtree) ray interval)]
          [(list (Pair interval1 subtree1) (Pair interval2 subtree2))
           (local
             {(define hit-or-miss : Maybe-Hit
                (object-hit? (kd-tree->object subtree1) ray interval1))}
             (if (Hit? hit-or-miss) hit-or-miss
                 (object-hit? (kd-tree->object subtree2) ray interval2)))])]))))

;; run tests
(: convert : Maybe-Hit -> Hit)
;; a funtion that converts a Maybe-Hit value into a
;; Hit value for testing purposes.
(define (convert maybe-hit)
  (if (Hit? maybe-hit) maybe-hit (error "Not a Hit type")))
;; define several hit information for testing purposes.
(define maybe-hit-1 : Hit
  (convert (object-hit? (kd-tree->object test-leaf-1)
                        (Ray fl3-zero (Float3 1.0 1.0 1.0))
                        (Pair 0.0 +inf.0))))
(define maybe-hit-2 : Hit
  (convert (object-hit? (kd-tree->object test-leaf-2)
                        (Ray fl3-zero (Float3 1.0 1.0 1.0))
                        (Pair 0.0 +inf.0))))
(define maybe-hit-3 : Hit
  (convert (object-hit? (kd-tree->object test-node-1)
                        (Ray fl3-zero (Float3 3.0 1.0 1.0))
                        (Pair 0.0 +inf.0))))
(check-within (Hit-t maybe-hit-1) 0.5 0.1)
(check-within (Hit-pt maybe-hit-1) (Float3 1.0 1.0 1.0) 0.1)
(check-within (Hit-norm maybe-hit-1) (Float3 0.5 0.5 0.7) 0.1)
(check-within (Hit-t maybe-hit-2) 0.6 0.1)
(check-within (Hit-pt maybe-hit-2) (Float3 2.0 2.0 2.0) 0.1)
(check-within (Hit-norm maybe-hit-2) (Float3 0.6 0.6 0.7) 0.1)
(check-within (Hit-t maybe-hit-3) 0.6 0.1)
(check-within (Hit-pt maybe-hit-3) (Float3 2.0 2.0 2.0) 0.1)
(check-within (Hit-norm maybe-hit-3) (Float3 0.6 0.6 0.7) 0.1)

(: scene->object : Scene -> Object)
;; construct an object for a Scene
(define (scene->object scene)
  (match (scene->kd-tree scene)
    [(KD-Leaf obj) obj]
    [tree (kd-tree->object tree)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Instances

;; a 3D Axis-aligned Bounding Box specified by the minimum and maximum coordinates.
;; This is used to represent the extent of an object when projected onto the
;; X, Y, and Z axes
;;
(define-type AABB (Pair Float3 Float3))

;; an instance is a scene object paired with its axis-aligned bounding box
(define-struct Instance
  ([obj : Scene-Object]
   [bbox : AABB]))

(: rotate-aabb : AABB (Float3 -> Float3) -> AABB)
;; compute a new AABB that contains the rotation of the given box.
;; The second argument is the rotation function
(define (rotate-aabb aabb rotate)
  (match aabb
    [(Pair (Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (local
       {(: grow : Float3 AABB -> AABB)
        ;; grow the AABB to include the rotated point
        (define (grow pt aabb)
          (match* ((rotate pt) aabb)
            [((Float3 x0 y0 z0) (Pair (Float3 x1 y1 z1) (Float3 x2 y2 z2)))
             (Pair
              (Float3 (min x0 x1) (min y0 y1) (min z0 z1))
              (Float3 (max x0 x2) (max y0 y2) (max z0 z2)))]))}
       ;; build a bounding box that contains all 8 corners after rotation
       (foldl grow
              (Pair (Float3 +inf.0 +inf.0 +inf.0) (Float3 -inf.0 -inf.0 -inf.0))
              (list
               (Float3 x1 y1 z1)
               (Float3 x1 y1 z2)
               (Float3 x1 y2 z1)
               (Float3 x1 y2 z2)
               (Float3 x2 y1 z1)
               (Float3 x2 y1 z2)
               (Float3 x2 y2 z1)
               (Float3 x2 y2 z2))))]))

(: make-instance : Scene-Object -> Instance)
;; given a Scene-Object, pair it with its bounding box
(define (make-instance obj)
  (local
    {(: compute-aabb : Scene-Object -> AABB)
     ;; compute the bounding box for an object
     (define (compute-aabb obj)
       (match obj
         [(Sphere (Float3 x y z) r _)
          (Pair (Float3 (- x r) (- y r) (- z r))
                (Float3 (+ x r) (+ y r) (+ z r)))]
         [(Box (Float3 x y z) (Float3 dx dy dz) _)
          (Pair (Float3 (- x dx) (- y dy) (- z dz))
                (Float3 (+ x dx) (+ y dy) (+ z dz)))]
         [(Translate-Box offset box)
          (match (compute-aabb box)
            [(Pair min max) (Pair (fl3+ min offset) (fl3+ max offset))])]
         [(Rotate-Box 'X angle box)
          (rotate-aabb (compute-aabb box) (fl3-rotate-x angle))]
         [(Rotate-Box 'Y angle box)
          (rotate-aabb (compute-aabb box) (fl3-rotate-y angle))]
         [(Rotate-Box 'Z angle box)
          (rotate-aabb (compute-aabb box) (fl3-rotate-z angle))]))}
    (Instance obj (compute-aabb obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Construction of KD trees

(: aabb-split : AABB Axis Float -> (Pair AABB AABB))
;; split an AABB along the specified axis at the specified point
(define (aabb-split aabb axis split-at)
  (match aabb
    [(Pair min max)
     (match* (min max axis)
       [((Float3 x1 y1 z1) (Float3 x2 y2 z2) 'X)
        (Pair (Pair min (Float3 split-at y2 z2))
              (Pair (Float3 split-at y1 z1) max))]
       [((Float3 x1 y1 z1) (Float3 x2 y2 z2) 'Y)
        (Pair (Pair min (Float3 x2 split-at z2))
              (Pair (Float3 x1 split-at z1) max))]
       [((Float3 x1 y1 z1) (Float3 x2 y2 z2) 'Z)
        (Pair (Pair min (Float3 x2 y2 split-at))
              (Pair (Float3 x1 y1 split-at) max))])]))

(: extent : Instance -> AABB)
;; compute the extent of an instance when projected onto the X, Y, and Z axes
(define (extent obj) (Instance-bbox obj))

(: axis-extent : Instance Axis -> Interval)
;; return the span of the object along the given axis
(define (axis-extent obj axis)
  (match obj
    [(Instance _ (Pair min max)) (Pair (fl3-project min axis) (fl3-project max axis))]))

;; We need to be able to identify objects, so we put them into a vector
;; and use their index in the vector as a unique ID.
;;
(define-type Obj-Vec (Vectorof Instance))
(define-type Obj-Id Natural)
(define-type Obj-Ids (Listof Obj-Id))

(: compute-bounds : (Listof Instance) -> AABB)
;; compute the bounds of a list of instances
(define (compute-bounds scene)
  (match scene
    ['() (error "compute-bounds: empty scene")]
    [(cons obj1 objr)
     (local
       {(: grow : Instance AABB -> AABB)
        ;; grow the AABB to include the AABB of the instance
        (define (grow obj aabb)
          (match* (aabb (extent obj))
            [((Pair (Float3 x11 y11 z11) (Float3 x12 y12 z12))
              (Pair (Float3 x21 y21 z21) (Float3 x22 y22 z22)))
             (Pair (Float3 (min x11 x21) (min y11 y21) (min z11 z21))
                   (Float3 (max x12 x22) (max y12 y22) (max z12 z22)))]))}
       (foldl grow (extent obj1) objr))]))

(: partition-at : Obj-Vec Obj-Ids Axis Float Interval
   -> (Triple Obj-Ids Float Obj-Ids))
;; partition the given objects along the given axis using
;; the specified split-at point.
(define (partition-at ovec ids axis split-at span)
  (local
    {(: part : Obj-Ids Natural Natural Float Float Obj-Ids Obj-Ids
        -> (Triple Obj-Ids Float Obj-Ids))
     ;; helper function that does the actual partitioning.  If an object is
     ;; wholly below split-at then it goes in the left list and if it is
     ;; wholly above split-at then it goes in the right list.  Otherwise,
     ;; it is added to both lists.
     (define (part ids nleft nright lo hi left right)
       (match ids
         ['()
          (if (< lo hi)
              ;; adjust split to edge of larger group
              (if (< nleft nright)
                  (Triple left hi right)
                  (Triple left lo right))
              (Triple left split-at right))]
         [(cons id idr)
          (match (axis-extent (vector-ref ovec id) axis)
            [(Pair min-ext max-ext)
             (cond
               [(<= max-ext split-at)
                (part idr (+ nleft 1) nright (max lo max-ext) hi (cons id left) right)]
               [(<= split-at min-ext)
                (part idr nleft (+ nright 1) lo (min hi min-ext) left (cons id right))]
               [else
                (part idr
                      (+ nleft 1) (+ nright 1)
                      (max lo max-ext) (min hi min-ext)
                      (cons id left) (cons id right))])])]))}
    (part ids 0 0 (Pair-fst span) (Pair-snd span) '() '())))

;; A (Split axis split-at cost left right) represents a partitioning
;; of a set of objects along the specified axis.
(define-struct Split
  ([axis : Axis]         ;; axis perpendicular to splitting plane
   [split-at : Float]    ;; position of splitting plane on axis
   [cost : Float]        ;; cost of split
   [left : Obj-Ids]      ;; objects on left side of split (i.e., <= split-at)
   [right : Obj-Ids]))   ;; objects on left side of split (i.e., > split-at)

(: make-split : (Triple Obj-Ids Float Obj-Ids) Axis Interval Float Float -> Split)
;; given a partition of objects on the axis, make a Split struct.
;; This primarily means computing a cost estimate for the split, which
;; is done using the approach described by MacDonald and Booth in
;; "Heuristics for Ray Tracing Using Space Subdivision" (Graphics Interface '89)
;;
(define (make-split partition axis span wid ht)
  (local
    {;; the area of the base of the volume being split
     (define base-area (* wid ht))
     ;; the area of the sides of the volume being split
     (define side-area (* 2.0 (+ wid ht) (interval-width span)))}
    ;; partition the objects by the spatial median
    (match* (partition span)
      [((Triple left split-at right) (Pair span-min span-max))
       (local
         {(define lsa (+ base-area (* side-area (- split-at span-min))))
          (define rsa (+ base-area (* side-area (- span-max split-at))))
          (define cost (+ (* (->fl (length left)) lsa)
                          (* (->fl (length right)) rsa)))}
         (Split axis split-at cost left right))])))

(: adjust-split : (Listof Interval) Float Interval -> Float)
;; given a list of object spans and the average center of the spans
;; adjust the split point to align with an object edge
(define (adjust-split spans center span)
  (match spans
    ['() (error "adjust-split: empty list")]
    [(list _) (error "adjust-split: singleton list")]
    [_
     (local
       {(: lp : (Listof Interval) Natural Float (Listof Interval) Natural Float
           -> Float)
        ;; loop over the intervals collecting the following information:
        ;;   - the number of intervals below the center and their upper bound
        ;;   - the number of intervals above the center and their lower bound
        ;;   - list of intervals that cover the center
        (define (lp spans n-left lo c-spans n-right hi)
          (match spans
            ['()
             (match c-spans
               ['() (if (< n-left n-right) hi lo)]
               [(cons span spanr)
                (cond
                  [(= n-left n-right 0)
                   (local  ;; all spans are in the c-spans list
                     {(: dist : Float -> Float)
                      ;; distance from w to center
                      (define (dist w) (abs (- center w)))
                      (: pick-best : Interval Float -> Float)
                      ;; compare span's edges against best so far
                      (define (pick-best span best)
                        (cond
                          [(< (dist (Pair-fst span)) best) (dist (Pair-fst span))]
                          [(< (dist center) best) (dist center)]
                          [else best]))}
                     (foldl pick-best
                            (if (<= (dist (Pair-fst span)) (dist (Pair-snd span)))
                                (Pair-fst span)
                                (Pair-snd span))
                            c-spans))]
                  [(< n-left n-right) hi]
                  [else lo])])]
            [(cons span spanr)
             (cond
               [(<= (Pair-snd span) center)
                (lp spanr (+ n-left 1) (max lo (Pair-snd span)) c-spans n-right hi)]
               [(<= center (Pair-fst span))
                (lp spanr n-left lo c-spans (+ n-right 1) (min hi (Pair-fst span)))]
               [else
                (lp spanr n-left lo (cons span c-spans) n-right hi)])]))}
       (lp spans 0 (Pair-fst span) '() 0 (Pair-snd span)))]))

(: compute-median : Obj-Vec Obj-Ids Axis Interval -> Float)
;; compute a split point for the axis that is close to the average of the
;; object centers.  The actual split point will be chosen to align with
;; the edge of some object that overlaps the average center.
(define (compute-median ovec ids axis span)
  (local
    {;; generate a list of intervals for the object
     (define spans
       (map (lambda ([id : Obj-Id])
              (interval-intersect span (axis-extent (vector-ref ovec id) axis)))
            ids))
     (: lp : (Listof Interval) Natural Float -> Float)
     ;; loop over objects computing the number of objects and sum of centers;
     ;; return the average center
     (define (lp spans n sum)
       (match spans
         ['() (* (/ 1.0 (->fl n)) sum)]
         [(cons span spanr) (lp
                             spanr
                             (+ n 1)
                             (+ sum (interval-center span)))]))}
    ;; compute the average center and adjust it
    (adjust-split spans (lp spans 0 0.0) span)))

(: mean-split : Obj-Vec Obj-Ids Axis Interval Float Float -> Split)
;; compute a split of the objects in a volume with respect to the given axis.
;; The interval is the span of the volume along the axis and the last two
;; parameters are the other dimensions of the volume.  The evaluation of
;; the split plane uses the technique described by MacDonald and Booth in
;; "Heuristics for Ray Tracing Using Space Subdivision" (Graphics Interface '89)
;;
(define (mean-split ovec ids axis span wid ht)
  (make-split
   (partition-at ovec ids axis (compute-median ovec ids axis span) span)
   axis span wid ht))

(: compute-split : Obj-Vec Obj-Ids AABB -> Split)
;; determine the best split for the given bounding box containing the
;; list of objects
;;
(define (compute-split ovec ids aabb)
  (match aabb
    [(Pair (Float3 x1 y1 z1) (Float3 x2 y2 z2))
     (local
       {;; get dimensions of the bounding box
        (define wid-x (- x2 x1))
        (define wid-y (- y2 y1))
        (define wid-z (- z2 z1))
        ;; compute splits by each axis
        (define split-x (mean-split ovec ids 'X (Pair x1 x2) wid-y wid-z))
        (define split-y (mean-split ovec ids 'Y (Pair y1 y2) wid-x wid-z))
        (define split-z (mean-split ovec ids 'Z (Pair z1 z2) wid-x wid-y))
        (: split<= : Split Split -> Boolean)
        ;; compare two splits and return true if the first is lower cost
        ;; than the second
        (define (split<= split1 split2)
          (match* (split1 split2)
            [((Split _ _ c1 (cons _ _) (cons _ _)) (Split _ _ c2 (cons _ _) (cons _ _)))
             (<= c1 c2)]
            [((Split _ _ c1 (cons _ _) (cons _ _)) (Split _ _ c2 l2 r2))
             (or (empty? l2) (empty? r2) (<= c1 c2))]
            [((Split _ _ c1 _ _) (Split _ _ c2 (cons _ _) (cons _ _))) #f]
            [((Split _ _ c1 _ _) (Split _ _ c2 _ _)) (<= c1 c2)]))}
       ;; pick split with lowest cost score
       (cond
         [(and (split<= split-x split-y) (split<= split-x split-z)) split-x]
         [(split<= split-y split-z) split-y]
         [else split-z]))]))

(: scene->kd-tree : Scene -> KD-Tree)
;; build a KD tree for the scene.
(define (scene->kd-tree scene)
  (local
    {;; compute object instances
     (define instances (map make-instance scene))
     ;; build the vector of objects and initial object-id list
     (define ovec : Obj-Vec (list->vector instances))
     (define ids : Obj-Ids
       (build-list (vector-length ovec) (lambda ([id : Natural]) id)))
     ;; convert the Scene-Objects into Objects
     (define objects : (Vectorof Object)
       (list->vector (map scene-object->object scene)))
     (: obj-ref : Obj-Id -> Object)
     ;; return the Object associated with the given ID
     (define (obj-ref id) (vector-ref objects id))
     (: make-tree : Obj-Ids AABB Natural -> KD-Tree)
     ;; recursively build a KD-Tree for the given list of object IDs.
     ;; The triple of intervals describes the bounds of the scene.
     ;;
     (define (make-tree ids aabb depth-limit)
       (match ids
         ['() (KD-Leaf empty-object)]
         [(list id) (KD-Leaf (obj-ref id))]
         [(list id1 id2)
          ;; only two objects, so it is not worth splitting them up
          (KD-Leaf (list->object (list (obj-ref id1) (obj-ref id2))))]
         [_ (if (= depth-limit 0)
                ;; hit tree depth limit, so we just group all objects
                (KD-Leaf (list->object (map obj-ref ids)))
                ;; split the objects into subtrees
                (match* ((length ids) (compute-split ovec ids aabb))
                  [(n-objs (Split axis split-at _ left right))
                   (if (= n-objs (length left) (length right))
                       ;; split has not improved the situation, so we just
                       ;; build a big leaf
                       (KD-Leaf (list->object (map obj-ref ids)))
                       ;; otherwise build a node
                       (match (aabb-split aabb axis split-at)
                         [(Pair left-aabb right-aabb)
                          (KD-Node
                           axis split-at
                           (make-tree left left-aabb (- depth-limit 1))
                           (make-tree right right-aabb (- depth-limit 1)))]))]))]))}
    (make-tree ids (compute-bounds instances) 16)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide scene->object)
