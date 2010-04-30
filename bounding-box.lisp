;;; bounding-box.lisp
;;; 
;;; A few bounding box functions
;;;            
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Wednesday, April 28 2010

(defpackage :bounding-box
  (:nicknames :bb)
  (:use :cl)
  (:export :find-bbox
           :find-union
           :find-union-2
           :expand-bbox
           :bbox-coords
           :bbox-center))

(in-package :bounding-box)

(defmacro v->l (v)
  `(map 'list #'identity ,v))

(defstruct bbox
  "A coordinate-aligned bounding box.
                     +--------2 right top back
                    /|       /|                                            
                   / |      / |               Z+  
                  +--------+  |               |   
                  |  +-----|--+               |  Y+ (into the screen)
                  | /      | /                | /
                  |/       |/                 |/
left bottom front 1--------+                  *--------> X+"
  set-p                                 ; -1e10 and 1e10 not in coords
  coords                                ; #(x1 y1 z1 x2 y2 z2)
  x-size                                ; x axis length
  y-size                                ; y axis length
  z-size                                ; z axis length
  center                                ; center of the box
  left                                  ; minimim X coord
  right                                 ; maximum X coord
  bottom                                ; minimum Y coord
  top                                   ; maximim Y coord
  front                                 ; maximim Z coord
  back)                                 ; minimum Z coord
  
(defun create-bbox (coords)
  "Return a new bbox instance from COORDS.
COORDS should be a vector of the form #(x1 y1 z1 x2 y2 z2)"
  (make-bbox :set-p (and (not (find 1e10 coords))   ; 640k ought to be
                         (not (find -1e10 coords))) ; enough for anybody
             :coords coords
             :x-size (- (aref coords 3) (aref coords 0))
             :y-size (- (aref coords 4) (aref coords 1))
             :z-size (- (aref coords 5) (aref coords 2))
             :center (vector (* (+ (aref coords 0) (aref coords 3))
                                .5)
                             (* (+ (aref coords 1) (aref coords 4))
                                .5)
                             (* (+ (aref coords 2) (aref coords 5))
                                .5))
             :left (aref coords 0)
             :right (aref coords 3)
             :bottom (aref coords 2)
             :top (aref coords 5)
             :front (aref coords 1)
             :back (aref coords 4)))

(defun find-bbox (vertices)
  "Find the coordinate-aligned box that bounds VERTICES.
VERTICES should have the form (#(x y z) #(x y z) ...)
Return a new bbox instance."  
  (let ((coords (make-array 6 :initial-contents
                            '(1e10 1e10 1e10 -1e10 -1e10 -1e10))))
    (loop for v in vertices do
         (destructuring-bind (x y z) (v->l v)
           (setf (aref coords 0) (min x (aref coords 0))
                 (aref coords 1) (min y (aref coords 1))
                 (aref coords 2) (min z (aref coords 2))
                 (aref coords 3) (max x (aref coords 3))
                 (aref coords 4) (max y (aref coords 4))
                 (aref coords 5) (max z (aref coords 5))))
         finally
         (return (create-bbox coords)))))

(defun find-union-2 (bbox1 bbox2)
  "Find the box that bounds BBOX1 and BBOX2.
Return a new bbox instance."
  (destructuring-bind (b1x1 b1y1 b1z1 b1x2 b1y2 b1z2)
      (v->l (bbox-coords bbox1))
    (destructuring-bind (b2x1 b2y1 b2z1 b2x2 b2y2 b2z2)
        (v->l (bbox-coords bbox2))
      (create-bbox (vector (min b1x1 b2x1)
                           (min b1y1 b2y1)
                           (min b1z1 b2z1)
                           (max b1x2 b2x2)
                           (max b1y2 b2y2)
                           (max b1z2 b2z2))))))

(defun find-union (bboxes)
  "Find the union of all bbox instances in the list BBOXES.
Return a new bbox instance."
  (reduce (lambda (b1 b2)
            (if b2
                (find-union-2 b1 b2)
                b1))
          bboxes))

(defun expand-bbox (bbox1 &rest bboxes)
  "Add the coords of BBOXES to BBOX1.
Return BBOX1."
  (setf (bbox-coords bbox1)
        (bbox-coords (find-union (cons bbox1 bboxes))))
  bbox1)
