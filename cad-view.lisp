;;; cad-view.lisp
;;;
;;; Computer Aided Drafting type viewing.
;;;    
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Friday, April 23 2010

(defpackage :cad-view
  (:nicknames :cv)
  (:use :cl)
  (:export init
           resize
           rotate
           zoom
           pan
           matrix
           set-center-of-rotation
           top-view
           right-view
           front-view
           iso-view))

(in-package :cad-view)

(require :cl-opengl)
(load "3d-math.lisp")

(defparameter *win-w* nil)
(defparameter *win-h* nil)
(defparameter *aspect* nil)
(defparameter *world-w* nil)
(defparameter *world-h* nil)
(defparameter *world-center* nil
  "#(x y) don't need the Z")
(defparameter *modelview-matrix* nil)
(defparameter *rot-factor* nil)
(defparameter *zoom-factor* nil)
(defparameter *center-of-rotation* nil
  "#(x y z)")

(defmacro v->l (v)
  `(map 'list #'identity ,v))

(defun init (&key (world-h 20) (center-of-rotation #(0 0 0)) (rot-factor 1)
             (zoom-factor .01))
  "Initialize the CAD view.
Args are self-explanatory, yes?"
  (setf *world-h* world-h
        *center-of-rotation* center-of-rotation
        *rot-factor* rot-factor
        *zoom-factor* zoom-factor)
  (iso-view))

(defun ortho ()
  "Update the projection matrix, for resize, pan, and zoom ops"
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (setf *world-w* (* *world-h* *aspect*))
  (let ((cx (aref *world-center* 0))
        (cy (aref *world-center* 1)))
    (gl:ortho (- cx (* *world-w* .5))
              (+ cx (* *world-w* .5))
              (- cy (* *world-h* .5))
              (+ cy (* *world-h* .5))
              -1000 1000))              ; TODO: model dependent
  (gl:matrix-mode :modelview))

(defun resize (win-w win-h)
  "Should be called when the window is resized."
  (when (or (zerop win-w)
            (zerop win-h))
    (return-from resize))
  (setf *win-w* win-w
        *win-h* win-h
        *aspect* (/ (coerce win-w 'float) win-h))
  (gl:viewport 0 0 win-w win-h)
  (unless *world-center*         ; initialize the world center on first resize
    (setf *world-center* (vector (/ (* *world-h* *aspect*) 2)
                                 (/ *world-h* 2))))
  (ortho))

(defun set-center-of-rotation (p)
  "P should be a vector of the form #(x y z)"
  (setf *center-of-rotation* p))

(defun matrix ()
  "Return the current modelview matrix"
  *modelview-matrix*)

(defun get-pixel-size (proj-matrix)
  "Get the world size of a pixel.
Return the values: width height."
  (values (/ (* 2 (/ 1 (aref proj-matrix 0))) *win-w*)
          (/ (* 2 (/ 1 (aref proj-matrix 5))) *win-h*)))

(defun rotate (dx dy)
  "Rotate about *CENTER-OF-ROTATION*.
DX and DY are the last relative mouse deltas."
  (let* ((rot-axis (vector dy dx 0))
         (nra (3d:normalize rot-axis)))
    (gl:with-pushed-matrix
      (gl:load-identity)
      (gl:rotate (* *rot-factor* (3d:magnitude rot-axis))
                 (aref nra 0)
                 (aref nra 1)
                 (aref nra 2))
      (gl:mult-matrix *modelview-matrix*)
      (setf *modelview-matrix* (gl:get-float :modelview-matrix)))
    (apply #'gl:translate (v->l *center-of-rotation*))))

(defun zoom (dy)
  "Zoom about the center of the screen.
DY is the last relative mouse delta."
  (decf *world-w* (* *world-w* *zoom-factor* dy))
  (decf *world-h* (* *world-h* *zoom-factor* dy))
  (ortho))

(defun pan (dx dy)
  "Shift the viewport left/right up/down.
DX and DY are last relative mouse deltas."
  (multiple-value-bind
        (pw ph)
      (get-pixel-size (gl:get-float :projection-matrix))
    (decf (aref *world-center* 0) (* pw dx))
    (incf (aref *world-center* 1) (* ph dy))
    (ortho)))

(defun top-view ()
  "Rotate to view the XY Plane"
  (gl:load-identity)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun right-view ()
  "Rotate to view the YZ Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (gl:rotate -90 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun front-view ()
  "XZ Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun iso-view ()
  "Rotate to an isometric view."
  (gl:load-identity)
  (gl:rotate -60 1 0 0)
  (gl:rotate -45 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))
