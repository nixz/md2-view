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
           start-zoom-box
           update-zoom-box
           finish-zoom-box
           zoom-box-p1
           zoom-box-p2
           box-zooming-p
           fit
           fit-zoom-box
           matrix
           set-center-of-rotation
           top-view
           bottom-view
           right-view
           left-view
           front-view
           back-view
           iso-view))

(in-package :cad-view)

(require :cl-opengl)
(load "3d-math.lisp")

(defparameter *win-w* nil)
(defparameter *win-h* nil)
(defparameter *win-aspect* nil
  "*win-w* / *win-h*")
(defparameter *world-w* nil)
(defparameter *world-h* nil)
(defparameter *world-center* nil
  "#(x y) don't need the Z")
(defparameter *modelview-matrix* nil)
(defparameter *rot-factor* nil)
(defparameter *zoom-factor* nil)
(defparameter *center-of-rotation* nil
  "#(x y z)")
(defparameter *box-zooming-p* nil)
(defparameter *zoom-box-p1* nil
  "For fit and zoom box operations")
(defparameter *zoom-box-p2* nil
  "For fit and zoom box operations")

;; ======================================================================
;; Utility

(defmacro v->l (v)
  `(coerce ,v 'list))

(defun get-pixel-size (proj-matrix)
  "Get the world size of a pixel.
Return the values: width height."
  (values (/ 2 (* (aref proj-matrix 0) *win-w*))
          (/ 2 (* (aref proj-matrix 5) *win-h*))))

(defun screen->world (x y)
  "Convert the screen coordinates X and Y to world coordinates.
Return #(x y)."
  (multiple-value-bind
        (pw ph)
      (get-pixel-size (gl:get-float :projection-matrix))
    (vector (+ (- (aref *world-center* 0)
                  (* *world-w* .5))
               (* pw x))
            (+ (- (aref *world-center* 1)
                  (* *world-h* .5))
               (* ph (- *win-h* y))))))

;; ======================================================================

(defun init (&key (world-h 20) (center-of-rotation #(0 0 0)) (rot-factor 1)
             (zoom-factor .01))
  "Initialize the CAD view.
Args are self-explanatory, yes?"
  (setf *world-h* world-h
        *center-of-rotation* center-of-rotation
        *rot-factor* rot-factor
        *zoom-factor* zoom-factor
        *box-zooming-p* nil
        *world-center* #(.0 .0))
  (iso-view))

(defun ortho ()
  "Update the projection matrix, for resize, pan, and zoom ops"
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (setf *world-w* (* *world-h* *win-aspect*))
  (let ((cx (aref *world-center* 0))
        (cy (aref *world-center* 1)))
    (force-output)
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
        *win-aspect* (/ (coerce win-w 'float) win-h))
  (gl:viewport 0 0 win-w win-h)
  (ortho))

(defun set-center-of-rotation (p)
  "P should be a vector of the form #(x y z)"
  (setf *center-of-rotation* p))

(defun matrix ()
  "Return the current modelview matrix"
  *modelview-matrix*)

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

(defun fit (p1 p2)
  "Zoom to the 2d box defined by P1 and P2.
P1 and P2 should have to form #(x y) in world coordinates"
  (let* ((x1 (aref p1 0))
         (y1 (aref p1 1))
         (x2 (aref p2 0))
         (y2 (aref p2 1))
         (w (abs (- x1 x2)))
         (h (abs (- y1 y2))))
    (unless (some #'zerop `(,h ,w))     ; dont zoom a zero-length box
      (setf *world-center* (vector (* (+ x1 x2) .5)
                                   (* (+ y1 y2) .5)))
      (if (>= (/ w h) *win-aspect*)
          (setf *world-h* (/ w *win-aspect*))
          (setf *world-h* h))
      (ortho))))

(defun fit-zoom-box ()
  (fit *zoom-box-p1* *zoom-box-p2*))

;; ======================================================================
;; Zoom box

(defun box-zooming-p ()
  *box-zooming-p*)

(defun zoom-box-p1 ()
  *zoom-box-p1*)

(defun zoom-box-p2 ()
  *zoom-box-p2*)

(defun start-zoom-box (x y)
  "Initialize a draggable zoom box.
X and Y are the current mouse screen coordinates."
  (let ((wc (screen->world x y)))
    (setf *box-zooming-p* t
          *zoom-box-p1* wc
          *zoom-box-p2* wc)))

(defun update-zoom-box (x y)
  "Update *zoom-box-p2*"
  (setf *zoom-box-p2* (screen->world x y)))

(defun finish-zoom-box (x y)
  "Terminate a draggable zoom box.
X and Y are the current mouse screen coordinates."
  (setf *box-zooming-p* nil
        *zoom-box-p2* (screen->world x y)))

;; ======================================================================
;; Quick views

(defun top-view ()
  "Rotate to view the +X +Y Plane"
  (gl:load-identity)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun bottom-view ()
  "Rotate to view the +X -Y Plane"
  (gl:load-identity)
  (gl:rotate 180 1 0 0)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun right-view ()
  "Rotate to view the +Y +Z Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (gl:rotate -90 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun left-view ()
  "Rotate to view the -Y +Z Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (gl:rotate 90 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun front-view ()
  "Rotate to view the +X +Z Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun back-view ()
  "Rotate to view the -X +Z Plane"
  (gl:load-identity)
  (gl:rotate -90 1 0 0)
  (gl:rotate 180 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))

(defun iso-view ()
  "Rotate to an isometric view."
  (gl:load-identity)
  (gl:rotate -60 1 0 0)
  (gl:rotate -45 0 0 1)
  (apply #'gl:translate (v->l (3d:v*s *center-of-rotation* -1)))
  (setf *modelview-matrix* (gl:get-float :modelview-matrix)))
