;;; 3d-math.lisp
;;;
;;; Matrix and vector operations for a right-handed coordinate system.
;;; 
;;; Vectors are length 3.
;;; 
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Thursday, April  8 2010

(defpackage 3d-math
  (:nicknames :3d)
  (:use :cl)
  (:export +i-axis+ +j-axis+ +k-axis+
           vx vy vz
           mi mj mk mt
           cross-product
           make-4x4-matrix
           normalize
           normalize!
           magnitude
           make-vector
           dot-product
           v+v v-v v*s m*v m*m
           vector&rotation-angle->matrix
           trans->matrix
           scale->matrix
           x-rot->matrix
           y-rot->matrix
           z-rot->matrix
           euler-angles->matrix
           matrix->euler-angles
           clamp
           d->r))

(in-package :3d-math)

(defconstant +i-axis+ #(1.0d0 0.0d0 0.0d0))
(defconstant +j-axis+ #(0.0d0 1.0d0 0.0d0))
(defconstant +k-axis+ #(0.0d0 0.0d0 1.0d0))

(defmacro d->r (d)
  `(* 0.017453292519943295d0 ,d))

(defun clamp (x low high)
  "Ensure x is in the interval [low, high]"
  (max low (min x high)))

(defmacro vx (v)
  "setf-able access to the X (index 0) component of vector v"
  `(aref ,v 0))

(defmacro vy (v)
  "setf-able access to the Y (index 1) component of vector v"
  `(aref ,v 1))

(defmacro vz (v)
  "setf-able access to the Z (index 2) component of vector v"
  `(aref ,v 2))

(defun make-vector (&optional (x 0.0d0) (y 0.0d0) (z 0.0d0))
  "Return a new vector of length 3 with each component initialized to 0.0 if
not supplied."
  (make-array 3 :initial-contents `(,(coerce x 'double-float)
                                     ,(coerce y 'double-float)
                                     ,(coerce z 'double-float))))


(defun make-4x4-matrix (&key (initial-contents
                              '((1.0d0 0.0d0 0.0d0 0.0d0)    ; initialize
                                (0.0d0 1.0d0 0.0d0 0.0d0)    ; to the
                                (0.0d0 0.0d0 1.0d0 0.0d0)    ; identity
                                (0.0d0 0.0d0 0.0d0 1.0d0)))) ; matrix
  "Return a new row-major 4x4 matrix.
If initial-contents is not supplied, the identity matrix is returned."
  (make-array '(4 4) :initial-contents initial-contents))

(defun mi (m)
  "Return a copy of the I (X) vector of m."
  (vector (aref m 0 0) (aref m 1 0) (aref m 2 0)))

(defun mj (m)
  "Return a copy of the J (Y) vector of m."
  (vector (aref m 0 1) (aref m 1 1) (aref m 2 1)))

(defun mk (m)
  "Return a copy of the K (Z) vector of m."
  (vector (aref m 0 2) (aref m 1 2) (aref m 2 2)))

(defun mt (m)
  "Return a copy of the T (translation) vector of m."
  (vector (aref m 0 3) (aref m 1 3) (aref m 2 3)))

(defun magnitude (v)
  "Return the magnitude of the vector v."
  (let ((x (vx v))
        (y (vy v))
        (z (vz v)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun normalize! (v)
  "Normalize and return v."
  (let ((mag (magnitude v)))
    (when (plusp mag)
      (setf (vx v) (/ (vx v) mag))
      (setf (vy v) (/ (vy v) mag))
      (setf (vz v) (/ (vz v) mag))))
  v)

(defun normalize (v)
  "Return a copy of the vector v normalized. If v has magnitude 0.0, the
return value is a copy of the vector v unaltered."
  (let ((mag (magnitude v))
        (out v))
    (when (plusp mag)
      (setf out (make-vector (/ (vx v) mag)
                             (/ (vy v) mag)
                             (/ (vz v) mag))))
    out))

(defun cross-product (v1 v2)
  "Return the cross product of the vectors v1 and v2.
Does not normalize the vectors."
  (make-vector (- (* (vy v1) (vz v2))
                  (* (vz v1) (vy v2)))
               (- (* (vz v1) (vx v2))
                  (* (vx v1) (vz v2)))
               (- (* (vx v1) (vy v2))
                  (* (vy v1) (vx v2)))))

(defun dot-product (v1 v2)
  "Return the dot product (cosine) of the vectors v1 and v2"
  (+ (* (vx v1) (vx v2))
     (* (vy v1) (vy v2))
     (* (vz v1) (vz v2))))

(defun v-v (v1 v2)
  "Difference of the vectors v1 and v2. Return a new vector."
  (make-vector (- (vx v1) (vx v2))
               (- (vy v1) (vy v2))
               (- (vz v1) (vz v2))))

(defun v+v (v1 v2)
  "Sum of the vectors v1 and v2. Return a new vector."
  (make-vector (+ (vx v1) (vx v2))
               (+ (vy v1) (vy v2))
               (+ (vz v1) (vz v2))))

(defun v*s (v s)
  "Product of the vector v and the scalar s. Return a new vector."
  (let ((s (coerce s 'double-float)))
    (make-vector (* (vx v) s)
                 (* (vy v) s)
                 (* (vz v) s))))

(defun m*v (m v)
  "Product of the matrix m and the vector v. Return a new vector."
  (let ((vv `#(,(vx v) ,(vy v) ,(vz v) 1.0))
        (out (vector 0 0 0 0))
        result)
    (dotimes (r 4)
      (setf result 0.0d0)
      (dotimes (c 4)
        (incf result (* (aref vv c) (aref m c r))))
      (setf (aref out r) result))
    (subseq out 0 3)))

(defun vector&rotation-angle->matrix (v radian-angle)
  "Return the matrix describing the rotation radian-angle about the
normalized vector v."
  (let* ((c (cos radian-angle))
         (s (sin radian-angle))
         (q (- 1.0d0 c))
         (i (vx v))
         (j (vy v))
         (k (vz v))
         (out (make-4x4-matrix)))
    ;; I
    (setf (aref out 0 0) (+ c (* q i i)))
    (setf (aref out 1 0) (- (* q i j) (* s k)))
    (setf (aref out 2 0) (+ (* q i k) (* s j)))
    ;; J
    (setf (aref out 0 1) (+ (* q i j) (* s k)))
    (setf (aref out 1 1) (+ c (* q j j)))
    (setf (aref out 2 1) (- (* q j k) (* s i)))
    ;; K
    (setf (aref out 0 2) (- (* q i k) (* s j)))
    (setf (aref out 1 2) (+ (* q j k) (* s i)))
    (setf (aref out 2 2) (+ c (* q k k)))
    out))

(defun trans->matrix (x y z)
  "Return a new 4x4 translation matrix."
  (let ((m (make-4x4-matrix)))
    (setf (aref m 3 0) (coerce x 'double-float))
    (setf (aref m 3 1) (coerce y 'double-float))
    (setf (aref m 3 2) (coerce z 'double-float))
    m))

(defun scale->matrix (sx sy sz)
  "Return a new 4x4 scale matrix."
  (let ((m (make-4x4-matrix)))
    (setf (aref m 0 0) (coerce sx 'double-float))
    (setf (aref m 1 1) (coerce sy 'double-float))
    (setf (aref m 2 2) (coerce sz 'double-float))
    m))

(defun x-rot->matrix (radian-angle)
  "Return a new 4x4 matrix defining a rotation about the X axis."
  (let ((m (make-4x4-matrix))
        (c (cos radian-angle))
        (s (sin radian-angle)))
    (setf (aref m 1 1) c)
    (setf (aref m 2 1) (- s))
    (setf (aref m 1 2) s)
    (setf (aref m 2 2) c)
    m))

(defun y-rot->matrix (radian-angle)
  "Return a new 4x4 matrix defining a rotation (in radians) about the Y axis."
  (let ((m (make-4x4-matrix))
        (c (cos radian-angle))
        (s (sin radian-angle)))
    (setf (aref m 0 0) c)
    (setf (aref m 2 0) s)
    (setf (aref m 0 2) (- s))
    (setf (aref m 2 2) c)
    m))

(defun z-rot->matrix (radian-angle)
  "Return a new 4x4 matrix defining a rotation about the Z axis."
  (let ((m (make-4x4-matrix))
        (c (cos radian-angle))
        (s (sin radian-angle)))
    (setf (aref m 0 0) c)
    (setf (aref m 1 0) (- s))
    (setf (aref m 0 1) s)
    (setf (aref m 1 1) c)
    m))

(defun euler-angles->matrix (x-radian-angle y-radian-angle z-radian-angle)
  "Return a new 4x4 matrix created from the given xyz Euler angles."
  (let* ((a (cos x-radian-angle))
         (b (sin x-radian-angle))
         (c (cos y-radian-angle))
         (d (sin y-radian-angle))
         (e (cos z-radian-angle))
         (f (sin z-radian-angle))
         (ad (* a d))
         (bd (* b d))
         (m (make-4x4-matrix)))
    ;; I
    (setf (aref m 0 0) (* c e))
    (setf (aref m 1 0) (* (- c) f))
    (setf (aref m 2 0) d)
    ;; J
    (setf (aref m 0 1) (+ (* bd e) (* a f)))
    (setf (aref m 1 1) (+ (* (- bd) f) (* a e)))
    (setf (aref m 2 1) (* (- b) c))
    ;; K
    (setf (aref m 0 2) (+ (* (- ad) e) (* b f)))
    (setf (aref m 1 2) (+ (* ad f) (* b e)))
    (setf (aref m 2 2) (* a c))
    m))

(defun matrix->euler-angles (m)
  "Return the x, y, and z Euler angles computed from the given 4x4 matrix." 
  (let* ((yang (asin (aref m 2 0)))
         (c (cos yang))
         xang zang
         (pi/2 (/ pi 2.0d0)))
    (cond ((plusp (abs c))
           (setf xang (atan (/ (- (aref m 2 1)) c)
                            (/ (aref m 2 2) c)))
           (setf zang (atan (/ (- (aref m 1 0)) c)
                            (/ (aref m 0 0) c))))
          (t
           (setf xang 0.0d0)
           (setf zang (atan (aref m 0 1) (aref m 1 1)))))
    (values (clamp xang (- pi/2) pi/2)
            (clamp yang (- pi/2) pi/2)
            (clamp zang (- pi/2) pi/2))))

(defun m*m (m1 m2)
  "Product of two 4x4 matrices. Return a new 4x4 matrix."
  (let ((m (make-4x4-matrix))
        result)
    (dotimes (r 4)
      (dotimes (c 4)
        (setf result 0.0)
        (dotimes (k 4)
          (incf result (* (aref m1 k r)
                          (aref m2 c k))))
        (setf (aref m c r) result)))
    m))
