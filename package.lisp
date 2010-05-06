;;;; --------------------------------------------------------------------------
;;;; @file   package.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Wed May  5 18:49:47 2010
;;;;
;;;; @brief  Contains all the packages in the system
;;;; --------------------------------------------------------------------------


(in-package #:cl-user)

(defpackage #:3d-math
  (:nicknames #:3d)
  (:use #:cl)
  (:export :+i-axis+
           :+j-axis+
           :+k-axis+
           :vx
           :vy
           :vz
           :mi
           :mj
           :mk
           :mt
           :cross-product
           :make-4x4-matrix
           :make-4x4-matrix-from-gl-matrix
           :normalize
           :normalize!
           :magnitude
           :make-vector
           :dot-product
           :v+v
           :v-v
           :v*s
           :m*v
           :m*m
           :vector&rotation-angle->matrix
           :trans->matrix
           :scale->matrix
           :x-rot->matrix
           :y-rot->matrix
           :z-rot->matrix
           :euler-angles->matrix
           :matrix->euler-angles
           :clamp
           :d->r))

(defpackage #:gl-texture
  (:nicknames #:gltex)
  (:use #:cl)
  (:export :load-texture))

(defpackage #:bounding-box
  (:nicknames #:bb)
  (:use #:cl)
  (:export :find-bbox
           :find-union
           :find-union-2
           :expand-bbox
           :bbox-coords
           :bbox-center
           :bbox-left
           :bbox-right
           :bbox-bottom
           :bbox-top
           :bbox-front
           :bbox-back))

(defpackage #:id-md2
  (:nicknames :md2)
  (:use #:cl
        #:gl-texture
        #:bounding-box)
  (:export :load-model
           :bounding-box
           :model-file-name
           :model-skin-file-names
           :model-skin-width
           :model-skin-height
           :model-bbox
           :model-tex-coords
           :model-triangles
           :model-gl-commands
           :model-frames
           :model-gl-tex-ids
           :model-evaluated-p
           :print-model-info
           :triangle-vertex-indices
           :triangle-st-indices
           :vertex-v
           :vertex-n
           :frame-scale
           :frame-translate
           :frame-name
           :frame-vertices
           :*anorms*))

(defpackage #:cad-view
  (:nicknames :cv)
  (:use #:cl
        #:3d-math)
  (:export :init
           :resize
           :rotate
           :zoom
           :pan
           :start-zoom-box
           :update-zoom-box
           :finish-zoom-box
           :zoom-box-p1
           :zoom-box-p2
           :box-zooming-p
           :fit
           :fit-zoom-box
           :matrix
           :set-center-of-rotation
           :top-view
           :bottom-view
           :right-view
           :left-view
           :front-view
           :back-view
           :iso-view))


(defpackage #:md2-view
  (:use #:cl
        #:3d-math
        #:id-md2
        #:cad-view
        #:bounding-box)
  (:export :run))


