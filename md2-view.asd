;;;;===========================================================================
;;;; @file   md2-view.asd
;;;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;; @date   Wed May  5 16:48:47 2010
;;;; 
;;;; @brief  asdf-system for md2-view
;;;;===========================================================================        

(defpackage #:md2-view-asd (:use #:asdf #:cl))
(in-package :md2-view-asd)

(defsystem md2-view
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description "This is the asd file for md2view"
  :depends-on ("lispbuilder-sdl-image" "lispbuilder-sdl" "cl-opengl")
  :components ((:file "package")
               (:file "gl-texture" :depends-on ("package"))
               (:file "anorms" :depends-on ("package"))
               (:file "bounding-box" :depends-on ("package"))
               (:file "3d-math" :depends-on ("package"))
               (:file "cad-view" :depends-on ("package"
                                             "3d-math"))
               (:file "id-md2" :depends-on ("package"
                                            "gl-texture"
                                            "anorms"
                                            "bounding-box"))
               (:file "md2-view" :depends-on ("package"
                                              "3d-math"
                                              "id-md2"
                                              "cad-view"
                                              "bounding-box"))))
