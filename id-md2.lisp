;;; id-md2.lisp
;;;
;;; Id MD2 file reader.
;;;
;;; model
;;; |-- evaluated-p
;;; |-- "model-file-name"
;;; |-- skin-width
;;; |-- skin-height
;;; |-- bbox
;;; |-- skin-file-names
;;; |   |-- "skin-file-name-1"
;;; |   |-- "skin-file-name-2"
;;; |   `-- ...
;;; |-- tex-coords
;;; |   |-- (s1 . t1)
;;; |   |-- (s2 . t2)         
;;; |   `-- ...
;;; |-- triangles
;;; |   |-- triangle
;;; |   |   |-- #(x-idx y-idx z-idx)
;;; |   |   `-- #(st-x-idx st-y-idx st-z-idx)
;;; |   |-- triangle
;;; |   |   |-- #(x-idx y-idx z-idx)
;;; |   |   `-- #(st-x-idx st-y-idx st-z-idx)
;;; |   `-- ...
;;; |-- gl-commands
;;; |   |-- -42
;;; |   |-- 99
;;; |   `-- ...
;;; |-- frames
;;; |   |-- frame
;;; |   |   |-- scale
;;; |   |   |-- translate
;;; |   |   |-- "frame-name-1"
;;; |   |   `-- vertices
;;; |   |       |-- vertex
;;; |   |       |   |-- #(x y z)
;;; |   |       |   `-- #(i j k)
;;; |   |       |-- vertex
;;; |   |       |   |-- #(x y z)
;;; |   |       |   `-- #(i j k)
;;; |   |       `-- ...
;;; |   `-- frame
;;; |       |-- scale
;;; |       |-- translate
;;; |       |-- "frame-name-2"
;;; |       `-- vertices
;;; |           |-- vertex
;;; |           |   |-- #(x y z)
;;; |           |   `-- #(i j k)
;;; |           |-- vertex
;;; |           |   |-- #(x y z)
;;; |           |   `-- #(i j k)
;;; |           `-- ...
;;; `-- gl-text-ids
;;;     |-- 0
;;;     |-- 9
;;;     `-- ...
;;;
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Saturday, April 24 2010

(defpackage :id-md2
  (:nicknames :md2)
  (:use :cl)
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

(in-package :id-md2)

(load "gl-texture.lisp")
(load "anorms.lisp")
(load "bounding-box.lisp")

(defconstant +magic-number+ 844121161)

(defstruct model
  file-name
  skin-width
  skin-height
  bbox                                ; load-time model bounding box @ frame 0
  skin-file-names
  tex-coords
  triangles
  gl-commands
  frames
  gl-tex-ids                          ; associated glGenTextures
  evaluated-p)                        ; t if tex-coords, normals and vertex
                                       ; coords have been evaluated during load

(defstruct header
  ident                                ; magic number "IDP2" or 844121161
  version                              ; must be 8
  skin-width                           ; texture width
  skin-height                          ; texture height
  frame-size                           ; size in bytes of a frame and its data
  num-skins                            ; number of associated textures
  num-vertices                         ; vertice count for one frame
  num-tex-coords                       ; number of texture coordinates
  num-triangles                        ; number of triangles
  num-gl-commands                      ; number of OpenGL commands
  num-frames                           ; number of frames
  ;; byte offset from 0 to...
  offset-skins                          ; texture data
  offset-tex-coords                     ; texture coordinates data
  offset-triangles                      ; triangle data
  offset-frames                         ; frame data
  offset-gl-commands                    ; OpenGL commands
  offset-end)                           ; end of file

;; ======================================================================
;; Data types

;; Vector
;; simple vector of 3 floats

;; Texture info
(defstruct skin
  (file-name ""))                       ; 64 characters

;; Triangles
(defstruct triangle
  (vertex-indices #(0 0 0))
  (st-indices #(0 0 0)))

;; Vertices
(defstruct vertex
  (v #(0 0 0))                          ; vertex
  (n #(0 0 0)))                         ; normal

;; Frames
(defstruct frame
  (scale #(0.0 0.0 0.0))                ; scale factor
  (translate #(0.0 0.0 0.0))            ; translation vector
  (name "")                             ; 16 characters
  (vertices nil))                       ; vector of vetex

;; ======================================================================
;; Read an MD2 file

(defun read-uint2 (s)
  "Read a 16 bit unsigned integer"
  (let ((out 0))
    (setf (ldb (byte 8 0) out) (read-byte s))
    (setf (ldb (byte 8 8) out) (read-byte s))
    out))

(defun read-sint2 (s)
  "Read a 16 bit signed integer"
  (let ((out (read-uint2 s)))
    (if (logbitp 15 out)
        (- (+ 1 (logxor out #xffff)))
        out)))

(defun read-uint4 (s)
  "Read a 32 bit unsigned integer"
  (let ((out 0))
    (setf (ldb (byte 8 0) out) (read-byte s))
    (setf (ldb (byte 8 8) out) (read-byte s))
    (setf (ldb (byte 8 16) out) (read-byte s))
    (setf (ldb (byte 8 24) out) (read-byte s))
    out))

(defun read-sint4 (s)
  "Read a 32 bit signed integer"
  (let ((out (read-uint4 s)))
    (if (logbitp 31 out)
        (- (+ 1 (logxor out #xffffffff)))
        out)))

(defun read-float4 (s)
  "Read a 32 bit single precision float."
  (let* ((b32 (read-uint4 s))
         (exponent (ash (logand b32 #x7f800000) -23))
         (significand (logand b32 #x007FFFFF))
         (decoded-exponent (- exponent #x7f))
         (decoded-significand 0)
         (mask #x800000))
    (unless (zerop exponent)
      (setf significand (logior significand mask))) ; implicit leading 1
    (setf decoded-significand
          (loop
             with term = 1.0 and
             result = 0
             for b from 0 upto 22 do
             (setf term (/ 1.0 (expt 2 b)))
             (when (plusp (logand mask significand))
               (incf result term))
             (setf mask (ash mask -1))
             finally (return result)))
    (if (logbitp 31 b32)
        (- (* decoded-significand (expt 2 decoded-exponent)))
        (* decoded-significand (expt 2 decoded-exponent)))))

(defun read-string (s size)
  (map 'string #'code-char
       (progn
         (let ((chars (make-array size :element-type '(unsigned-byte 8))))
           (read-sequence chars s)
           (remove-if #'zerop chars))))) ; padded 0's

(defun read-triangle (s)
  (make-triangle :vertex-indices
                 `#(,(read-uint2 s)
                    ,(read-uint2 s)
                    ,(read-uint2 s))
                 :st-indices
                 `#(,(read-uint2 s)
                    ,(read-uint2 s)
                    ,(read-uint2 s))))

(defun read-tex-coord (s evaluate-p skin-width skin-height)
  (if evaluate-p
      (cons (/ (coerce (read-sint2 s)
                       'single-float)
               skin-width)
            (/ (coerce (read-sint2 s)
                       'single-float) skin-height))
      (cons (read-sint2 s)
            (read-sint2 s))))

(defun read-vertex (s evaluate-p scale translate)
  (if evaluate-p
      (let ((coords (vector (read-byte s) (read-byte s) (read-byte s))))
        (make-vertex :v (map 'vector #'(lambda (s v tx)
                                         (+ (* s v) tx))
                             scale
                             coords
                             translate)
                     :n (map 'vector #'identity
                             (aref *anorms* (read-byte s)))))
      (make-vertex :v (vector (read-byte s)
                              (read-byte s)
                              (read-byte s))
                   :n (read-byte s))))

(defun read-frame-vertices (s n-vertices evaluate-p scale translate)
  (let ((vertices (make-array n-vertices)))
    (dotimes (i n-vertices)
      (setf (aref vertices i)
            (read-vertex s evaluate-p scale translate)))
    vertices))

(defun read-frame (s n-vertices evaluate-p)
  (let ((scale (vector (read-float4 s)
                       (read-float4 s)
                       (read-float4 s)))
        (translate (vector (read-float4 s)
                           (read-float4 s)
                           (read-float4 s))))
    (make-frame :scale scale
                :translate translate
                :name (read-string s 16)
                :vertices (read-frame-vertices s n-vertices evaluate-p
                                               scale translate))))

(defun read-gl-commands (s n-commands)
  "Read N-COlMMANDS OpenGL commands from the stream S into an arry.
The texture coordinates are read and stored as single floats. Return the
array."
  (loop
     with gl-commands = (make-array n-commands) and
     data = 0 and
     i = -1
     do
     (setf data (read-sint4 s)          ; n vertices, strip or fan
           (aref gl-commands (incf i)) data
           data (abs data))             ; subsequent vertice count
     (when (zerop data)
       (return gl-commands))
     (dotimes (j data)
       (setf (aref gl-commands (incf i)) (read-float4 s)    ; s
             (aref gl-commands (incf i)) (read-float4 s)    ; t
             (aref gl-commands (incf i)) (read-uint4 s))))) ; vertex index

(defun bounding-box (model frame-n)
  "Find the coordiante-aligned bounding box for MODEL at FRAME-N.
Return a new bbox instance."
  (let* ((frame (aref (model-frames model) frame-n))
         (vertices (frame-vertices frame)))
    (bb:find-bbox
     (cond ((model-evaluated-p model)
            (loop
               for v being each element in vertices
               collect (vertex-v v)))
           (t
            (let ((scale (frame-scale frame))
                  (translate (frame-translate frame)))
              (loop
                 for v being each element in vertices
                 collect (vector
                          (+ (* (aref (vertex-v v) 0)
                                (aref scale 0))
                             (aref translate 0))
                          (+ (* (aref (vertex-v v) 1)
                                (aref scale 1))
                             (aref translate 1))
                          (+ (* (aref (vertex-v v) 2)
                                (aref scale 2))
                             (aref translate 2))))))))))

;; XXX: wont handle .tga
(defun load-textures (file-name texture-file-path texture-file-names)
  "Load the current model's textures from the list TEXTURE-FILE-NAMES.
Return a list of the texture object ids"
  (let ((path (or texture-file-path
                   (directory-namestring (pathname file-name))))
        (ids '()))
    ;; ensure trailing / in path 
    (when (and path
               (char/= (char path (- (length path) 1)) #\/)) 
      (setf path (concatenate 'string path "/")))
    (dolist (name texture-file-names)
      (setf name (concatenate 'string path name))
      ;; see if the file exists, if not try other formats
      (let ((tmp-name (copy-seq name)))
        (unless (probe-file tmp-name)
          (loop
             for ext in '(".png" ".bmp" ".jpg" ".jpeg" ".gif" ".pcx") do
             (replace tmp-name ext :start1 (position #\. name))
             (when (probe-file tmp-name)
               (return))
             finally
             (error "id-md2:load-texture: could not load texture: ~A" name)))
        ;; found texture file, load that bitch
        (push (gltex:load-texture tmp-name) ids)))
    ids))

(defun load-model (file-name &optional (evaluate-p t) (load-texture-p t)
                   texture-file-path)
  "Load the md2 FILE-NAME into a model struct. Return that model.
If EVALUATE-P is t, uncompress texture coordinates. If the string
TEXTURE-FILE-PATH is not supplied, use file-name's path"
  (let ((model (make-model :file-name file-name))
        ident)
    (with-open-file (s file-name
                       :direction :input
                       :element-type '(unsigned-byte 8))
      ;; Read the header
      (setf ident (read-uint4 s))
      (when (/= ident +magic-number+)
        (error "id-md2:load-model: ~A does not appear to be a Id MD2 file."
               file-name))
      (let ((version (read-uint4 s))
            (skin-width (read-uint4 s))
            (skin-height (read-uint4 s))
            (frame-size (read-uint4 s))
            (num-skins (read-uint4 s))
            (num-vertices (read-uint4 s))
            (num-tex-coords (read-uint4 s))
            (num-triangles (read-uint4 s))
            (num-gl-commands (read-uint4 s))
            (num-frames (read-uint4 s))
            (offset-skins (read-uint4 s))
            (offset-tex-coords (read-uint4 s))
            (offset-triangles (read-uint4 s))
            (offset-frames (read-uint4 s))
            (offset-gl-commands (read-uint4 s))
            (offset-end (read-uint4 s)))
        (declare (ignore version frame-size offset-end))
        (setf (model-skin-width model) skin-width)
        (setf (model-skin-height model) skin-height)
        (setf (model-evaluated-p model) evaluate-p)
        ;; Read the model data
        ;; skin-file-names
        (let ((skin-file-names '()))
          (file-position s offset-skins)
          (dotimes (i num-skins)
            (push (read-string s 64) skin-file-names))
          (setf (model-skin-file-names model) skin-file-names)
          (when load-texture-p
            (setf (model-gl-tex-ids model)
                  (load-textures file-name texture-file-path
                                 skin-file-names))))
        ;; tex-coords
        (let ((tex-coords (make-array num-tex-coords)))
          (file-position s offset-tex-coords)
          (dotimes (i num-tex-coords)
            (setf (aref tex-coords i)
                  (read-tex-coord s evaluate-p
                                  skin-width 
                                  skin-height)))
          (setf (model-tex-coords model) tex-coords))
        ;; triangles
        (let ((triangles (make-array num-triangles
                                     :element-type 'triangle)))
          (file-position s offset-triangles)
          (dotimes (i num-triangles)
            (setf (aref triangles i) (read-triangle s)))
          (setf (model-triangles model) triangles))
        ;; gl-commands
        (file-position s offset-gl-commands)
        (setf (model-gl-commands model)
              (read-gl-commands s num-gl-commands))
        ;; frames
        (let ((frames (make-array num-frames
                                  :element-type 'frame)))
          (file-position s offset-frames)
          (dotimes (i num-frames)
            (setf (aref frames i) (read-frame s num-vertices evaluate-p)))
          (setf (model-frames model) frames)))
      ;; bounding box
      (setf (model-bbox model) (bounding-box model 0))
      model)))

(defun print-model-info (model)
  (format t "
 Model info for: ~A
     skin-width: ~D
    skin-height: ~D
           bbox: ~S
skin-file-names: ~S
      triangles: ~D
         frames: ~D
    frame-names: ~S~%"
          (model-file-name model)
          (model-skin-width model)
          (model-skin-height model)
          (model-bbox model)
          (model-skin-file-names model)
          (length (model-triangles model))
          (length (model-frames model))
          (loop
               for f being each element in (model-frames model)
               collecting (frame-name f)))
  (force-output))
