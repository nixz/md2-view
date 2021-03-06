;;; md2-view.lisp
;;;
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Sunday, April 25 2010
;;; Added Packaging system (May 05 2010) Nikhil J. Shetty <nikhil.j.shetty@gmail.com>

(in-package :md2-view)


;; some of these state vars I don't really need, could use gl:get-foo
(defparameter *models* nil)            ; list of loaded model instances
(defparameter *triangle-count* nil)    ; number of triangles loaded 
(defparameter *frame-count* nil)       ; number of frames in the loaded model
(defparameter *lighting-p* t)          ; t enable, nil disable
(defparameter *polygon-mode* :fill)    ; line, point, fill
(defparameter *texture-p* t)           ; t enable, nil disable
(defparameter *smooth-p* t)            ; t smooth, nil flat
(defparameter *bboxes-p* nil)          ; t show bboxes, nil hide them
(defparameter *gnome-p* t)             ; t show gnome, nil hide it
(defparameter *animate-p* nil)         ; t to interpolate frames
(defparameter *t-divisor* nil)         ; num of animation frames/sec (sort of)
(defparameter *frame-n* nil)           ; animation start frame
(defparameter *t-val* nil)             ; animation interpolation value

(defmacro v->l (v)
  `(coerce ,v 'list))

;; lotta shit just for an axis indicator!
;; I was going to disable :cull-face, but it looks a little
;; better if the arrow tip that's facing away gets culled.
;; TODO: display list
(defun render-gnome ()
  "Render XYZ axis @ the origin with red green and blue colors, respectively.
Each axis will be approximately 1/8 the screen width."
  (gl:push-attrib :lighting)
  (gl:disable :lighting)
  (gl:disable :depth-test)              ; render on top of everything else
  ;(gl:enable :line-smooth)              ; fat smooth axis
  (gl:polygon-mode :front-and-back :fill)
  (let ((s (* .25 (/ 1 (aref (gl:get-float :projection-matrix) 0)))))
    (gl:with-pushed-matrix
      (gl:scale s s s)
      (gl:with-primitives :lines
        (gl:color 1 0 0)
        (gl:vertex 0 0 0)
        (gl:vertex 1 0 0)
        (gl:color 0 1 0)
        (gl:vertex 0 0 0)
        (gl:vertex 0 1 0)
        (gl:color 0 0 1)
        (gl:vertex 0 0 0)
        (gl:vertex 0 0 1))
      (gl:with-primitives :triangle-fan
        (gl:color 1 0 0)
        (gl:vertex 1.1 0 0)
        (loop
           for a from 0 upto (* pi 2) by (/ pi 2) do
             (gl:vertex 1 (* .05 (sin a)) (* .05 (cos a)))))
      (gl:with-primitives :triangle-fan
        (gl:color 0 1 0)
        (gl:vertex 0 1.1 0)
        (loop
           for a from 0 upto (* pi 2) by (/ pi 2) do
             (gl:vertex (* .05 (cos a)) 1 (* .05 (sin a)))))
      (gl:with-primitives :triangle-fan
        (gl:color 0 0 1)
        (gl:vertex 0 0 1.1)
        (loop
           for a from 0 upto (* pi 2) by (/ pi 2) do
             (gl:vertex (* .05 (sin a)) (* .05 (cos a)) 1)))))
  (gl:polygon-mode :front-and-back *polygon-mode*)
  ;(gl:disable :line-smooth)
  (gl:enable :depth-test)
  (gl:pop-attrib))

(defun render-bounding-boxes ()
  "Render coord-aligned bounding boxes for all loaded models at *FRAME-N*."
  (gl:push-attrib :lighting)
  (gl:disable :lighting :cull-face)
;;   (gl:enable :blend)
;;   (gl:blend-func :src-alpha :one-minus-src-alpha)
;;   (gl:shade-model :flat)
  (gl:polygon-mode :front-and-back :line)
  (gl:color .2 .3 .2)
  (dolist (b (mapcar (lambda (m)
                       (md2:bounding-box m *frame-n*))
                     ;(md2:model-bbox m))
                     *models*))
    (destructuring-bind (x1 y1 z1 x2 y2 z2) (v->l (bb:bbox-coords b))
      (gl:with-primitives :quad-strip
        (gl:vertex x1 y1 z1)
        (gl:vertex x1 y1 z2)
        (gl:vertex x2 y1 z1)
        (gl:vertex x2 y1 z2)
        (gl:vertex x2 y2 z1)
        (gl:vertex x2 y2 z2)
        (gl:vertex x1 y2 z1)
        (gl:vertex x1 y2 z2)
        (gl:vertex x1 y1 z1)
        (gl:vertex x1 y1 z2))))
  (gl:pop-attrib)
  (gl:enable :cull-face)
;;   (gl:disable :blend)
;;   (gl:shade-model (if *smooth-p* :smooth :flat))
  (gl:polygon-mode :front-and-back *polygon-mode*))

(defun render-animation (model)
  "Render MODEL @ *T-VAL* between *FRAME-N* and *FRAME-N* + 1."
  (let* ((from-frame (aref (md2:model-frames model) *frame-n*))
         (from-frame-vertices (md2:frame-vertices from-frame))
         (to-frame (aref (md2:model-frames model) (+ *frame-n* 1)))
         (to-frame-vertices (md2:frame-vertices to-frame))
         (gl-commands (md2:model-gl-commands model)))
    (loop
       with i = -1 and
       cmd = 0
       do
       (setf cmd (aref gl-commands (incf i)))
       (when (zerop cmd)
         (return))
       (if (< cmd 0)
           (gl:begin :triangle-fan)
           (gl:begin :triangle-strip))
       (dotimes (j (abs cmd))           ; each vertex
         (gl:tex-coord (aref gl-commands (incf i))
                       (aref gl-commands (incf i)))
         (let* ((from-vertice (aref from-frame-vertices
                                    (aref gl-commands (incf i))))
                (to-vertice (aref to-frame-vertices
                                  (aref gl-commands i)))
                (from-v (md2:vertex-v from-vertice))
                (to-v (md2:vertex-v to-vertice))
                (from-n (md2:vertex-n from-vertice))
                (to-n (md2:vertex-n to-vertice)))
           (apply #'gl:normal (v->l
                               (3d:v+v from-n
                                       (3d:v*s (3d:v-v to-n
                                                       from-n)
                                               *t-val*))))
           (apply #'gl:vertex (v->l
                               (3d:v+v from-v
                                       (3d:v*s (3d:v-v to-v
                                                       from-v)
                                               *t-val*))))))
       (gl:end))))

(defun render-frame (model)
  "Render *FRAME-N* of MODEL"
  (let* ((frame (aref (md2:model-frames model) *frame-n*))
         (frame-vertices (md2:frame-vertices frame))
         (gl-commands (md2:model-gl-commands model)))
    (loop
       with i = -1 and
       cmd = 0
       do
       (setf cmd (aref gl-commands (incf i)))
       (when (zerop cmd)
         (return))
       (if (< cmd 0)
           (gl:begin :triangle-fan)
           (gl:begin :triangle-strip))
       (dotimes (j (abs cmd))           ; each vertex
         (gl:tex-coord (aref gl-commands (incf i))
                       (aref gl-commands (incf i)))
         (let ((v (aref frame-vertices (aref gl-commands (incf i)))))
           (apply #'gl:normal (v->l (md2:vertex-n v)))
           (apply #'gl:vertex (v->l (md2:vertex-v v)))))
       (gl:end))))

(defun render-zoom-box ()
  (gl:push-attrib :lighting)
  (gl:disable :lighting :depth-test :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:color 1 1 0)
  (gl:with-pushed-matrix
    (gl:load-identity)
    (let ((p1 (cad-view:zoom-box-p1))
          (p2 (cad-view:zoom-box-p2)))
      (gl:rect (aref p1 0)
               (aref p1 1)
               (aref p2 0)
               (aref p2 1))))
  (gl:pop-attrib)
  (gl:enable :depth-test :cull-face)
  (gl:polygon-mode :front-and-back *polygon-mode*))

(defun debug-render-bbox (bbox)
  (gl:push-attrib :lighting)
  (gl:disable :lighting :depth-test :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    (gl:load-identity)
    (gl:rect (bb:bbox-left bbox)
             (bb:bbox-back bbox)
             (bb:bbox-right bbox)
             (bb:bbox-front bbox)))
  (gl:pop-attrib)
  (gl:enable :depth-test :cull-face)
  (gl:polygon-mode :front-and-back *polygon-mode*))

(defun render ()
  "Render everything"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-matrix (cad-view:matrix))
  (gl:color 1 1 1)
  (when *texture-p* (gl:enable :texture-2d))
  (cond (*animate-p*
         (dolist (m *models*)
           (gl:bind-texture :texture-2d (car (md2:model-gl-tex-ids m)))
           (render-animation m))
         ;; Check if the last t of last frame was just rendered.
         ;; If so, turn animation off
         (when (>= (incf *t-val* (/ 1.0 *t-divisor*)) 1)
           (setf *t-val* 0)
           (incf *frame-n*)
           (update-caption)
           (if (= *frame-n* (- *frame-count* 1)) ; last frame rendered
               (setf *animate-p* nil))))
        (t
         (dolist (m *models*)
           (gl:bind-texture :texture-2d (car (md2:model-gl-tex-ids m)))
           (render-frame m))))
  (gl:disable :texture-2d)
  (when *bboxes-p*
    (render-bounding-boxes))
  (when *gnome-p*
    (render-gnome))
  (when (cad-view:box-zooming-p)
    (render-zoom-box))
  ;(debug-render-bbox (debug-find-all-bb))
  (sdl:update-display))

(defun init-gl ()
  (gl:clear-color 0 0 .2 1)
  (gl:shade-model :smooth)
  (gl:enable :depth-test :cull-face)
  (gl:cull-face :front))

(defun init-lighting ()
  (gl:enable :lighting :light0))

(defun init (w h file-names)
  (init-gl)
  (init-lighting)
  (setf *models* nil
        *frame-n* 0
        *lighting-p* t
        *polygon-mode* :fill
        *texture-p* t
        *smooth-p* t
        *bboxes-p* nil
        *gnome-p* t
        *animate-p* nil
        *t-divisor* 10
        *frame-n* 0
        *t-val* 0)
  ;; load the models
  (dolist (file-name file-names)
    (push (md2:load-model file-name) *models*))
  ;; all must have the same frame count (I think that's how MD2 works, hrm)
  (unless (apply #'= (mapcar (lambda (m)
                               (length (md2:model-frames m)))
                             *models*))
    (error "Loaded models do not have the same number of frames."))
  (setf *frame-count* (length (md2:model-frames (car *models*)))
        *triangle-count* (loop for m in *models*
                            sum (length (md2:model-triangles m))))
  ;; initialize the cad view with the union of all model's bounding boxes
  ;; at frame 0
  (cad-view:init :center-of-rotation
                 (bb:bbox-center (funcall #'bb:find-union
                                          (mapcar (lambda (m)
                                                    (md2:model-bbox m))
                                                  *models*))))
  ;; setup the projection matrix
  (cad-view:resize w h)
  (fit-all-to-view))

(defun update-caption ()
  (sdl:set-caption (format nil "Triangles:~D  Frame:~D of ~D"
                           *triangle-count* (+ *frame-n* 1)
                           *frame-count*) ""))
(defun help ()
  (princ "
Interface:
  * Mouse
      Left mouse button to rotate
      Middle button to zoom
      Right button to pan
      Ctrl+Left mouse button to drag a zoom box
  * Keys
      1 - top view
      2 - right view
      3 - front view
      4 - isometric view
      5 - left view
      6 - bottom view
      7 - back view      
      a - toggle animation (animation will not start at the last frame)
      b - toggle bounding boxes (disabled during animation)
      c - cycle line -> point -> face polygon modes
      f - fit all loaded models in the view
      g - toggle gnome (disabled during animation)
      h - print these commands
      i - print loaded models info to stdout
      l - toggle lighting (default OpenGL lighting is implemented)
      n - next frame (disabled during animation)
      p - previous frame (disabled during animation)
      q - exit
      s - cycle smooth -> flat shade models
      t - toggle textures 
      s - cycle smooth -> flat shade models
      t - toggle textures")
  (force-output))

(defun debug-find-all-bb ()
  (let* ((mat (3d:make-4x4-matrix-from-gl-matrix (gl:get-float
                                                  :modelview-matrix))))
    (bb:find-bbox
     (mapcan (lambda (m)
               (mapcar (lambda (v)
                         (3d:m*v mat (md2:vertex-v v)))
                       (v->l (md2:frame-vertices
                              (aref (md2:model-frames m)
                                    *frame-n*)))))
             *models*))))

;; XXX: This is not going to scale
(defun fit-all-to-view ()
  "Update the projection matrix so all loaded models fit snugly in the view."
  (let* ((mat (3d:make-4x4-matrix-from-gl-matrix (gl:get-float
                                                  :modelview-matrix)))
         (bb (bb:find-bbox
              (mapcan (lambda (m)
                        (mapcar (lambda (v)
                                  (3d:m*v mat (md2:vertex-v v)))
                                (v->l (md2:frame-vertices
                                       (aref (md2:model-frames m)
                                             *frame-n*)))))
                      *models*))))
    (cad-view:fit (subseq (bb:bbox-coords bb) 0 3)
                  (subseq (bb:bbox-coords bb) 3))))

(defun run (file-names &key (w 500) (h 500))
  (unless (consp file-names)
    (warn "Need a list of at least one model to load, exiting...")
    (return-from run))
  (sdl:with-init ()
    (sdl:window w h
                :hw t
                :opengl t
                :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                     ;; (:sdl-gl-swap-control 1)
                                     ;; (:sdl-gl-accelerated-visual 1)
                                     )
                :title-caption ""
                :icon-caption "")
    (init w h file-names)
    (setf (sdl:frame-rate) 60)
    (update-caption)
    (render)                            ; dont really need this
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (render))
      (:key-up-event (:key key)
                       (case key
                         ((:sdl-key-lctrl)
                          (cad-view:finish-zoom-box 0 0))))
      (:key-down-event (:key key)
                       (case key
                         ((:sdl-key-a)
                          ;; Can't animiate with only one frame.
                          ;; Also, don't start animation if last frame
                          ;; is active.
                          (when (and (> *frame-count* 1)
                                     (/= *frame-n* (- *frame-count* 1)))
                            (setf *animate-p* (not *animate-p*))
                            (when *animate-p*
                              (setf *bboxes-p* nil
                                    *gnome-p* nil
                                    *t-val* 0))))
                         ((:sdl-key-b)
                          (unless *animate-p*
                            (setf *bboxes-p* (not *bboxes-p*))))
                         ((:sdl-key-c)  ; polygon-mode point, line, fill, 
                          (case *polygon-mode*
                            ((:point)
                             (setf *polygon-mode* :line)
                             (gl:polygon-mode :back :line))
                            ((:line)
                             (setf *polygon-mode* :fill)
                             (gl:polygon-mode :back :fill))
                            ((:fill)
                             (setf *polygon-mode* :point)
                             (gl:polygon-mode :back :point))))
                         ((:sdl-key-f)
                          (fit-all-to-view))
                         ((:sdl-key-g)
                          (unless *animate-p*
                            (setf *gnome-p* (not *gnome-p*))))
                         ((:sdl-key-i)
                          (mapcar #'md2:print-model-info *models*))
                         ((:sdl-key-h)
                          (help))
                         ((:sdl-key-l)  ; lighting on/off
                          (case *lighting-p*
                            ((nil)
                             (setf *lighting-p* t)
                             (gl:enable :lighting))
                            ((t)
                             (setf *lighting-p* nil)
                             (gl:disable :lighting))))
                         ((:sdl-key-n)  ; next frame
                          (unless *animate-p*
                            (incf *frame-n*)
                            (when (= *frame-n*
                                     (length (md2:model-frames
                                              (car *models*))))
                              (setf *frame-n* 0))
                            (update-caption)))
                         ((:sdl-key-p)  ; previous frame
                          (unless *animate-p*
                            (decf *frame-n*)
                            (when (< *frame-n* 0)
                              (setf *frame-n* (- (length
                                                  (md2:model-frames
                                                   (car *models*)))
                                                 1)))
                            (update-caption)))
                         ((:sdl-key-q)  ; quit
                          (sdl:push-quit-event))
                         ((:sdl-key-s)  ; smooth/flat shade
                          (case *smooth-p*
                            ((nil)
                             (setf *smooth-p* t)
                             (gl:shade-model :smooth))
                            ((t)
                             (setf *smooth-p* nil)
                             (gl:shade-model :flat))))
                         ((:sdl-key-t)  ; texture on/off
                          (case *texture-p*
                            ((nil)
                             (setf *texture-p* t)
                             (gl:enable :texture-2d))
                            ((t)
                             (setf *texture-p* nil)
                             (gl:disable :texture-2d))))
                         ((:sdl-key-1)
                          (cad-view:top-view)
                          (fit-all-to-view))
                         ((:sdl-key-2)
                          (cad-view:right-view)
                          (fit-all-to-view))
                         ((:sdl-key-3)
                          (cad-view:front-view)
                          (fit-all-to-view))
                         ((:sdl-key-4)
                          (cad-view:iso-view)
                          (fit-all-to-view))
                         ((:sdl-key-5)
                          (cad-view:left-view)
                          (fit-all-to-view))
                         ((:sdl-key-6)
                          (cad-view:bottom-view)
                          (fit-all-to-view))
                         ((:sdl-key-7)
                          (cad-view:back-view)
                          (fit-all-to-view))))
      (:mouse-button-down-event (:button b :x x :y y)
                                (when (and (eq b sdl:sdl-button-left)
                                           (sdl:key-down-p :sdl-key-lctrl))
                                  (cad-view:start-zoom-box x y)))
      (:mouse-button-up-event (:button b :x x :y y)
                                (when (and (cad-view:box-zooming-p)
                                           (eq b sdl:sdl-button-left)
                                           (sdl:key-down-p :sdl-key-lctrl))
                                  (cad-view:finish-zoom-box x y)
                                  (cad-view:fit-zoom-box)))
      ;; rotate, zoom, pan
      (:mouse-motion-event (:state s :x-rel dx :y-rel dy :x x :y y)
                           (cond ((eq s sdl:sdl-button-left)
                                  (if (cad-view:box-zooming-p)
                                      (cad-view:update-zoom-box x y)
                                      (cad-view:rotate dx dy)))
                                 ((eq s sdl:sdl-button-middle)
                                  (cad-view:zoom dy))
                                 ((= s 4)
                                  (cad-view:pan dx dy))))
      (:idle
       (render)))))
