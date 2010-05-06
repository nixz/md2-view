;;; gl-texture.lisp
;;;
;;; S. Edward Dolan <bytecolor@gmail.com>
;;; Monday, April 26 2010
;;; Added Packaging system (May 05 2010) Nikhil J. Shetty <nikhil.j.shetty@gmail.com>

(in-package :gl-texture)


;; TODO: handle tga
(defun load-texture (file-name)
  "Create an OpenGL texture object from the image FILE-NAME.
Return the texture object id."
  (unless sdl:*default-display*
    (error
     "SDL not initialized. Need an active display to convert the image."))
  (unless (sdl-image:image-init-p)
    (sdl-image:init-image))
  (let ((id (car (gl:gen-textures 1))))
    (gl:pixel-store :unpack-alignment 1)
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (let* ((img (sdl:convert-to-display-format
                 :surface (sdl-image:load-image file-name)))
           (format (ecase (sdl:byte-depth img)
                     (3
                      (if (= (sdl:r-mask img) #x000000ff)
                          :rgb
                          :bgr))
                     (4
                      (if (= (sdl:r-mask img) #x000000ff)
                          :rgba
                          :bgra)))))
      (sdl:with-pixel (p (sdl:fp img))
        (gl:tex-image-2d :texture-2d
                         0
                         (sdl:byte-depth img)
                         (sdl:width img)
                         (sdl:height img)
                         0
                         format
                         :unsigned-byte
                         (sdl:pixel-data p)))
      (sdl:free img)
      id)))
