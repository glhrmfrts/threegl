(in-package #:threegl)

(deftype texture-format '(member r8 rgba8 rgba16f depth-component32f))

(defconstant +max-texture-units+ 5)

(defstruct texture
  (id 0)
  (fmt :rgba)
  (width 0)
  (height 0))

(defun texture-format-to-gl-format (fmt)
  (case fmt
    (:r8 :red)
    (:rgba8 :rgba)
    (:rgba16f :rgba)
    (:depth-component32f :depth-component)
    (otherwise nil)))

(defun texture-format-components (fmt)
  (case fmt
    (:r8 1)
    (:rgba8 4)
    (:rgba16f 4)
    (:depth-component32f 1)
    (otherwise nil)))

(defun texture-format-data-type (fmt)
  (case fmt
    (:r8 :unsigned-byte)
    (:rgba8 :unsigned-byte)
    (:rgba16f :unsigned-short)
    (:depth-component32f :float)
    (otherwise nil)))

(defun components-to-format (n)
  (case n
    (1 :r8)
    (3 :rgba8)
    (4 :rgba8)
    (otherwise nil)))

(defun png-color-type-to-format (ct)
  (case ct
    (:greyscale :r8)
    (:truecolour :rgba8)
    (:truecolour-alpha :rgba8)
    (otherwise nil)))

(defun bind-texture (tex unit)
  (gl:active-texture unit)
  (gl:bind-texture :texture-2d (texture-id tex)))

(defun create-texture-2d (data &key (width 0) (height 0) (pformat 0) (filter :nearest) (wrap :repeat))
  (let ((tex (gl:gen-texture))
        (gl-format (texture-format-to-gl-format pformat)))
    (cffi:with-pointer-to-vector-data (data-ptr data)
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d tex)
      (gl:tex-parameter :texture-2d :texture-min-filter filter)
      (gl:tex-parameter :texture-2d :texture-mag-filter filter)
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
      (gl:tex-image-2d :texture-2d 0 gl-format width height 0 gl-format (texture-format-data-type pformat) data-ptr)
      (make-texture :id tex :width width :height height :fmt pformat))))

(defun to-rgba (data)
  (let* ((clen (/ (length data) 3))
         (rgba (make-array (* clen 4) :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below clen :do
      (let ((src-index (* i 3))
            (dst-index (* i 4)))
        (setf (aref rgba dst-index) (aref data src-index))
        (setf (aref rgba (+ dst-index 1)) (aref data (+ src-index 1)))
        (setf (aref rgba (+ dst-index 2)) (aref data (+ src-index 2)))
        (setf (aref rgba (+ dst-index 3)) 255)))
    rgba))

(defparameter *textures* (make-hash-table :test 'equal))

(defun keep-texture (key tex)
  (setf (gethash key *textures*) tex))

(defun find-texture (key)
  (gethash key *textures*))

(defun load-texture-resolved-bmp (key filename)
  (let* ((bmp (org.shirakumo.bmp:read-bmp filename)))
    (multiple-value-bind (data width height channels) (org.shirakumo.bmp:decode-pixels bmp)
      (let ((tex (create-texture-2d
                  (if (= channels 3) (to-rgba data) data)
                  :width width
                  :height height
                  :pformat (components-to-format channels))))
        (keep-texture key tex)
        tex))))

(defun load-texture-resolved-png (key filename)
  (labels
      ((flatten-image-data (coltype data w h)
         (let ((rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
           (loop :for y :from 0 :below h
                 :do (loop :for x :from 0 :below w
                           :do (let ((dst-index (* 4 (+ x (* w y)))))
				(setf (aref rgba dst-index) (aref data x y 0))
				(setf (aref rgba (+ 1 dst-index)) (aref data x y 1))
				(setf (aref rgba (+ 2 dst-index)) (aref data x y 2))
				(if (string-equal coltype :truecolor)
				    (setf (aref rgba (+ 3 dst-index)) 255)
				    (setf (aref rgba (+ 3 dst-index)) (aref data x y 3))))))
           rgba)))
    (let* ((png (png-read:read-png-file filename))
           (w (png-read:width png))
           (h (png-read:height png))
           (tex (create-texture-2d (flatten-image-data
				    (png-read:colour-type png)
				    (png-read:image-data png)
				    w h)
                                   :width w
                                   :height h
                                   :pformat (components-to-format 4))))
      (keep-texture key tex)
      tex)))

(defun load-texture-resolved (key filename)
  (cond
    ((alexandria:ends-with-subseq "bmp" (namestring filename))
     (load-texture-resolved-bmp key filename))
    ((alexandria:ends-with-subseq "png" (namestring filename))
     (load-texture-resolved-png key filename))
    (t
     (error "bad texture file extension"))))

(defun load-texture (filename)
  (load-texture-resolved (namestring filename) filename))
