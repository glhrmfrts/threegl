(in-package :threegl)

(defstruct geometry
  (vao 0)
  (ebo 0)
  (attrs nil)
  (usage :static)
  (vertex-size 0)
  (index-size 0))

(defun create-gl-array (data-type elems)
  (let ((arr (gl:alloc-gl-array data-type (length elems))))
    (dotimes (i (length elems))
      (setf (gl:glaref arr i) (aref elems i)))
    arr))

(defun gl-data-type-size (data-type)
  (ecase data-type
    (:unsigned-byte 1)
    (:unsigned-short 2)
    (:float 4)
    (:unsigned-int 4)))

(defun create-geometry (attrs idx-format usage)
  (let* ((vao (gl:gen-vertex-array))
         (ebo (if idx-format (gl:gen-buffer) nil)))
    (gl:bind-vertex-array vao)
    (make-geometry-buffer :vao vao
                          :ebo ebo
                          :usage usage
                          :vertex-size 0
                          :index-size 0
                          :attrs (mapcar (lambda (attr)
                                           (progn
                                             (enable-vertex-attribute attr)
                                             attr))))))

(defun upload-geometry (geo &key (indices nil))
  (loop for attr in (geometry-attrs geo)
        do (upload-vertex-buffer attr))
  (when indices
    (progn
      (gl:bind-buffer :element-array-buffer (geometry-buffer-ebo buf))
      (gl:buffer-data :element-array-buffer (geometry-buffer-usage buf) indices))))

(defun draw-geometry (buf count)
  (gl:bind-vertex-array (geometry-buffer-vao buf))
  (gl:draw-arrays :triangles 0 count))
