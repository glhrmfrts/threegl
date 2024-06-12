(in-package :threegl)

(bs:define-io-structure one-float
  (x float32))

(bs:define-io-structure one-integer
  (x sint32))

(defstruct view-data
  (proj (meye 4) :type mat4)
  (view (meye 4) :type mat4)
  (projview (meye 4) :type mat4)
  (invprojview (meye 4) :type mat4))

(defstruct object-data
  (transform (meye 4) :type mat4)
  (color (vec 1 1 1 1) :type vec4))

(defparameter *view-data* (make-view-data))
(defparameter *object-data* (make-object-data))

(defparameter *view-ubo* nil)
(defparameter *object-ubo* nil)

(defparameter *view-dirty* t)
(defparameter *object-dirty* t)

(defparameter *current-material* nil)
(defparameter *current-shader* nil)

(defgeneric write-gl-value (x buf))

(defmethod write-gl-value ((x float) buf)
  (write-one-float (make-one-float :x x) buf))

(defmethod write-gl-value ((x integer) buf)
  (write-one-integer (make-one-integer :x x) buf))

(defmethod write-gl-value ((x vec4) buf)
  (write-gl-value (vx4 x) buf)
  (write-gl-value (vy4 x) buf)
  (write-gl-value (vz4 x) buf)
  (write-gl-value (vw4 x) buf))

(defmethod write-gl-value ((x mat4) buf)
  (write-gl-value (mcol x 0) buf)
  (write-gl-value (mcol x 1) buf)
  (write-gl-value (mcol x 2) buf)
  (write-gl-value (mcol x 3) buf))

(defmethod write-gl-value ((x view-data) buf)
  (write-gl-value (view-data-proj x) buf)
  (write-gl-value (view-data-view x) buf)
  (write-gl-value (view-data-projview x) buf)
  (write-gl-value (view-data-invprojview x) buf))

(defmethod write-gl-value ((x object-data) buf)
  (write-gl-value (object-data-transform x) buf)
  (write-gl-value (object-data-color x) buf))

(defun init-render-state ()
  (setf *view-ubo* (create-uniform-buffer "viewData" (* 64 4) :dynamic-draw))
  (setf *object-ubo* (create-uniform-buffer "objectData" (* 20 4) :dynamic-draw)))

(defun destroy-render-state ()
  (destroy-static-shaders))

(defun upload-view-data ()
  (let ((buf (flexi:with-output-to-sequence (output :element-type '(unsigned-byte 8))
              (write-gl-value *view-data* output))))
      ;(format t "~a~%" buf)
      (upload-uniform *view-ubo* (create-gl-array :uchar :scalar buf))))

(defun upload-object-data ()
  (let ((buf (flexi:with-output-to-sequence (output :element-type '(unsigned-byte 8))
              (write-gl-value *object-data* output))))
      ;(format t "~a~%" buf)
      (upload-uniform *object-ubo* (create-gl-array :uchar :scalar buf))))

(defun set-clear-color (r g b a)
  (gl:clear-color r g b a))

(defun clear (&rest bufs)
  (apply #'gl:clear bufs))

(defun set-shader (sh)
  (gl:use-program (shader-program sh)))

(defun set-projection-view (proj view)
  (setf (view-data-proj *view-data*) proj)
  (setf (view-data-view *view-data*) view)
  (setf (view-data-projview *view-data*) (m* proj view))
  (setf (view-data-invprojview *view-data*) (minv (m* proj view)))
  (setf *view-dirty* t))

(defun set-transform (transform)
  (setf (object-data-transform *object-data*) transform)
  (setf *object-dirty* t))

(defun set-color (c)
  (setf (object-data-color *object-data*) c)
  (setf *object-dirty* t))

(defun set-material (mat)
  (set-shader (material-shader mat))
  (set-color (material-color mat)))

(defun sync-ubos ()
  (when *view-dirty*
    (progn
      (upload-view-data)
      (setf *view-dirty nil)))
  (when *object-dirty*
    (progn
      (upload-object-data)
      (setf *object-dirty* nil))))

(defun draw-arrays (geo prim start count)
  (sync-ubos)
  (gl:bind-vertex-array (geometry-vao geo))
  (gl:draw-arrays prim start count))

(defun draw-elements (geo prim start count)
  (sync-ubos)
  (gl:bind-vertex-array (geometry-vao geo))
  (gl:draw-elements prim
                    (gl:make-null-gl-array :unsigned-short)
                    :offset start
                    :count count))
