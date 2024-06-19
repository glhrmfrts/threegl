(in-package #:threegl)

(defconstant +object-flag-static+ 1)
(defconstant +object-flag-transform-dirty+ 4096)

(defclass object ()
  ((name :initarg :name :initform "" :accessor name)
   (translation :initarg :translation :initform (vec 0 0 0) :accessor translation)
   (rotation :initarg :rotation :initform (quat 0 0 0 1) :accessor rotation)
   (m-world-rotation :initform (quat 0 0 0 1) :accessor m-world-rotation)
   (scale :initarg :scale :initform (vec 1 1 1) :accessor scale)
   (parent :initarg :parent :initform nil :accessor parent)
   (children :initarg :children :initform nil :accessor children)
   (m-transform :initform (meye 4) :accessor m-transform)
   (m-world-transform :initform (meye 4) :accessor m-world-transform)
   (flags :initarg :flags :initform +object-flag-transform-dirty+ :accessor object-flags)))

(defun has-flag-p (obj flag)
  (/= 0 (logand (object-flags obj) flag)))

(defun add-flag (obj flag)
  (setf (object-flags obj) (logior (object-flags obj) flag)))

(defun add-flag-recur (obj flag)
  (setf (object-flags obj) (logior (object-flags obj) flag))
  (dolist (child (children obj))
    (add-flag-recur child flag)))

(defun remove-flag (obj flag)
  (setf (object-flags obj) (logand (object-flags obj) (lognot flag))))

(defclass model (object)
  ((mesh :initarg :mesh :initform nil :type mesh :accessor model-mesh)
   (material :initarg :material :initform nil :type material :accessor model-material)
   (instance-id :initarg :instance-id :initform nil :type integer :accessor model-instance-id)))

(defclass camera (object)
  ((projection :initform (meye 4) :accessor camera-projection)
   (view :initform (meye 4) :accessor camera-view)
   (type :initarg :type :initform :perspective :accessor camera-type)
   (fov :initarg :fov :initform 70.0 :accessor camera-fov)
   (near :initarg :near :initform 0.1 :accessor camera-near)
   (far :initarg :far :initform 1000.0 :accessor camera-far)))

(defun camera-matrices (cam)
  (setf (camera-projection cam)
	(mperspective (camera-fov cam)
		      (/ (float *view-width*) (float *view-height*))
		      (camera-near cam)
		      (camera-far cam)))
  (setf (camera-view cam) (minv (world-transform cam)))
  (values (camera-projection cam) (camera-view cam)))

(defclass scene (object) ())

(defgeneric render-object (object))
(defmethod render-object ((obj object)) ())

(defun update-world-transform (obj)
  (labels
      ((cont (m)
	 (setf (m-transform obj) m)
	 (if (parent obj)
	     (progn
	       (setf (m-world-transform obj)
		     (m* (world-transform (parent obj)) m)))
	     (setf (m-world-transform obj) m))))
    (let ((self (meye 4)))
      (!qmat self (rotation obj))
      (setf self (m*
		  (mtranslation (translation obj))
		  (mscaling (scale obj))
		  self))
      (cont self)
      (values))))

(defun world-transform (obj)
  (when (has-flag-p obj +object-flag-transform-dirty+)
    (update-world-transform obj)
    (remove-flag obj +object-flag-transform-dirty+))
  (m-world-transform obj))

(defun world-translation (obj)
  (let ((pos (mcol (world-transform obj) 3)))
    (vec3 (elt pos 0) (elt pos 1) (elt pos 2))))

(defun world-rotation (obj)
  (if (parent obj)
      (q* (world-rotation (parent obj)) (rotation obj))
      (rotation obj)))

(defun world-front-vector (obj)
  (q* (world-rotation obj) (vec 0.0 0.0 -1.0)))

(defun world-right-vector (obj)
  (q* (world-rotation obj) (vec 1.0 0.0 0.0)))

(defun world-up-vector (obj)
  (q* (world-rotation obj) (vec 0.0 1.0 0.0)))

(defun add-child (obj child)
  (assert (null (parent child)))
  (setf (parent child) obj)
  (setf (children obj) (append (children obj) (list child))))

(defun traverse (obj f)
  (funcall f obj)
  (dolist (child (children obj))
    (traverse child f)))

(defun traverse-with-parent (obj f &optional parent)
  (funcall f obj parent)
  (dolist (child (children obj))
    (traverse-with-parent child f obj)))

(defun link (&rest roots)
  (let ((f (lambda (child p) (setf (parent child) p))))
    (dolist (root roots)
      (traverse-with-parent root f))))

(defmethod render-object ((m model))
  (if (geometry-instanced-p (mesh-geometry (model-mesh m)))
      (set-instance-transform (mesh-geometry (model-mesh m))
			      (model-instance-id m)
			      (world-transform m))
      (progn
	(set-transform (world-transform m))
	(render-mesh (model-mesh m) (model-material m)))))

(defun render-scene (s cam)
  (gl:enable :depth-test)
  (multiple-value-bind (proj view) (camera-matrices cam)
    (set-projection-view proj view))
  (traverse s (lambda (child) (render-object child)))
  (render-instanced-meshes)
  (gl:disable :depth-test))
