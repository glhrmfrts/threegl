(in-package #:threegl)

(defclass object ()
  ((name :initarg :name :initform "" :accessor name)
   (translation :initarg :translation :initform (vec 0 0 0) :accessor translation)
   (scale :initarg :scale :initform (vec 1 1 1) :accessor scale)
   (children :initarg :children :initform nil :accessor children)
   (m-world-transform :initform (meye 4) :accessor m-world-transform)
   (world-transform-dirty :initform t :accessor world-transform-dirty)))

(defclass model (object)
  ((mesh :initarg :mesh :initform nil :type mesh :accessor model-mesh)))

(defclass camera (object)
  ((projection :initform (meye 4) :accessor projection)
   (view :initform (meye 4) :accessor view)))

(defclass scene (object) ())

(defgeneric render-object (object) ())

(defun update-world-transform (obj)
  (let ((m (3d-matrices:m*
	    (3d-matrices:mtranslation (translation obj))
	    (mscaling (scale obj)))))
    (setf (m-world-transform obj) m)))

(defun world-transform (obj)
  (when (world-transform-dirty obj)
    (update-world-transform obj)
    (setf (world-transform-dirty obj) nil))
  (m-world-transform obj))

(defun traverse (obj f)
  (funcall f obj)
  (dolist (child (children obj))
    (traverse child f)))

(defmethod render-object ((m model))
  (set-transform (world-transform m))
  (render-mesh (model-mesh m)))

(defun render-scene (s)
  (traverse s (lambda (child) (render-object child))))
