(in-package #:threegl)

(defclass grid-helper (object)
  ((grid-width :initarg :grid-width :initform 10 :accessor grid-width)
   (grid-height :initarg :grid-height :initform 10 :accessor grid-height)
   (cell-size :initarg :cell-size :initform 1 :accessor cell-size)
   (mesh :accessor grid-mesh)))

(defun create-grid-mesh (g)
  ())

(defmethod render-object ((g grid-helper))
  (unless (grid-mesh g)
    (create-grid-mesh g))
  (set-transform (world-transform g))
  (render-mesh (grid-mesh g)))
