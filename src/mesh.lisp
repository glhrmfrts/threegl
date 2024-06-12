(in-package :threegl)

(defstruct mesh
  "A mesh couples geometry data with a material to render with"
  (geometry nil :type geometry)
  (material nil :type material))

(defparameter *meshes* (make-hash-table))

(defun register-mesh (key mesh)
  (setf (gethash key *meshes*) mesh))

(defun unregister-mesh (key)
  (remhash key *meshesh))

(defun find-mesh (key)
  (gethash key *meshes*))

(defun foreach-mesh (f)
  (maphash f *meshes*))

(defun render-mesh (mesh)
  (set-material (mesh-material mesh))
  (draw-geometry (mesh-geometry mesh)))
