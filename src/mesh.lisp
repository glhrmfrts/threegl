(in-package :threegl)

(defstruct mesh
  "A mesh couples geometry data with a material to render with"
  (name "")
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

(defun create-mesh (&rest other-keys)
  (let ((m (apply #'make-mesh other-keys)))
    (register-mesh (getf other-keys :name) m)
    m))

(defun create-mesh-instance (mesh)
  (create-instance (mesh-geometry mesh)))

(defun render-mesh (mesh &optional custom-material)
  (set-material (or custom-material (mesh-material mesh)))
  (draw-geometry (mesh-geometry mesh)))

(defun render-instanced-meshes ()
  (loop :for mesh :being :each :hash-value :of *meshes* :do
    (when (geometry-instanced-p (mesh-geometry mesh))
      (upload-geometry (mesh-geometry mesh))
      (set-material (mesh-material mesh))
      (draw-geometry-instanced (mesh-geometry mesh)))))
