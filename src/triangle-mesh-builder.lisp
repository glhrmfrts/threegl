(in-package #:threegl)

(defconstant +tmb-initial-elements+ 4096)
(defconstant +tmb-vertex-components+ 3)
(defconstant +tmb-uv-components+ 2)
(defconstant +tmb-normal-components+ 3)
(defconstant +tmb-color-components+ 4)

(defstruct triangle-mesh-builder
  (n-elems :accessor tmb-n-elems)
  (n-indices :accessor tmb-n-indices)
  (vertices :accessor tmb-vertices)
  (uvs :accessor tmb-uvs)
  (normals :accessor tmb-normals)
  (colors :accessor tmb-colors)
  (indices :accessor tmb-indices))

(defun triangle-mesh-n-elements (b) (tmb-n-elems b))
(defun triangle-mesh-n-indices (b) (tmb-n-indices b))
(defun triangle-mesh-vertices (b) (tmb-vertices b))
(defun triangle-mesh-uvs (b) (tmb-uvs b))
(defun triangle-mesh-normals (b) (tmb-normals b))
(defun triangle-mesh-colors (b) (tmb-colors b))
(defun triangle-mesh-indices (b) (tmb-indices b))

(defun triangle-mesh-begin (b &key (vertices-p nil) (uvs-p nil) (normals-p nil) (colors-p nil))
  (declare (type triangle-mesh-builder b))
  (setf (tmb-n-elems b) 0)
  (setf (tmb-n-indices b) 0)
  (if (tmb-indices b)
      (adjust-array (tmb-indices b) (* 1 +tmb-initial-elements+) :element-type integer :fill-pointer 0)
      (setf (tmb-indices) (make-array (* 1 +tmb-initial-elements+) :element-type integer :fill-pointer 0)))
  (when vertices-p
    (if (tmb-vertices b)
	(adjust-array (tmb-vertices b) (* +tmb-vertex-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0)
	(setf (tmb-vertices) (make-array (* +tmb-vertex-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0))))
  (when uvs-p
    (if (tmb-uvs b)
	(adjust-array (tmb-uvs b) (* +tmb-uv-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0)
	(setf (tmb-uvs) (make-array (* +tmb-uv-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0))))
  (when normals-p
    (if (tmb-normals b)
	(adjust-array (tmb-normals b) (* +tmb-normal-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0)
	(setf (tmb-normals) (make-array (* +tmb-normal-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0))))
  (when colors-p
    (if (tmb-colors b)
	(adjust-array (tmb-colors b) (* +tmb-color-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0)
	(setf (tmb-vertices) (make-array (* +tmb-color-components+ +tmb-initial-elements+) :element-type single-float :fill-pointer 0))))
  )

(defun triangle-mesh->geometry (b)
  (let ((attrs nil))
    (when (tmb-vertices b)
      (push
       (make-attribute-with-items (tmb-vertices b)
				  :kind +vertex-attribute-position+
				  :element-type :scalar
				  :component-type :float
				  :component-count 3)
       attrs))
    (when (tmb-uvs b)
      (push
       (make-attribute-with-items (tmb-vertices b)
				  :kind +vertex-attribute-texcoord0+
				  :element-type :scalar
				  :component-type :float
				  :component-count 2)
       attrs))
    (when (tmb-normals b)
      (push
       (make-attribute-with-items (tmb-vertices b)
				  :kind +vertex-attribute-normal+
				  :element-type :scalar
				  :component-type :float
				  :component-count 3)
       attrs))
    (when (tmb-colors b)
      (push
       (make-attribute-with-items (tmb-colors b)
				  :kind +vertex-attribute-color+
				  :element-type :scalar
				  :component-type :float
				  :component-count 4)
       attrs))
    (let ((geo (create-geometry :triangles attrs :static)))
      (geometry-set-indices geo (make-attribute-with-items (tmb-indices b)
							   :element-type :scalar
							   :component-type :unsigned-int
							   :component-count 1))
      (upload-geometry geo)
      geo)))

(defun triangle-mesh-add-vertex (b vtx)
  (vector-push (vx vtx) (tmb-vertices b))
  (vector-push (vy vtx) (tmb-vertices b))
  (vector-push (vz vtx) (tmb-vertices b))
  (incf (tmb-n-elems b)))

(defun triangle-mesh-add-color (b c)
  (vector-push (vx c) (tmb-colors b))
  (vector-push (vy c) (tmb-colors b))
  (vector-push (vz c) (tmb-colors b))
  (vector-push (vw c) (tmb-colors b)))

(defun triangle-mesh-add-index (b idx)
  (vector-push idx (tmb-indices b))
  (incf (tmb-n-indices b)))

(defun triangle-mesh-add-triangle-vc (b v1 v2 v3 c1 c2 c3)
  (declare (type triangle-mesh-builder b))
  (let ((base-idx (tmb-n-elems b)))
    (triangle-mesh-add-vertex b v1)
    (triangle-mesh-add-vertex b v2)
    (triangle-mesh-add-vertex b v3)
    (triangle-mesh-add-color b c1)
    (triangle-mesh-add-color b c2)
    (triangle-mesh-add-color b c3)
    (triangle-mesh-add-index b (+ base-idx 0))
    (triangle-mesh-add-index b (+ base-idx 1))
    (triangle-mesh-add-index b (+ base-idx 2))
    ))

(defun triangle-mesh-add-quad-vc (b v1 v2 v3 v4 c1 c2 c3 c4)
  (declare (type triangle-mesh-builder b))
  (let ((base-idx (tmb-n-elems b)))
    (triangle-mesh-add-vertex b v1)
    (triangle-mesh-add-vertex b v2)
    (triangle-mesh-add-vertex b v3)
    (triangle-mesh-add-vertex b v4)
    (triangle-mesh-add-color b c1)
    (triangle-mesh-add-color b c2)
    (triangle-mesh-add-color b c3)
    (triangle-mesh-add-color b c4)
    (triangle-mesh-add-index b (+ base-idx 0))
    (triangle-mesh-add-index b (+ base-idx 1))
    (triangle-mesh-add-index b (+ base-idx 2))
    (triangle-mesh-add-index b (+ base-idx 0))
    (triangle-mesh-add-index b (+ base-idx 2))
    (triangle-mesh-add-index b (+ base-idx 3))
    ))

(defun triangle-mesh-add-cube-vc (b center size color)
  (declare (type triangle-mesh-builder b))
  (let* ((half-size (v* size 0.5))
         (cx (vx center))
         (cy (vy center))
         (cz (vz center))
         ;; Vertices of the cube
         (v1 (vec (- cx half-size) (- cy half-size) (- cz half-size)))
         (v2 (vec (+ cx half-size) (- cy half-size) (- cz half-size)))
         (v3 (vec (+ cx half-size) (+ cy half-size) (- cz half-size)))
         (v4 (vec (- cx half-size) (+ cy half-size) (- cz half-size)))
         (v5 (vec (- cx half-size) (- cy half-size) (+ cz half-size)))
         (v6 (vec (+ cx half-size) (- cy half-size) (+ cz half-size)))
         (v7 (vec (+ cx half-size) (+ cy half-size) (+ cz half-size)))
         (v8 (vec (- cx half-size) (+ cy half-size) (+ cz half-size)))
         ;; Colors for each vertex
         (c1 color)
         (c2 color)
         (c3 color)
         (c4 color)
         (c5 color)
         (c6 color)
         (c7 color)
         (c8 color))
    ;; Add each face of the cube
    ;; Front face (v1, v2, v3, v4)
    (triangle-mesh-add-quad-vc b v1 v2 v3 v4 c1 c2 c3 c4)
    ;; Back face (v5, v8, v7, v6)
    (triangle-mesh-add-quad-vc b v5 v8 v7 v6 c5 c8 c7 c6)
    ;; Left face (v1, v4, v8, v5)
    (triangle-mesh-add-quad-vc b v1 v4 v8 v5 c1 c4 c8 c5)
    ;; Right face (v2, v6, v7, v3)
    (triangle-mesh-add-quad-vc b v2 v6 v7 v3 c2 c6 c7 c3)
    ;; Top face (v4, v3, v7, v8)
    (triangle-mesh-add-quad-vc b v4 v3 v7 v8 c4 c3 c7 c8)
    ;; Bottom face (v1, v5, v6, v2)
    (triangle-mesh-add-quad-vc b v1 v5 v6 v2 c1 c5 c6 c2)))
