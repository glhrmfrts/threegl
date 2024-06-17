(in-package :threegl)

(deftype index-format () '(member :uint16 :uint32))

(defstruct geometry
  (vao 0)
  (primitives nil)
  (attrs nil)
  (indices nil)
  (usage :static)
  (vertex-draw-count 0))

(defun create-geometry (primitives attrs usage)
  (declare (type usage buffer-usage))
  (let* ((vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)
    (loop for attr in attrs do (enable-vertex-attribute attr))
    (make-geometry :vao vao :attrs attrs :usage usage :primitives primitives)))

(defun geometry-set-indices (geo indices)
  (declare (type geo geometry)
           (type indices attribute))
  (setf (geometry-indices geo) indices))

(defun find-attribute (geo kind)
  (find-if (lambda (attr) (= (attribute-kind attr) kind)) (geometry-attrs geo)))

(defun upload-geometry (buf)
  (loop for attr in (geometry-attrs buf) do
        (upload-attribute attr)
        (when (= (attribute-kind attr) +vertex-attribute-position+)
          (setf (geometry-vertex-draw-count buf)
                (/ (attribute-n-items attr) (attribute-component-count attr)))))
  (when (geometry-indices buf)
    (setf (geometry-vertex-draw-count buf)
          (/ (attribute-n-items (geometry-indices buf)) (attribute-component-count (geometry-indices buf))))
    (upload-attribute (geometry-indices buf))))

(defun draw-geometry (buf &key (offset 0) (count (geometry-vertex-draw-count buf)))
  (gl:bind-vertex-array (geometry-vao buf))
  (if (geometry-indices buf)
      (progn
        (bind-attribute (geometry-indices buf))
        (draw-elements buf (geometry-primitives buf) offset count))
      (draw-arrays buf (geometry-primitives buf) offset count)))

(defun generate-cube-data ()
  "Generates vertices and colors for a cube, all colors set to white."
  (let* ((vertices (mapcar #'float '(
                             ;; Front face
                                     -1 -1  1         ; Bottom-left
                                     1 -1  1          ; Bottom-right
                                     1  1  1          ; Top-right
                                     -1 -1  1         ; Bottom-left
                                     1  1  1          ; Top-right
                                     -1  1  1         ; Top-left
                                     ;; Back face
                                     -1 -1 -1         ; Bottom-left
                                     -1  1 -1         ; Top-left
                                     1  1 -1          ; Top-right
                                     -1 -1 -1         ; Bottom-left
                                     1  1 -1          ; Top-right
                                     1 -1 -1          ; Bottom-right
                                     ;; Top face
                                     -1  1 -1         ; Top-left
                                     -1  1  1         ; Bottom-left
                                     1  1  1          ; Bottom-right
                                     -1  1 -1         ; Top-left
                                     1  1  1          ; Bottom-right
                                     1  1 -1          ; Top-right
                                     ;; Bottom face
                                     -1 -1 -1         ; Top-left
                                     1 -1 -1          ; Top-right
                                     1 -1  1          ; Bottom-right
                                     -1 -1 -1         ; Top-left
                                     1 -1  1          ; Bottom-right
                                     -1 -1  1         ; Bottom-left
                                     ;; Right face
                                     1 -1 -1          ; Bottom-left
                                     1  1 -1          ; Top-left
                                     1  1  1          ; Top-right
                                     1 -1 -1          ; Bottom-left
                                     1  1  1          ; Top-right
                                     1 -1  1          ; Bottom-right
                                     ;; Left face
                                     -1 -1 -1         ; Bottom-right
                                     -1 -1  1         ; Bottom-left
                                     -1  1  1         ; Top-left
                                     -1 -1 -1         ; Bottom-right
                                     -1  1  1         ; Top-left
                                     -1  1 -1         ; Top-right
                             )))
         (colors (loop repeat 36 append '(1.0 1.0 1.0 1.0))))  ; 24 vertices, each with the color white (1.0, 1.0, 1.0)
    (values vertices colors)))

(defun generate-textured-cube-data ()
  "Generates vertices and texture coordinates for a cube."
  (let* ((vertices '(
                     ;; Front face
                     -1 -1  1  ; Bottom-left
                     1 -1  1  ; Bottom-right
                     1  1  1  ; Top-right
                     -1  1  1  ; Top-left
                     ;; Back face
                     -1 -1 -1  ; Bottom-left
                     -1  1 -1  ; Top-left
                     1  1 -1  ; Top-right
                     1 -1 -1  ; Bottom-right
                     ;; Top face
                     -1  1 -1  ; Top-left
                     -1  1  1  ; Bottom-left
                     1  1  1  ; Bottom-right
                     1  1 -1  ; Top-right
                     ;; Bottom face
                     -1 -1 -1  ; Top-left
                     1 -1 -1  ; Top-right
                     1 -1  1  ; Bottom-right
                     -1 -1  1  ; Bottom-left
                     ;; Right face
                     1 -1 -1  ; Bottom-left
                     1  1 -1  ; Top-left
                     1  1  1  ; Top-right
                     1 -1  1  ; Bottom-right
                     ;; Left face
                     -1 -1 -1  ; Bottom-right
                     -1 -1  1  ; Bottom-left
                     -1  1  1  ; Top-left
                     -1  1 -1  ; Top-right
                     ))
         (texture-coords '(
                           ;; Front face
                           0.0 0.0  ; Bottom-left
                           1.0 0.0  ; Bottom-right
                           1.0 1.0  ; Top-right
                           0.0 1.0  ; Top-left
                           ;; Back face
                           1.0 0.0  ; Bottom-left
                           1.0 1.0  ; Top-left
                           0.0 1.0  ; Top-right
                           0.0 0.0  ; Bottom-right
                           ;; Top face
                           0.0 1.0  ; Top-left
                           0.0 0.0  ; Bottom-left
                           1.0 0.0  ; Bottom-right
                           1.0 1.0  ; Top-right
                           ;; Bottom face
                           1.0 1.0  ; Top-left
                           0.0 1.0  ; Top-right
                           0.0 0.0  ; Bottom-right
                           1.0 0.0  ; Bottom-left
                           ;; Right face
                           1.0 0.0  ; Bottom-left
                           1.0 1.0  ; Top-left
                           0.0 1.0  ; Top-right
                           0.0 0.0  ; Bottom-right
                           ;; Left face
                           0.0 0.0  ; Bottom-right
                           1.0 0.0  ; Bottom-left
                           1.0 1.0  ; Top-left
                           0.0 1.0  ; Top-right
                           )))
    (values vertices texture-coords)))

(defun create-cube-geometry ()
  (multiple-value-bind (vertices colors) (generate-cube-data)
    (let* ((vert-attr (make-attribute-with-items (coerce vertices 'vector)
                                                 :kind +vertex-attribute-position+
                                                 :element-type :scalar
                                                 :component-type :float
                                                 :component-count 3))
           (color-attr (make-attribute-with-items (coerce colors 'vector)
                                                  :kind +vertex-attribute-color+
                                                  :element-type :scalar
                                                  :component-type :float
                                                  :component-count 4))
           (geo (create-geometry :triangles (list vert-attr color-attr) :static)))
      (upload-geometry geo)
      geo)))

(defun generate-screen-data ()
  (let ((vertices '(-1.0 -1.0 0.0
		    1.0 -1.0 0.0
		    1.0 1.0 0.0
		    -1.0 -1.0 0.0
		    1.0 1.0 0.0
		    -1.0 1.0 0.0))
	(tex-coords '(0.0 0.0
		      1.0 0.0
		      1.0 1.0
		      0.0 0.0
		      1.0 1.0
		      0.0 1.0)))
    (values vertices tex-coords)))

(defun create-screen-geometry ()
  (multiple-value-bind (vertices tex-coords) (generate-screen-data)
    (let* ((vert-attr (make-attribute-with-items (coerce vertices 'vector)
						:kind +vertex-attribute-position+
						:element-type :scalar
						:component-type :float
						:component-count 3))
	   (texcoords-attr (make-attribute-with-items (coerce tex-coords 'vector)
						     :kind +vertex-attribute-texcoord0+
						     :element-type :scalar
						     :component-type :float
						     :component-count 2))
	   (geo (create-geometry :triangles (list vert-attr texcoords-attr) :static)))
      (upload-geometry geo)
      geo)))
