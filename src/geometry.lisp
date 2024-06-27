(in-package #:threegl)

(deftype index-format () '(member :uint16 :uint32))

(defstruct geometry
  (vao 0)
  (primitives nil)
  (attrs nil)
  (indices nil)
  (usage :static-draw)
  (vertex-draw-count 0)
  (instanced-p nil))

(defun create-geometry (primitives attrs usage &key (instanced-p nil))
  (declare (type usage buffer-usage))
  (let* ((vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)
    (loop :for attr :in attrs :do (enable-vertex-attribute attr))
    (make-geometry :vao vao :attrs attrs :usage usage :primitives primitives :instanced-p instanced-p)))

(defun geometry-set-indices (geo indices)
  (declare (type geometry geo)
           (type attribute indices))
  (setf (geometry-indices geo) indices))

(defun find-attribute (geo kind)
  (find-if (lambda (attr) (= (attribute-kind attr) kind)) (geometry-attrs geo)))

(defun upload-geometry (buf)
  (loop :for attr :in (geometry-attrs buf) :do
        (upload-attribute attr)
        (when (= (attribute-kind attr) +vertex-attribute-position+)
          (setf (geometry-vertex-draw-count buf)
                (/ (attribute-n-items attr) (attribute-component-count attr)))))
  (when (geometry-indices buf)
    (setf (geometry-vertex-draw-count buf) (attribute-n-items (geometry-indices buf)))
    (upload-attribute (geometry-indices buf))))

(defun draw-geometry (geo &key (offset 0) (count (geometry-vertex-draw-count geo)))
  (gl:bind-vertex-array (geometry-vao geo))
  (if (geometry-indices geo)
      (progn
        (bind-attribute (geometry-indices geo))
        (draw-elements geo (geometry-primitives geo) offset count))
      (draw-arrays geo (geometry-primitives geo) offset count)
      ))

(defun draw-geometry-instanced (geo &key (offset 0) (count (geometry-vertex-draw-count geo)))
  (gl:bind-vertex-array (geometry-vao geo))
  (if (geometry-indices geo)
      (progn
	(bind-attribute (geometry-indices geo))
	(draw-elements-instanced geo (geometry-primitives geo) offset count))
      (draw-arrays-instanced geo (geometry-primitives geo) offset count)))

(defun create-instance (geo)
  (let* ((attr (find-attribute geo +vertex-attribute-instance-transform+))
	 (id (attribute-n-items attr)))
    (grow-attribute attr 16)
    id))

(defun set-instance-transform (geo id trans)
  (let ((attr (find-attribute geo +vertex-attribute-instance-transform+)))
    (unless attr
      (error "geometry is not instanced"))
    (loop :for col-idx :from 0 :below 4 :do
      (let ((col (mcol trans col-idx)))
	(loop :for i :from 0 :below 4 :do
	  (set-attribute-nth-item attr
				  (+ id (* col-idx 4) i)
				  (elt col i)))))))

(defun geometry-instance-count (geo)
  (/ (attribute-n-items (find-attribute geo +vertex-attribute-instance-transform+)) 16))

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
         (colors (loop :repeat 36 :append '(1.0 1.0 1.0 1.0))))  ; 24 vertices, each with the color white (1.0, 1.0, 1.0)
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

(defun create-cube-geometry (&key (instanced-p nil))
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
	   (transform-attr (if instanced-p (list (make-attribute :kind +vertex-attribute-instance-transform+ :component-type :float :component-count 16 :usage :dynamic-draw :instanced-p t)) ()))
           (geo (create-geometry :triangles (append (list vert-attr color-attr) transform-attr) :static :instanced-p instanced-p)))
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
