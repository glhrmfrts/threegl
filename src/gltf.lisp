(in-package #:threegl)

(defclass gltf-scene (object) ())

(defun gltf-component-type->component-type (ct)
  (case ct
    (:uint8     :unsigned-byte)
    (:uint16    :unsigned-short)
    (:uint32    :unsigned-int)
    (otherwise  ct)))

(defun vector->vec3 (v)
  (vec (elt v 0) (elt v 1) (elt v 2)))

(defun vector->vec4 (v)
  (vec (elt v 0) (elt v 1) (elt v 2) (elt v 3)))

(defun material-from-gltf (gmat rtextures)
  (let ((color (gltf:albedo-factor (gltf:pbr gmat)))
	(tex-info (gltf:albedo (gltf:pbr gmat))))
    (if tex-info
	(create-basic-texture-material
	 :color (vector->vec4 color)
	 :texture (nth (gltf:idx (gltf:source (gltf:texture tex-info))) rtextures))
	(create-basic-vertex-color-material
	 :color (vector->vec4 color)))))

(defun load-texture-from-gltf (model-filename gtex)
  (load-texture
   (merge-pathnames
    (gltf:uri (gltf:source gtex))
    (make-pathname :directory (pathname-directory model-filename)))))

(defun create-mesh-from-gltf (gmesh rtextures)
  (labels
      ((attribute-name->kind (name)
         (ecase name
           (:POSITION +vertex-attribute-position+)
           (:NORMAL +vertex-attribute-normal+)
           (:COLOR +vertex-attribute-color+)
           (:TEXCOORD_0 +vertex-attribute-texcoord0+)
           (:TEXCOORD_1 +vertex-attribute-texcoord1+)
           (:INDICES +vertex-attribute-index+)))
       (collect-accessor-items (att)
         (loop :for i :from 0 :below (length att)
               :collect (if (vectorp (elt att i)) (copy-seq (elt att i)) (elt att i))))
       (get-accessor-items (att)
         (let ((its (coerce (collect-accessor-items att) 'vector)))
           its))
       (map-attribute (k att)
         (make-attribute-with-items
          (get-accessor-items att)
          :target (if (eq k :INDICES) :element-array-buffer :array-buffer)
          :kind (attribute-name->kind k)
          :component-type (gltf-component-type->component-type (gltf:component-type att))
          :component-count (element-type->component-count (gltf:element-type att))
          :element-type (gltf:element-type att)))
       (map-attributes (prim)
         (loop
           :for k :being :each :hash-key :of (gltf:attributes prim)
           :using (hash-value v)
           :collect (map-attribute k v))))
    (let* ((prim (elt (gltf:primitives gmesh) 0))
           (attrs (map-attributes prim))
           (idxs (map-attribute :INDICES (gltf:indices prim)))
           (geo (create-geometry (gltf:mode prim) attrs :static)))
      (geometry-set-indices geo idxs)
      (upload-geometry geo)
      (create-mesh
       :name (format nil "gltf-mesh~a" (gltf:idx gmesh))
       :geometry geo
       :material (material-from-gltf (gltf:material prim) rtextures)))))

(defun create-object-from-gltf (gnode rmeshes)
  (let ((children (loop
		    :for c :across (gltf:children gnode)
		    :collect (create-object-from-gltf c rmeshes)))
	(translation (if (vectorp (gltf:translation gnode))
			 (vector->vec3 (gltf:translation gnode))
			 (vec 0 0 0)))
	(scale (if (vectorp (gltf:scale gnode))
		   (vector->vec3 (gltf:scale gnode))
		   (vec 1 1 1))))
    (if (gltf:mesh gnode)
	(make-instance 'model
		       :mesh (nth (gltf:idx (gltf:mesh gnode)) rmeshes)
		       :name (gltf:name gnode)
		       :translation translation
		       :scale scale
		       :children children)
	(make-instance 'object
		       :name (gltf:name gnode)
		       :translation translation
		       :scale scale
		       :children children))))

(defun load-gltf (filename)
  (gltf:with-gltf (gltf filename)
    (let* ((rtextures (loop
			:for tex :across (gltf:textures gltf)
			:collect (load-texture-from-gltf filename tex)))
	   (rmeshes (loop
		      :for mesh :across (gltf:meshes gltf)
		      :collect (create-mesh-from-gltf mesh rtextures)))
	   (robjs (loop
		     :for node :across (gltf:nodes (elt (gltf:scenes gltf) 0))
		     :collect (create-object-from-gltf node rmeshes))))
      ;;(dolist (rnode rnodes) (describe rnode))
      (make-instance 'gltf-scene :name filename :children robjs))))
