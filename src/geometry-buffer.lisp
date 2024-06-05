(in-package :threegl)

(deftype index-format () '(member :uint16 :uint32))

(deftype buffer-usage () '(member :static-draw :dynamic-draw))

(defconstant +vertex-attrib-position+ 0)
(defconstant +vertex-attrib-normal+ 1)
(defconstant +vertex-attrib-color+ 2)
(defconstant +vertex-attrib-texcoord+ 3)
(defconstant +vertex-attrib-lmtexcoord+ 4)

(defstruct vertex-attribute
  (attrib 0)
  (size 0)
  (data-type nil)
  (stride 0)
  (offset 0))

(defstruct geometry-buffer
  (vao 0)
  (vbo 0)
  (ebo 0)
  (attrs nil)
  (usage :static)
  (vertex-size 0)
  (index-size 0))

(defun create-gl-array (data-type elems)
  (let ((arr (gl:alloc-gl-array data-type (length elems))))
    (dotimes (i (length elems))
      (setf (gl:glaref arr i) (aref elems i)))
    arr))

(defun gl-data-type-size (data-type)
  (case data-type
    (:unsigned-byte 1)
    (:float 4)
    (:unsigned-int 4)
    (otherwise nil)))

(defun make-position2d-vertex-attribute ()
  (make-vertex-attribute
    :attrib +vertex-attrib-position+
    :size 2
    :data-type :float))

(defun make-position3d-vertex-attribute ()
  (make-vertex-attribute
    :attrib +vertex-attrib-position+
    :size 3
    :data-type :float))

(defun make-normal-vertex-attribute ()
  (make-vertex-attribute
    :attrib +vertex-attrib-normal+
    :size 3
    :data-type :float))

(defun make-color-vertex-attribute ()
  (make-vertex-attribute
    :attrib +vertex-attrib-color+
    :size 4
    :data-type :float))

(defun make-texcoord-vertex-attribute ()
  (make-vertex-attribute
    :attrib +vertex-attrib-texcoord+
    :size 2
    :data-type :float))

(defun enable-vertex-attribute (attr)
  (declare (type vertex-attribute attr))
  (gl:enable-vertex-attrib-array (vertex-attribute-attrib attr))
  (gl:vertex-attrib-pointer
    (vertex-attribute-attrib attr)
    (vertex-attribute-size attr)
    (vertex-attribute-data-type attr)
    nil
    (vertex-attribute-stride attr)
    (cffi:make-pointer (vertex-attribute-offset attr))))

(defun compute-vertex-attributes-stride-and-offset (attrs)
  (let* ((stride 0)
         (nattrs (mapcar
            (lambda (attr)
              (let* ((nattr (make-vertex-attribute
                              :offset stride
                              :attrib (vertex-attribute-attrib attr)
                              :size (vertex-attribute-size attr)
                              :data-type (vertex-attribute-data-type attr))))
                (setq stride (+ stride (* (vertex-attribute-size attr) (gl-data-type-size (vertex-attribute-data-type attr)))))
                nattr))
            attrs))
          (nnattrs (mapcar
            (lambda (attr)
              (make-vertex-attribute
                            :stride stride
                            :offset (vertex-attribute-offset attr)
                            :attrib (vertex-attribute-attrib attr)
                            :size (vertex-attribute-size attr)
                            :data-type (vertex-attribute-data-type attr)))
            nattrs)))
    nnattrs))

(defun vertex-attribute-byte-size (attr)
  (*
    (vertex-attribute-size attr)
    (gl-data-type-size (vertex-attribute-data-type attr))))

(defun compute-vertex-byte-size (attrs)
  (reduce
    (lambda (attr acc)
      (+ acc (vertex-attribute-byte-size attr))) 
    attrs))

(defun create-geometry-buffer (attrs idx-format usage)
  (declare (type idx-format index-format))
  (declare (type usage buffer-usage))
  (let* ((nattrs (compute-vertex-attributes-stride-and-offset attrs))
         (vao (gl:gen-vertex-array))
         (vbo (gl:gen-buffer))
         (ebo (if idx-format (gl:gen-buffer) nil)))
    ;(format t "~a~%" nattrs)
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo) 
    (loop for attr in nattrs do (enable-vertex-attribute attr))
    (make-geometry-buffer :vao vao :vbo vbo :ebo ebo :attrs nattrs :usage usage :vertex-size 0 :index-size 0)))

(defun upload-geometry (buf &key (vertices nil) (indices nil))
  (when vertices
    (progn
      (gl:bind-buffer :array-buffer (geometry-buffer-vbo buf))
      (gl:buffer-data :array-buffer (geometry-buffer-usage buf) vertices)))
  (when indices
    (progn
      (gl:bind-buffer :element-array-buffer (geometry-buffer-ebo buf))
      (gl:buffer-data :element-array-buffer (geometry-buffer-usage buf) indices))))

(defun draw-geometry (buf count)
  (gl:bind-vertex-array (geometry-buffer-vao buf))
  (gl:draw-arrays :triangles 0 count))
