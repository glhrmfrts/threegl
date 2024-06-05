(in-package :threegl)

(deftype vertex-buffer-usage () '(member :static :dynamic))

(defconstant +vertex-attrib-position+ 0)
(defconstant +vertex-attrib-normal+ 1)
(defconstant +vertex-attrib-color+ 2)
(defconstant +vertex-attrib-texcoord+ 3)
(defconstant +vertex-attrib-lmtexcoord+ 4)

(defstruct vertex-buffer
  (type :position)
  (vbo 0)
  (usage :static)
  (component-count 0)
  (component-type :float)
  (vertex-size 0)
  (items nil)
  (needs-update-p nil))

(defun vertex-buffer-type-to-position (type)
  (ecase type
    (:position +vertex-attrib-position+)
    (:normal +vertex-attrib-normal+)
    (:color +vertex-attrib-color+)
    (:texcoord +vertex-attrib-texcoord+)
    (:lmtexcoord +vertex-attrib-lmtexcoord+)))

(defun destroy-vertex-buffer (buf)
  (with-slots (vbo) buf
    (gl:delete-buffers (list vbo))))

(defun enable-vertex-attribute (buf)
  (with-slots (vbo type component-count component-type) buf
    (gl:bind-buffer :array-buffer vbo)
    (gl:enable-vertex-attrib-array (vertex-buffer-type-to-position type))
    (gl:vertex-attrib-pointer (vertex-buffer-type-to-position type)
                              component-count
                              component-type
                              nil
                              0
                              (cffi:make-pointer 0))))

(defun resize-vertex-buffer (buf len)
  (setf (vertex-buffer-items buf) (gl:alloc-gl-array data-type len))
  (setf (vertex-buffer-needs-update-p buf) t))

(defun upload-vertex-buffer (buf)
  (when (vertex-buffer-needs-update-p buf)
    (unless (vertex-buffer-vbo buf)
      (setf (vertex-buffer-vbo buf) (gl:gen-buffer)))
    (gl:bind-buffer :array-buffer (vertex-buffer-vbo buf))
    (gl:buffer-data :array-buffer (vertex-buffer-usage buf) (vertex-buffer-items buf))))
