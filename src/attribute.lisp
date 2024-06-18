(in-package #:threegl)

(deftype buffer-usage () '(member :static-draw :dynamic-draw))

(defconstant +vertex-attribute-position+ 0)
(defconstant +vertex-attribute-normal+ 1)
(defconstant +vertex-attribute-color+ 2)
(defconstant +vertex-attribute-texcoord0+ 3)
(defconstant +vertex-attribute-texcoord1+ 4)

(defstruct attribute
  kind
  (usage :static-draw)
  (target :array-buffer)
  element-type
  component-type
  component-count
  divisor
  id
  needs-update
  items
  (n-items 0)
  (max-size 0)
  ever-uploaded)

(defun gl-data-type-size (data-type)
  (case data-type
    (:unsigned-byte 1)
    (:unsigned-short 2)
    (:float 4)
    (:unsigned-int 4)
    (otherwise nil)))

(defun element-type->component-count (elem-type)
  (ecase elem-type
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    (:vec4 4)))

(defun create-gl-array (data-type elem-type elems)
  (let ((arr (gl:alloc-gl-array
              data-type
              (* (length elems) (element-type->component-count elem-type)))))
    (ecase elem-type
      (:scalar
       (dotimes (i (length elems))
         (setf (gl:glaref arr i) (aref elems i))))
      (:vec2
       (dotimes (i (length elems))
         (dotimes (j 2)
           (setf (gl:glaref arr (+ j (* i 2))) (elt (aref elems i) j)))))
      (:vec3
       (dotimes (i (length elems))
         (dotimes (j 3)
           (setf (gl:glaref arr (+ j (* i 3))) (elt (aref elems i) j)))))
      (:vec4
       (dotimes (i (length elems))
         (dotimes (j 4)
           (setf (gl:glaref arr (+ j (* i 4))) (elt (aref elems i) j))))))
    arr))

(defun make-attribute-with-items (items &rest other-keys)
  (let ((attr (apply #'make-attribute other-keys)))
    (set-attribute-items attr (getf other-keys :element-type) items)
    attr))

(defun bind-attribute (attr)
  (gl:bind-buffer (attribute-target attr) (attribute-id attr)))

(defun setup-attribute (attr data-type n-items)
  (when (attribute-items attr)
    (gl:free-gl-array (attribute-items attr)))
  (setf (attribute-n-items attr) n-items)
  (setf (attribute-items attr) (gl:alloc-gl-array data-type n-items))
  (setf (attribute-needs-update attr) t))

(defun set-attribute-nth-item (attr n val)
  (setf (attribute-needs-update attr) t)
  (setf (gl:glaref (attribute-items attr)) val))

(defun set-attribute-items (attr elem-type items)
  "Set attribute items from a vector"
  (when (attribute-items attr)
    (gl:free-gl-array (attribute-items attr)))
  (setf (attribute-n-items attr) (* (length items) (element-type->component-count elem-type)))
  (setf (attribute-items attr)
        (create-gl-array (attribute-component-type attr) elem-type items))
  (setf (attribute-needs-update attr) t))

(defun destroy-attribute (attr)
  (gl:delete-buffers (list (attribute-id attr))))

(defun ensure-buffer-id (attr)
  (unless (attribute-id attr)
    (setf (attribute-id attr) (gl:gen-buffer))))

(defun enable-vertex-attribute (attr)
  (unless (eq (attribute-target attr) :element-array-buffer)
    (ensure-buffer-id attr)
    (gl:bind-buffer (attribute-target attr) (attribute-id attr))
    (gl:enable-vertex-attrib-array (attribute-kind attr))
    (gl:vertex-attrib-pointer (attribute-kind attr)
                              (attribute-component-count attr)
                              (attribute-component-type attr)
                              nil
                              0
                              (cffi:make-pointer 0))))

(defun upload-attribute (attr)
  (when (and (attribute-needs-update attr) (attribute-items attr))
    (ensure-buffer-id attr)
    (gl:bind-buffer (attribute-target attr) (attribute-id attr))
    (if (and (attribute-ever-uploaded attr)
             (<= (attribute-n-items attr) (attribute-max-size attr)))
        (gl:buffer-sub-data (attribute-target attr) (attribute-items attr))
        (gl:buffer-data (attribute-target attr)
                        (attribute-usage attr)
                        (attribute-items attr)))
    (setf (attribute-max-size attr) (max (attribute-n-items attr)
                                         (attribute-max-size attr)))
    (setf (attribute-needs-update attr) nil)
    (setf (attribute-ever-uploaded attr) t)))
