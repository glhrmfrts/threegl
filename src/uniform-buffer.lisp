(in-package #:threegl)

(defstruct uniform-buffer
  (id 0)
  (name "")
  (size 0)
  (binding-point 0)
  (usage :static))

(defparameter *ubo-list* nil)

(defparameter *binding-point-gen* 0)

(defun create-uniform-buffer (name size usage)
  (declare (type string name))
  (declare (type integer size))
  (declare (type buffer-usage usage))
  (let* ((ubo (gl:gen-buffer))
         (binding-point (- (incf *binding-point-gen*) 1)))
    (gl:bind-buffer :uniform-buffer ubo)
    (%gl:bind-buffer-base :uniform-buffer binding-point ubo)
    (let ((buffer (make-uniform-buffer :id ubo :name name :size size :binding-point binding-point :usage usage)))
      (setf *ubo-list* (cons buffer *ubo-list*))
      buffer)))

(defun upload-uniform (buf data)
  (gl:bind-buffer :uniform-buffer (uniform-buffer-id buf))
  (gl:buffer-data :uniform-buffer (uniform-buffer-usage buf) data))

(defun destroy-uniform-buffer (buf)
  (gl:delete-buffers (list (uniform-buffer-id buf)))
  (setf *ubo-list* (remove buf *ubo-list* :test #'equalp)))
