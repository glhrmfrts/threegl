(in-package :threegl)

(define-condition shader-compilation-error (error)
  ((filename :type string :initform "" :initarg :filename :accessor sce-filename)
   (message :type string :initform "" :initarg :message :accessor sce-message))
  (:report (lambda (condition stream)
    (format stream "error compiling shader '~s': ~s"
      (sce-filename condition)
      (sce-message condition)))))

(defstruct shader
  (program 0))

(defun process-shader-source (header source filename)
  (let ((string-list (list "#version 330" header source)))
    (format nil "~{~a~%~}" string-list)))

(defun compile-shader (sh source filename)
  (gl:shader-source sh source)
  (gl:compile-shader sh)
  (if (gl:get-shader sh :compile-status)
    t
    (progn
      (let ((msg (gl:get-shader-info-log sh)))
        (error (make-instance 'shader-compilation-error :filename filename :message msg))))))

(defun link-shader-program (program vert-shader frag-shader filename)
  (gl:attach-shader program vert-shader)
  (gl:attach-shader program frag-shader)

  (gl:bind-attrib-location program +vertex-attrib-position+ "position")
  (gl:bind-attrib-location program +vertex-attrib-normal+ "normal")
  (gl:bind-attrib-location program +vertex-attrib-color+ "color")
  (gl:bind-attrib-location program +vertex-attrib-texcoord+ "texcoord")
  (gl:bind-attrib-location program +vertex-attrib-lmtexcoord+ "lmtexcoord")

  (gl:link-program program)

  (if (not (gl:get-program program :link-status))
    (progn (format t "Could not link ~a: ~a" filename (gl:get-program-info-log program)) nil)
    (progn
      (gl:delete-shader vert-shader)
      (gl:delete-shader frag-shader)
      t)))

(defun create-shader (source filename)
  (let* ((program (gl:create-program))
         (vert-shader (gl:create-shader :vertex-shader))
         (frag-shader (gl:create-shader :fragment-shader))
         (vert-source (process-shader-source "#define VERTEX_SHADER" source filename))
         (frag-source (process-shader-source "#define FRAGMENT_SHADER" source filename))
         (compiled (every
            (lambda (pair) (compile-shader (first pair) (second pair) filename))
            (list (list vert-shader vert-source) (list frag-shader frag-source)))))
    (when compiled
      (progn
        (link-shader-program program vert-shader frag-shader filename)

        ;; Bind all uniform blocks
        (loop for ubo in *ubo-list*
         do (let* ((block-index (gl:get-uniform-block-index program (uniform-buffer-name ubo)))
                  (block-size (gl:get-active-uniform-block program block-index :uniform-block-data-size)))
            (when (/= block-size (uniform-buffer-size ubo))
              (log-warn "WARNING: OpenGL driver disagrees with us about UBO size of ~a" (uniform-buffer-name ubo)))
            (%gl:uniform-block-binding program block-index (uniform-buffer-binding-point ubo))))

        ;; Bind all texture units samplers
        (loop for i from 0 to (- +max-texture-units+ 1)
         do (let* ((sampler-name (format nil "tex~a" i))
                   (sampler-loc (gl:get-uniform-location program sampler-name)))
            (when (/= sampler-loc -1)
              (progn
                (gl:use-program program)
                (gl:uniformi sampler-loc i)))))

        (make-shader :program program)))))

(defun load-shader (filename)
  (with-open-file (file filename :direction :input :if-does-not-exist :error)
    (create-shader (format nil "~{~a~%~}"
                    (loop for line = (read-line file nil nil)
                      while line
                      collect line))
                  filename)))
