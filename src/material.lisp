(in-package :threegl)

(deftype render-mode () '(member :opaque :alpha :cutout))

(define-condition invalid-render-mode (error)
  ((filename :type string :initform "" :accessor irm-filename)
   (value :type string :initform "" :accessor irm-value))
  (:report (lambda (condition stream)
    (format stream "error parsing material '~s': '~s' is not a valid render mode"
      (irm-filename condition)
      (irm-value condition)))))

(defstruct material
  (mode :opaque :type render-mode)
  (color (vec 1 1 1 1) :type vec4)
  (shader nil)
  (texture nil))

(defun create-basic-vertex-color-material (&rest other-keys)
  (let ((args (append other-keys (list :shader (threegl:basic-vertex-color-shader)))))
    (apply #'make-material args)))

(defun create-basic-texture-material (&rest other-keys)
  (let ((args (append other-keys (list :shader (threegl:basic-texture-shader)))))
    (apply #'make-material args)))

(defun load-material (filename)
  (with-open-file (file filename :direction :input)
    (labels
      ((parse-material-option (pair)
        (destructuring-bind (key . val) pair
          (list key (case key
            (:mode
              (let ((msym (intern (string-upcase val) "KEYWORD")))
                (or
                  (and (typep msym 'render-mode) msym)
                  (error 'invalid-render-mode :filename filename :value val))))
            (:color
              (let ((ival (parse-integer (subseq val 1) :radix 16)))
                (vec
                  (logand (ash ival -24) #xff)
                  (logand (ash ival -16) #xff)
                  (logand (ash ival -8) #xff)
                  (logand ival #xff))))
            (:shader (load-shader (make-pathname :name val)))
            (:texture (load-texture (make-pathname :name val)))))))
       (parse-material (value)
         (format t "material: ~a~%" value)
         (let ((mat-opts (mapcar #'parse-material-option value)))
          (apply #'make-material (alexandria:flatten mat-opts)))))
      (format t "material: ~a~%" (parse-material (json:decode-json file))))))
