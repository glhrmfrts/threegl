(in-package #:threegl)

(defconstant +tex2d-vertex-size+ 4)

(defparameter *tex2d-geo* nil)
(defparameter *tex2d-shader* nil)
(defparameter *fnt* nil)

(deftype batch-kind () '(member :texture :color :none))

(defparameter *current-batch* :texture)
(defparameter *current-primitive* nil)
(defparameter *current-tex* nil)

(defparameter *prev-batch* :texture)
(defparameter *prev-primitive* nil)
(defparameter *prev-tex* nil)

(defparameter *batch-maxverts* (* 1024 8))
(defparameter *batch-verts* nil)
(defparameter *batch-texcoords* nil)
(defparameter *batch-colors* nil)
(defparameter *batch-nverts* 0)

(defparameter *color2d-geo* nil)
(defparameter *color2d-shader* nil)

(defun init-draw2d (&key (default-font-path nil))
  (let ((attrs (list
                (make-attribute :kind +vertex-attribute-position+
                                :component-type :float
                                :component-count 2
                                :usage :dynamic-draw)
                (make-attribute :kind +vertex-attribute-texcoord0+
                                :component-type :float
                                :component-count 2
                                :usage :dynamic-draw)
		(make-attribute :kind +vertex-attribute-color+
                                :component-type :float
                                :component-count 4
                                :usage :dynamic-draw))))
    (setf *tex2d-geo* (create-geometry :triangles attrs :dynamic-draw))
    (setf *tex2d-shader* (create-shader *tex2d-shader-source* "tex2d"))
    (setf *fnt* (load-font default-font-path)))

  (let ((attrs (list
                (make-attribute :kind +vertex-attribute-position+
                                :component-type :float
                                :component-count 2
                                :usage :dynamic-draw)
                (make-attribute :kind +vertex-attribute-color+
                                :component-type :float
                                :component-count 4
                                :usage :dynamic-draw))))
    (setf *color2d-geo* (create-geometry :triangles attrs :dynamic-draw))
    (setf *color2d-shader* (create-shader *color2d-shader-source* "color2d")))

  (setf *batch-verts* (make-array (* *batch-maxverts* 2)
                                  :element-type :float
                                  :fill-pointer 0))
  (setf *batch-colors* (make-array (* *batch-maxverts* 4)
                                   :element-type :float
                                   :fill-pointer 0))
  (setf *batch-texcoords* (make-array (* *batch-maxverts* 2)
                                      :element-type :float
                                      :fill-pointer 0)))

(defun begin-draw2d ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))
  ;;(print "begin-draw2d"))

(defun reset-batch (batch tex primitive)
  (setf *current-batch* batch)
  (setf *current-tex* tex)
  (setf *current-primitive* primitive)
  (setf *batch-nverts* 0)
  (setf (fill-pointer *batch-verts*) 0)
  (setf (fill-pointer *batch-colors*) 0)
  (setf (fill-pointer *batch-texcoords*) 0))

(defun flush-draw2d ()
  (setf *prev-batch* *current-batch*)
  (setf *prev-primitive* *current-primitive*)
  (setf *prev-tex* *current-tex*)
  (when (and *current-batch* *current-primitive* (> *batch-nverts* 0))
    (unless (and (eq *current-batch* :texture) (null *current-tex*))
      ;;(format t "batch ~a: ~a (~a)~%" *current-batch* *current-primitive* *batch-nverts*)
      (let ((geo (if (eq *current-batch* :texture) *tex2d-geo* *color2d-geo*)))
	(set-color (vec 1 1 1 1))
	(set-attribute-items (find-attribute geo +vertex-attribute-position+)
			     :scalar
			     (coerce *batch-verts* 'vector))
	(set-attribute-items (find-attribute geo +vertex-attribute-color+)
                             :scalar
                             (coerce *batch-colors* 'vector))
	(case *current-batch*
	  (:texture
	   (set-shader *tex2d-shader*)
	   (bind-texture *current-tex* :texture0)
	   (set-attribute-items (find-attribute geo +vertex-attribute-texcoord0+)
				:scalar
				(coerce *batch-texcoords* 'vector)))
	  (:color
	   (set-shader *color2d-shader*)))
	(upload-geometry geo)
	(draw-arrays geo *current-primitive* 0 *batch-nverts*)
	(setf *batch-nverts* 0)
	(setf *current-batch* nil)
	(setf *current-tex* nil)
	(setf *current-primitive* nil)))))

(defun set-batch (batch)
  (unless (eq batch *current-batch*)
    ;;(format t "flushing because of batch ~a~%" batch)
    (flush-draw2d)
    (reset-batch batch *prev-tex* *prev-primitive*)))

(defun set-primitive (prim)
  (unless (eq prim *current-primitive*)
    ;;(format t "flushing because of primitive ~a~%" prim)
    (flush-draw2d)
    (reset-batch *prev-batch* *prev-tex* prim)))

(defun set-texture (tex)
  (unless (eq tex *current-tex*)
    (flush-draw2d)
    (reset-batch *prev-batch* tex *prev-primitive*)))

(defun ensure-batch-capacity (nextra)
  (unless (< (+ *batch-nverts* nextra) *batch-maxverts*)
    (flush-draw2d)
    (reset-batch *prev-batch* *prev-tex* *prev-primitive*)))

(defun configure-batch (batch prim nverts tex)
  ;; @TODO: do everything inside this function, instead of having
  ;; separate functions and prev- variables
  (set-batch batch)
  (set-primitive prim)
  (set-texture tex)
  (ensure-batch-capacity nverts))

(defun add-vertex-to-texture-batch (px py tx ty cr cg cb ca)
  (vector-push px *batch-verts*)
  (vector-push py *batch-verts*)
  (vector-push tx *batch-texcoords*)
  (vector-push ty *batch-texcoords*)
  (vector-push cr *batch-colors*)
  (vector-push cg *batch-colors*)
  (vector-push cb *batch-colors*)
  (vector-push ca *batch-colors*)
  (incf *batch-nverts*))

(defun add-vertex-to-color-batch (px py cr cg cb ca)
  ;;(format t "add-vertex-to-color-batch ~a ~a~%" *current-batch* *current-primitive*)
  (vector-push px *batch-verts*)
  (vector-push py *batch-verts*)
  (vector-push cr *batch-colors*)
  (vector-push cg *batch-colors*)
  (vector-push cb *batch-colors*)
  (vector-push ca *batch-colors*)
  (incf *batch-nverts*))

(defun add-textured-quad-to-tex-batch (rec tex-rec)
  (declare (type rect rec tex-rect))
  (ensure-batch-capacity 6)
  (labels
      ((add-vertex (px py tx ty)
	 (add-vertex-to-texture-batch px py tx ty 1.0 1.0 1.0 1.0)))

    (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-mins rec))
                (vx2 (rect-mins tex-rec)) (vy2 (rect-mins tex-rec)))

    (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-mins rec))
                (vx2 (rect-maxs tex-rec)) (vy2 (rect-mins tex-rec)))

    (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec))
                (vx2 (rect-maxs tex-rec)) (vy2 (rect-maxs tex-rec)))

    (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-mins rec))
                (vx2 (rect-mins tex-rec)) (vy2 (rect-mins tex-rec)))

    (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec))
                (vx2 (rect-maxs tex-rec)) (vy2 (rect-maxs tex-rec)))

    (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-maxs rec))
                (vx2 (rect-mins tex-rec)) (vy2 (rect-maxs tex-rec)))))

(defun draw-rect (rec color)
  (configure-batch :color :triangles 6 nil)

  (add-vertex-to-color-batch (vx2 (rect-mins rec)) (vy2 (rect-mins rec))
			     (vx color) (vy color) (vz color) (vw color))

  (add-vertex-to-color-batch (vx2 (rect-maxs rec)) (vy2 (rect-mins rec))
			     (vx color) (vy color) (vz color) (vw color))

  (add-vertex-to-color-batch (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec))
			     (vx color) (vy color) (vz color) (vw color))

  (add-vertex-to-color-batch (vx2 (rect-mins rec)) (vy2 (rect-mins rec))
			     (vx color) (vy color) (vz color) (vw color))

  (add-vertex-to-color-batch (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec))
			     (vx color) (vy color) (vz color) (vw color))

  (add-vertex-to-color-batch (vx2 (rect-mins rec)) (vy2 (rect-maxs rec))
			     (vx color) (vy color) (vz color) (vw color))

  )

(defun draw-lines (positions color)
  (configure-batch :color :lines 1 nil)
  (loop :for i :from 0 :below (length positions) :do
    (let ((p1 (nth i positions)))
      (add-vertex-to-color-batch (vx2 p1) (vy2 p1) (vx color) (vy color) (vz color) (vw color))
      )))

(defun draw-line-loop (positions color)
  (configure-batch :color :lines (* 2 (length positions)) nil)
  (loop :for i :from 1 :below (length positions) :do
    (let ((p1 (nth (- i 1) positions))
	  (p2 (nth (mod i (length positions)) positions)))
      (add-vertex-to-color-batch (vx2 p1) (vy2 p1) (vx color) (vy color) (vz color) (vw color))
      (add-vertex-to-color-batch (vx2 p2) (vy2 p2) (vx color) (vy color) (vz color) (vw color))
      )))

(defun draw-texture-rect (tex rec tex-rec color)
  (declare (type rect rec tex-rect))
  (declare (type vec4 color))
  (configure-batch :texture :triangles 6 tex)
  (add-textured-quad-to-tex-batch rec tex-rec))

(defun measure-text (text &key (end nil) (out-size nil))
  (let ((cur-x 0.0)
        (cur-y 0.0)
	(max-x 0.0)
	(idx 0))

    (labels
      ((find-glyph-data (c)
        (aref (font-atlas-glyph-data (font-fnt *fnt*)) (char-code c)))

      (generate-glyph-vertices (c)
        (let ((gd (find-glyph-data c)))
          (incf cur-x (font-glyph-data-bearing-x gd))
          (if (equal c (char " " 0))
            (incf cur-x (font-glyph-data-adv-x gd))
	    (progn
              (incf cur-x (font-glyph-data-char-width gd))))
	  (setq max-x (max max-x cur-x)))))

      (with-input-from-string (input text)
        (loop :for line = (read-line input nil nil) :while line :do
          (setq cur-x 0.0)
          (loop :for c :across line :while (or (not end) (< idx end)) :do
            (generate-glyph-vertices c)
	    (incf idx))
          (incf cur-y (font-atlas-new-line-height (font-fnt *fnt*)))))

      (when out-size
	;;(when end (format t "~a (~a) => ~a~%" text end max-x))
	(setf (vx2 out-size) max-x)
	(setf (vy2 out-size) cur-y)))))

(defconstant +vertex-per-char+ 6)
(defun draw-text (text pos color &key (out-size nil))
  (declare (type string text))
  (declare (type vec2 pos))
  (declare (type vec4 color))

  (unless (zerop (length text))
    (let* ((cur-x (vx2 pos))
           (cur-y (vy2 pos))
	   (max-x cur-x)
           (nverts (* (length text) +vertex-per-char+)))

      (configure-batch :texture :triangles nverts (font-tex *fnt*))

      (setf nverts 0)

      (labels
	  ((find-glyph-data (c)
             (aref (font-atlas-glyph-data (font-fnt *fnt*)) (char-code c)))

	   (compute-glyph-y (gd)
             (+ (font-glyph-data-adv-y gd) (- cur-y (font-glyph-data-char-height gd))))

	   (generate-glyph-vertices (c)
             (let ((gd (find-glyph-data c)))
               (incf cur-x (font-glyph-data-bearing-x gd))
               (if (equal c (char " " 0))
		   (incf cur-x (font-glyph-data-adv-x gd))
		   (let* ((rec (rect-from-size
				(vec cur-x (compute-glyph-y gd))
				(vec (font-glyph-data-char-width gd)
				     (font-glyph-data-char-height gd))))
			  (tex-rec (make-rect
				    :mins (vec
					   (font-glyph-data-rect-x gd)
					   (+ (font-glyph-data-rect-height gd)
					      (font-glyph-data-rect-y gd)))
				    :maxs (vec
					   (+ (font-glyph-data-rect-width gd)
					      (font-glyph-data-rect-x gd))
					   (font-glyph-data-rect-y gd)))))

		     (add-vertex-to-texture-batch (vx2 (rect-mins rec))
					      (vy2 (rect-mins rec))
					      (vx2 (rect-mins tex-rec))
					      (vy2 (rect-mins tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (add-vertex-to-texture-batch (vx2 (rect-maxs rec))
					      (vy2 (rect-mins rec))
					      (vx2 (rect-maxs tex-rec))
					      (vy2 (rect-mins tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (add-vertex-to-texture-batch (vx2 (rect-maxs rec))
					      (vy2 (rect-maxs rec))
					      (vx2 (rect-maxs tex-rec))
					      (vy2 (rect-maxs tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (add-vertex-to-texture-batch (vx2 (rect-mins rec))
					      (vy2 (rect-mins rec))
					      (vx2 (rect-mins tex-rec))
					      (vy2 (rect-mins tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (add-vertex-to-texture-batch (vx2 (rect-maxs rec))
					      (vy2 (rect-maxs rec))
					      (vx2 (rect-maxs tex-rec))
					      (vy2 (rect-maxs tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (add-vertex-to-texture-batch (vx2 (rect-mins rec))
					      (vy2 (rect-maxs rec))
					      (vx2 (rect-mins tex-rec))
					      (vy2 (rect-maxs tex-rec))
					      (vx color) (vy color) (vz color) (vw color))

		     (incf nverts 6)

		     (incf cur-x (font-glyph-data-char-width gd))
		     (setq max-x (max max-x cur-x)))))))

	(with-input-from-string (input text)
          (loop :for line = (read-line input nil nil) :while line :do
            (setq cur-x (vx2 pos))
            (loop :for c :across line :do
              (generate-glyph-vertices c))
            (incf cur-y (font-atlas-new-line-height (font-fnt *fnt*))))))

      (when out-size
	(setf (vx2 out-size) (- max-x (vx2 pos)))
	(setf (vy2 out-size) (- cur-y (vy2 pos)))))))

(defun end-draw2d ()
  (gl:disable :blend))
