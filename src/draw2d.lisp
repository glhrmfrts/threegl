(in-package :threegl)

(defconstant +tex2d-vertex-size+ 4)

(defparameter *tex2d-geo* nil)
(defparameter *tex2d-shader* nil)
(defparameter *fnt* nil)

(defparameter *current-tex* nil)
(defparameter *batch-maxverts* 4096)
(defparameter *batch-verts* nil)
(defparameter *batch-nverts* 0)

(defparameter *color2d-geo* nil)
(defparameter *color2d-shader* nil)

(defun init-draw2d (&key (texture-shader-path nil)
                         (color-shader-path nil)
                         (default-font-path nil))
  (let ((attrs (list
                (make-attribute :kind +vertex-attribute-position+
                                :component-type :float
                                :component-count 2
                                :usage :dynamic-draw)
                (make-attribute :kind +vertex-attribute-texcoord0+
                                :component-type :float
                                :component-count 2
                                :usage :dynamic-draw))))
    (setf *tex2d-geo* (create-geometry :triangles attrs :dynamic-draw))
    (setf *tex2d-shader* (load-shader texture-shader-path))
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
    (setf *color2d-shader* (load-shader color-shader-path))))

(defun begin-draw2d ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun draw-rect (rec color)
  (let ((verts (list
                  (vx2 (rect-mins rec)) (vy2 (rect-maxs rec))
                  (vx2 (rect-mins rec)) (vy2 (rect-mins rec))
                  (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec))
                  (vx2 (rect-maxs rec)) (vy2 (rect-mins rec))))
        (colors (list
                 1.0 1.0 1.0 1.0
                 1.0 1.0 1.0 1.0
                 1.0 1.0 1.0 1.0
                 1.0 1.0 1.0 1.0)))
    (set-attribute-items (find-attribute *color2d-geo* +vertex-attribute-position+)
                         :scalar
                         (coerce verts 'vector))
    (set-attribute-items (find-attribute *color2d-geo* +vertex-attribute-color+)
                         :scalar
                         (coerce colors 'vector))
    (set-shader *color2d-shader*)
    (set-color color)
    (upload-geometry *color2d-geo*)
    (draw-arrays *color2d-geo* :triangle-strip 0 4)))

(defun draw-line-loop (positions color)
  (let ((verts (alexandria:flatten (mapcar
                                    (lambda (p) (list (vx2 p) (vy2 p)))
                                    positions)))
        (colors (alexandria:flatten (mapcar
                                     (lambda (p) (list 1.0 1.0 1.0 1.0))
                                     positions))))
    (set-attribute-items (find-attribute *color2d-geo* +vertex-attribute-position+)
                         :scalar
                         (coerce verts 'vector))
    (set-attribute-items (find-attribute *color2d-geo* +vertex-attribute-color+)
                         :scalar
                         (coerce colors 'vector))
    (set-shader *color2d-shader*)
    (set-color color)
    (upload-geometry *color2d-geo*)
    (draw-arrays *color2d-geo* :line-loop 0 (length positions))))

(defun reset-batch (tex)
  (setf *current-tex* tex)
  (setf *batch-nverts* 0)
  (setf *batch-verts* (make-array (* *batch-maxverts* 2)
                                  :element-type :float
                                  :fill-pointer 0))
  (setf *batch-texcoords* (make-array (* *batch-maxverts* 2)
                                  :element-type :float
                                  :fill-pointer 0)))

(defun flush-draw2d ()
  (when (> *batch-nverts* 0)
    ;(format t "batch-nverts: ~a~%" *batch-nverts*)
    (set-shader *tex2d-shader*)
    (set-color (vec 1 1 1 1))
    (bind-texture *current-tex* :texture0)
    (set-attribute-items (find-attribute *tex2d-geo* +vertex-attribute-position+)
                         :scalar
                         (coerce *batch-verts* 'vector))
    (set-attribute-items (find-attribute *tex2d-geo* +vertex-attribute-texcoord0+)
                         :scalar
                         (coerce *batch-texcoords* 'vector))
    (upload-geometry *tex2d-geo*)
    (draw-arrays *tex2d-geo* :triangles 0 *batch-nverts*)
    (setf *batch-nverts* 0)
    (setf *current-tex* nil)))

(defun add-to-batch (rec tex-rec)
  (declare (type rect rec tex-rect))
  (labels
      ((add-vertex (px py tx ty)
         (loop for el in (list px py) do (vector-push el *batch-verts*))
         (loop for el in (list tx ty) do (vector-push el *batch-texcoords*))
         (incf *batch-nverts*)))

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

(defun draw-texture-rect (tex rec tex-rec color)
  (declare (type rect rec tex-rect))
  (declare (type vec4 color))
  (cond
    ((and (eq tex *current-tex*) (< (+ *batch-nverts* 6) *batch-maxverts*))
     (add-to-batch rec tex-rec))
    (t
     (flush-draw2d)
     (reset-batch tex)
     (add-to-batch rec tex-rec))))

(defun measure-text (text &key (out-size nil))
  (let ((cur-x 0)
        (cur-y 0)
	(max-x 0))

    (labels
      ((find-glyph-data (c)
        (aref (font-atlas-glyph-data (font-fnt *fnt*)) (char-code c)))

      (generate-glyph-vertices (c)
        (let ((gd (find-glyph-data c)))
          (incf cur-x (font-glyph-data-bearing-x gd))
          (if (equal c (char " " 0))
            (incf cur-x (font-glyph-data-adv-x gd))
	    (progn
              (incf cur-x (font-glyph-data-char-width gd))
	      (setq max-x (max max-x cur-x)))))))

      (with-input-from-string (input text)
        (loop :for line = (read-line input nil nil)
              :while line :do
          (setq cur-x 0)
          (loop :for c :across line do
            (generate-glyph-vertices c))
          (incf cur-y (font-atlas-new-line-height (font-fnt *fnt*)))))

      (when out-size
	(setf (vx2 out-size) max-x)
	(setf (vy2 out-size) cur-y)))))

(defun draw-text (text pos color &key (out-size nil))
  (declare (type string text))
  (declare (type vec2 pos))
  (declare (type vec4 color))

  (defconstant +vertex-per-char+ 6)

  (let* ((cur-x (vx2 pos))
         (cur-y (vy2 pos))
	 (max-x cur-x)
         (nverts (* (length text) +vertex-per-char+))
         (verts (make-array (* nverts 2) :element-type :float :fill-pointer 0))
         (texcoords (make-array (* nverts 2) :element-type :float :fill-pointer 0)))

    (labels
      ((find-glyph-data (c)
        (aref (font-atlas-glyph-data (font-fnt *fnt*)) (char-code c)))

      (add-vertex (px py tx ty)
        (loop for el in (list px py) do (vector-push el verts))
        (loop for el in (list tx ty) do (vector-push el texcoords)))

      (compute-glyph-y (gd)
        (+ (font-glyph-data-adv-y gd) (- cur-y (font-glyph-data-char-height gd))))

      (generate-glyph-vertices (c)
        (let ((gd (find-glyph-data c)))
          (incf cur-x (font-glyph-data-bearing-x gd))
          (if (equal c (char " " 0))
            (incf cur-x (font-glyph-data-adv-x gd))
            (let* ((rec (rect-from-size
                          (vec cur-x (compute-glyph-y gd))
                          (vec (font-glyph-data-char-width gd) (font-glyph-data-char-height gd))))
                   (tex-rec (make-rect
                              :mins (vec (font-glyph-data-rect-x gd) (+ (font-glyph-data-rect-height gd) (font-glyph-data-rect-y gd)))
                              :maxs (vec (+ (font-glyph-data-rect-width gd) (font-glyph-data-rect-x gd)) (font-glyph-data-rect-y gd)))))

              (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-mins rec)) (vx2 (rect-mins tex-rec)) (vy2 (rect-mins tex-rec)))
              (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-mins rec)) (vx2 (rect-maxs tex-rec)) (vy2 (rect-mins tex-rec)))
              (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec)) (vx2 (rect-maxs tex-rec)) (vy2 (rect-maxs tex-rec)))
              (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-mins rec)) (vx2 (rect-mins tex-rec)) (vy2 (rect-mins tex-rec)))
              (add-vertex (vx2 (rect-maxs rec)) (vy2 (rect-maxs rec)) (vx2 (rect-maxs tex-rec)) (vy2 (rect-maxs tex-rec)))
              (add-vertex (vx2 (rect-mins rec)) (vy2 (rect-maxs rec)) (vx2 (rect-mins tex-rec)) (vy2 (rect-maxs tex-rec)))

              (incf cur-x (font-glyph-data-char-width gd))
	      (setq max-x (max max-x cur-x)))))))

      (with-input-from-string (input text)
        (loop :for line = (read-line input nil nil)
              :while line :do
          (setq cur-x (vx2 pos))
          (loop :for c :across line do
            (generate-glyph-vertices c))
          (incf cur-y (font-atlas-new-line-height (font-fnt *fnt*))))))

    (set-shader *tex2d-shader*)
    (set-color color)
    (bind-texture (font-tex *fnt*) :texture0)
    (set-attribute-items (find-attribute *tex2d-geo* +vertex-attribute-position+)
                         :scalar
                         (coerce verts 'vector))
    (set-attribute-items (find-attribute *tex2d-geo* +vertex-attribute-texcoord0+)
                         :scalar
                         (coerce texcoords 'vector))
    (upload-geometry *tex2d-geo*)
    (draw-arrays *tex2d-geo* :triangles 0 nverts)

    (when out-size
      (setf (vx2 out-size) (- max-x (vx2 pos)))
      (setf (vy2 out-size) (- cur-y (vy2 pos))))))

(defun end-draw2d ()
  (gl:disable :blend))
