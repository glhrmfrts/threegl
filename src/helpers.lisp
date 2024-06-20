(in-package #:threegl)

(defclass grid-helper (object)
  ((grid-width :initarg :grid-width :initform 10 :accessor grid-width)
   (grid-height :initarg :grid-height :initform 10 :accessor grid-height)
   (cell-size :initarg :cell-size :initform 1 :accessor cell-size)
   (mesh :accessor grid-mesh)))

(defun generate-grid-data (grid-width grid-height cell-size)
  (let ((vertices
	  (append
	   (loop :for x :from 0 :below (+ 1 grid-width)
		 :append (list (vector (* cell-size (float x)) 0.0 0.0)
			       (vector (* cell-size (float x)) 0.0 (* cell-size (float grid-height)))))
	   (loop :for z :from 0 :below (+ 1 grid-height)
		 :append (list (vector 0.0 0.0 (* cell-size (float z)))
			       (vector (* cell-size (float grid-width)) 0.0 (* cell-size (float z)))))))
	(colors (loop :for i :from 0 :below (+ (* (+ 1 grid-width) 2) (* (+ 1 grid-height) 2))
		      :collect #(1.0 1.0 1.0 1.0))))
    (values vertices colors)))

(defun generate-grid-geometry (grid-width grid-height cell-size)
  (multiple-value-bind (vertices colors) (generate-grid-data grid-width grid-height cell-size)
    (let* ((vert-attr (make-attribute-with-items (coerce vertices 'vector)
                                                 :kind +vertex-attribute-position+
                                                 :element-type :vec3
                                                 :component-type :float
                                                 :component-count 3))
           (color-attr (make-attribute-with-items (coerce colors 'vector)
                                                  :kind +vertex-attribute-color+
                                                  :element-type :vec4
                                                  :component-type :float
                                                  :component-count 4))
	   (geo (create-geometry :lines (list vert-attr color-attr) :static)))
      (upload-geometry geo)
      geo)))

(defun create-grid-mesh (g)
  (setf (grid-mesh g)
	(make-mesh :geometry (generate-grid-geometry (float (grid-width g)) (float (grid-height g)) (float (cell-size g)))
		   :material (create-basic-vertex-color-material))))

(defmethod render-object ((g grid-helper))
  (unless (slot-boundp g 'mesh)
    (create-grid-mesh g))
  (set-transform (world-transform g))
  (render-mesh (grid-mesh g)))

(defclass orbit-control-helper (object)
  ((mouse-x :initform 0.0 :accessor och-mouse-x)
   (mouse-y :initform 0.0 :accessor och-mouse-y)
   (mouse-move-x :initform 0.0 :accessor och-mouse-move-x)
   (mouse-move-y :initform 0.0 :accessor och-mouse-move-y)
   (yaw :initform 0.0 :accessor och-yaw)
   (pitch :initform 0.0 :accessor och-pitch)
   (rotate-pressed :initform nil :accessor rotate-pressed)
   (pan-pressed :initform nil :accessor pan-pressed)
   (move-right-pressed :initform 0.0 :accessor move-right-pressed)
   (move-left-pressed :initform 0.0 :accessor move-left-pressed)
   (move-front-pressed :initform 0.0 :accessor move-front-pressed)
   (move-back-pressed :initform 0.0 :accessor move-back-pressed)
   (move-x :initform 0.0 :accessor och-move-x)
   (move-y :initform 0.0 :accessor och-move-y)
   (look-at :initform nil :accessor och-look-at)))

(defun orbit-control-key-changed (ctrl key action modifiers)
  (cond
    ((eq key :d)
     (setf (move-right-pressed ctrl) (if (or (eq action :press) (eq action :repeat)) 1.0 0.0)))
    ((eq key :a)
     (setf (move-left-pressed ctrl) (if (or (eq action :press) (eq action :repeat)) 1.0 0.0)))
    ((eq key :w)
     (setf (move-front-pressed ctrl) (if (or (eq action :press) (eq action :repeat)) 1.0 0.0)))
    ((eq key :s)
     (setf (move-back-pressed ctrl) (if (or (eq action :press) (eq action :repeat)) 1.0 0.0)))))

(defun orbit-control-mouse-moved (ctrl mx my)
  (setf (och-mouse-move-x ctrl) (- mx (och-mouse-x ctrl)))
  (setf (och-mouse-move-y ctrl) (- my (och-mouse-y ctrl)))
  (setf (och-mouse-x ctrl) mx)
  (setf (och-mouse-y ctrl) my))

(defun orbit-control-mouse-button-changed (ctrl btn action modifiers)
  (setf (rotate-pressed ctrl) (and (eq btn :left) (eq action :press)))
  (setf (pan-pressed ctrl) (and (eq btn :right) (eq action :press)))
  )

(defun orbit-control-look-at (c pos)
  (setf (och-look-at c) pos))

(defun orbit-control-update (c dt)
  (defconstant +speed+ 0.1)

  (when (rotate-pressed c)
    (incf (och-yaw c) (* (- (och-mouse-move-x c)) +speed+ dt))
    (incf (och-pitch c) (* (- (och-mouse-move-y c)) +speed+ dt)))

  (setf (och-pitch c) (max -1.0 (min (och-pitch c) 1.0)))

  (let ((right (world-right-vector c))
	(front (world-front-vector c))
	(up (vec 0.0 1.0 0.0)))

    ;;(setf (rotation c) (q* (rotation c) (qfrom-angle up (och-yaw c))))
    ;;(setf (rotation c) (q* (rotation c) (qfrom-angle right (och-pitch c))))

    ;; @TODO: fix whatever the fuck is going on with qlookat
    (if (och-look-at c)
        (let* ((dir (vunit (v- (translation c) (och-look-at c)))))
	  (setf (rotation c) (quat))
	  )
	(setf (rotation c) (q* (qfrom-angle up (och-yaw c)) (qfrom-angle right (och-pitch c)))))

    (setf (och-move-x c)
	  (* (+ (* 1.0 (move-right-pressed c)) (* -1.0 (move-left-pressed c))) dt 50.0))
    (setf (och-move-y c)
	  (* (+ (* 1.0 (move-front-pressed c)) (* -1.0 (move-back-pressed c))) dt 50.0))

    (when (pan-pressed c)
      (setf (och-move-x c) (* -0.05 (och-mouse-move-x c)))
      (setf (och-move-y c) (* -0.05 (och-mouse-move-y c))))

    (setf (translation c) (v+ (translation c) (v* right (och-move-x c))))
    (setf (translation c) (v+ (translation c) (v* front (och-move-y c))))
    )

  (setf (och-mouse-move-x c) 0.0)
  (setf (och-mouse-move-y c) 0.0)

  ;;(print (translation c))
  ;;(print (world-translation c))

  (add-flag-recur c +object-flag-transform-dirty+)

  )
