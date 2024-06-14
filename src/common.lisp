(in-package :threegl)

;; vec4 structure to send to GPU
(bs:define-io-structure vec4-s
   (x float32)
   (y float32)
   (z float32)
   (w float32))

(defstruct rect
  (mins (vec 0 0) :type vec2)
  (maxs (vec 0 0) :type vec2))

(defun rect-from-size (pos size)
  (make-rect :mins pos :maxs (v+ pos size)))

(defun rect-collides-p (rect1 rect2)
  "Check if RECT1 and RECT2 collide."
  (let ((rect1-mins (rect-mins rect1))
        (rect1-maxs (rect-maxs rect1))
        (rect2-mins (rect-mins rect2))
        (rect2-maxs (rect-maxs rect2)))
    (not (or (> (vx2 rect1-mins) (vx2 rect2-maxs))
             (< (vx2 rect1-maxs) (vx2 rect2-mins))
             (> (vy2 rect1-mins) (vy2 rect2-maxs))
             (< (vy2 rect1-maxs) (vy2 rect2-mins))))))

(defun point-in-rect-p (p rect)
  (let ((px  (vx2 p))
        (py  (vy2 p))
        (rx  (vx2 (rect-mins rect)))
        (ry  (vy2 (rect-mins rect)))
        (rx2 (vx2 (rect-maxs rect)))
        (ry2 (vy2 (rect-maxs rect))))
    (and (>= px rx)
         (<= px rx2)
         (>= py ry)
         (<= py ry2))))
