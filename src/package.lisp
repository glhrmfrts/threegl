(in-package #:cl-user)

(defpackage #:threegl
  (:local-nicknames
   (:bs #:org.shirakumo.binary-structures)
   (:flexi #:flexi-streams))
  (:use #:cl :org.shirakumo.binary-structures.types :3d-matrices :3d-vectors)
  (:export
   :init-render-state

   :set-projection-view
   :set-shader
   :set-color
   :draw-arrays

   :rect-from-size
   :rect-mins
   :rect-maxs
   :rect-collides-p

   :load-texture
   :find-texture
   :texture-width
   :texture-height

   :load-shader

   :load-mdl2
   
   :init-draw2d
   :begin-draw2d
   :draw-texture-rect
   :draw-rect
   :draw-text
   :flush-draw2d
   :end-draw2d))

(in-package #:threegl)
