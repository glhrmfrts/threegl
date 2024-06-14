(in-package #:cl-user)

(defpackage #:threegl
  (:local-nicknames
   (:bs #:org.shirakumo.binary-structures)
   (:gltf #:org.shirakumo.fraf.gltf)
   (:flexi #:flexi-streams))
  (:use #:cl :org.shirakumo.binary-structures.types :3d-matrices :3d-vectors)
  (:export
   :init-render-state
   :destroy-render-state

   :set-clear-color
   :clear
   :set-projection-view
   :set-shader
   :set-color
   :set-transform
   :set-material
   :draw-arrays
   :draw-elements

   :make-attribute
   :find-attribute
   :create-geometry
   :create-cube-geometry
   :draw-geometry

   :rect-from-size
   :rect-mins
   :rect-maxs
   :rect-collides-p
   :point-in-rect-p

   :load-texture
   :find-texture
   :texture-width
   :texture-height

   :load-shader
   :basic-vertex-color-shader
   :basic-texture-shader

   :create-basic-vertex-color-material
   :create-basic-texture-material
   :load-material

   :make-mesh
   :render-mesh

   :node
   :translation
   :scale
   :children
   :model
   :camera
   :scene
   :traverse
   :render-model
   :render-scene

   :load-gltf

   :init-draw2d
   :begin-draw2d
   :draw-texture-rect
   :draw-rect
   :draw-line-loop
   :measure-text
   :draw-text
   :flush-draw2d
   :end-draw2d))

(in-package #:threegl)
