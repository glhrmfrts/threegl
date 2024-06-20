(in-package #:cl-user)

(defpackage #:threegl
  (:local-nicknames
   (:bs #:org.shirakumo.binary-structures)
   (:gltf #:org.shirakumo.fraf.gltf)
   (:flexi #:flexi-streams))
  (:use
   #:cl
   #:org.shirakumo.binary-structures.types
   #:org.shirakumo.fraf.math.matrices
   #:org.shirakumo.fraf.math.vectors
   #:org.shirakumo.fraf.math.quaternions)
  (:export
   :init-render-state
   :destroy-render-state

   :stats
   :*stats*
   :stats-draw-calls
   :stats-vertices

   :begin-frame
   :end-frame
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

   :create-mesh
   :render-mesh
   :create-mesh-instance

   :create-basic-vertex-color-material
   :create-basic-texture-material
   :load-material

   :load-gltf

   :+object-flag-static+
   :+object-flag-transform-dirty+
   :object
   :translation
   :world-translation
   :rotation
   :world-rotation
   :transform
   :world-transform
   :scale
   :world-front-vector
   :world-right-vector
   :world-up-vector
   :add-flag
   :has-flag-p
   :remove-flag
   :add-flag-recur
   :children
   :model
   :camera
   :scene
   :add-child
   :traverse
   :link
   :render-model
   :render-scene

   :grid-helper

   :orbit-control-helper
   :orbit-control-mouse-moved
   :orbit-control-key-changed
   :orbit-control-mouse-button-changed
   :orbit-control-look-at
   :orbit-control-update

   :triangle-mesh-builder
   :triangle-mesh-n-elements
   :triangle-mesh-n-indices
   :triangle-mesh-indices
   :triangle-mesh-vertices
   :triangle-mesh-normals
   :triangle-mesh-colors
   :triangle-mesh-uvs
   :triangle-mesh-begin
   :triangle-mesh-end
   :triangle-mesh-add-vertex
   :triangle-mesh-add-color
   :triangle-mesh-add-index
   :triangle-mesh-add-quad-vc
   :triangle-mesh-add-cube-vc
   :triangle-mesh->geometry

   :init-draw2d
   :begin-draw2d
   :draw-texture-rect
   :draw-rect
   :draw-line-loop
   :measure-text
   :draw-text
   :flush-draw2d
   :end-draw2d

   :begin-postfx
   :end-postfx))

(in-package #:threegl)
