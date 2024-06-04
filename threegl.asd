#|
 This file is a part of threegl
 (c) 2024 guilherme (guilherme.nemeth@gmail.com)
 Author: guilherme <guilherme.nemeth@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem threegl
  :version "0.0.0"
  :license "BSD-3"
  :author "guilherme <guilherme.nemeth@gmail.com>"
  :maintainer "guilherme <guilherme.nemeth@gmail.com>"
  :description "Graphics library"
  :serial T
  :pathname "src"
  :components ((:file "package")             
               (:file "common")
               (:file "draw2d")
               (:file "font")
               (:file "geometry-buffer")
               (:file "material")
               (:file "model-mesh")
               (:file "shader")
               (:file "render-state")
               (:file "texture")
               (:file "uniform-buffer"))
  :depends-on ("3d-vectors"
               "3d-matrices"
               "alexandria"
               "binary-structures"
               "flexi-streams"
               "cl-opengl"
               "cl-json"
               "cl-bmp"
               "png-read"))
