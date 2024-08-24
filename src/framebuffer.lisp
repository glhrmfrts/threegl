(in-package #:threegl)

(deftype framebuffer-feature () '(member :multisampled :filtered :hdr :depth :shadow-map))

(defstruct framebuffer
  id
  width
  height
  target
  filter
  fmt
  features
  color-textures
  depth-texture)

(defparameter *color-attachments*
  '(:color-attachment0
    :color-attachment1
    :color-attachment2
    :color-attachment3
    :color-attachment4
    :color-attachment5))

(defun create-framebuffer-texture (target width height filter fmt shadowmap-p)
  (let ((id (gl:gen-texture)))
    (gl:active-texture :texture0)
    (gl:bind-texture target id)
    (gl:tex-parameter target :texture-mag-filter filter)
    (gl:tex-parameter target :texture-min-filter filter)
    (gl:tex-parameter target :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter target :texture-wrap-t :clamp-to-edge)
    (when shadowmap-p
      (gl:tex-parameter target :texture-compare-mode :compare-ref-to-texture))
    (if (eq target :texture-2d-multisample)
	(%gl:tex-image-2d-multisample target 2 fmt width height nil)
	(gl:tex-image-2d target 0 fmt width height 0 (if (eq fmt :depth-component32f) :depth-component :bgra) :unsigned-byte nil))
    (make-texture :id id)))

(defun create-framebuffer (width height num-color-textures features)
  (let* ((id (gl:gen-framebuffer))
	 (target (if (find :multisampled features) :texture-2d-multisample :texture-2d))
	 (filter (if (find :filtered features) :linear :nearest))
	 (fmt (if (find :hdr features) :rgba16f :rgba8))
	 (fbo (make-framebuffer :id id
				:width width
				:height height
				:target target
				:filter filter
				:fmt fmt
				:features features)))

    (gl:bind-framebuffer :framebuffer id)

    (loop :for i :from 0 :below num-color-textures :do
      (let ((tex (create-framebuffer-texture target width height filter fmt nil)))
	(push tex (framebuffer-color-textures fbo))
	(gl:framebuffer-texture-2d :framebuffer
				   (nth i *color-attachments*)
				   target
				   (texture-id tex)
				   0)))

    (setf (framebuffer-color-textures fbo) (reverse (framebuffer-color-textures fbo)))
    (gl:draw-buffers *color-attachments*)

    (when (find :depth features)
      (setf (framebuffer-depth-texture fbo)
	    (create-framebuffer-texture target width height filter :depth-component32f
					(find :shadow-map features)))
      (gl:framebuffer-texture-2d :framebuffer
				   :depth-attachment
				   target
				   (texture-id (framebuffer-depth-texture fbo))
				   0))

    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (when (not (eq status :framebuffer-complete-oes))
	(error (format nil "framebuffer not complete, status: ~a" status)))

      (gl:bind-framebuffer :framebuffer 0)
      fbo)))

(defun destroy-framebuffer (fbo)
  (when (framebuffer-id fbo)
    (gl:delete-framebuffers (list (framebuffer-id fbo)))
    (gl:delete-textures (framebuffer-color-textures fbo))
    (when (framebuffer-depth-texture fbo)
      (gl:delete-textures (list (framebuffer-depth-texture fbo))))))

(defparameter *bound-fbo* nil)

(defun unbind-framebuffer ()
  (gl:bind-framebuffer :framebuffer 0)
  (gl:viewport 0 0 *view-width* *view-height*)
  (setf *bound-fbo* nil))

(defun bind-framebuffer (fbo)
  (unless (eq fbo *bound-fbo*)
    (gl:bind-framebuffer :framebuffer (framebuffer-id fbo))
    (gl:viewport 0 0 (framebuffer-width fbo) (framebuffer-height fbo))
    (setf *bound-fbo* fbo)))

(defun bind-framebuffer-texture (fbo idx unit)
  (gl:active-texture unit)
  (gl:bind-texture
   (if (find :multisampled (framebuffer-features fbo))
       :texture-2d-multisample
       :texture-2d)
   (texture-id (nth idx (framebuffer-color-textures fbo)))))

(defun bind-framebuffer-depth-texture (fbo unit)
  (gl:active-texture unit)
  (gl:bind-texture
   (if (find :multisampled (framebuffer-features fbo))
       :texture-2d-multisample
       :texture-2d)
   (texture-id (framebuffer-depth-texture fbo))))
