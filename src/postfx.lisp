(in-package #:threegl)

(defvar *screen-geometry* nil)
(defvar *testfbo* nil)
(defvar *testshader* nil)

(defparameter +testshader-source+
    "
#ifdef VERTEX_SHADER

in vec3  position;
in vec2  texcoord0;

smooth out vec2 passTexCoord;

void main() {
    gl_Position = vec4(position, 1.0);
    passTexCoord = texcoord0;
}

#else

uniform sampler2D tex0;

smooth in vec2 passTexCoord;

out vec4 outColor;

void main() {
	outColor = texture(tex0, passTexCoord);
}

#endif
")

(defun init-postfx ()
  (setf *testfbo* (create-framebuffer 1280 720 1 (list :depth)))
  (setf *testshader* (create-shader +testshader-source+ "testshader")))

(defun draw-screen ()
  (unless *screen-geometry*
    (setf *screen-geometry* (create-screen-geometry)))
  (draw-geometry *screen-geometry*))

(defun begin-postfx ()
  (bind-framebuffer *testfbo*))

(defun end-postfx ()
  (unbind-framebuffer)
  (clear :color-buffer)
  (set-shader *testshader*)
  (bind-framebuffer-texture *testfbo* 0 :texture0)
  (draw-screen))
