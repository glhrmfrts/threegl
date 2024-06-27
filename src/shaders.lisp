(in-package #:threegl)

(defparameter *tex2d-shader-source* "
#ifdef VERTEX_SHADER

in vec2  position;
in vec2  texcoord0;
in vec4  color;

smooth out vec2 passTexCoord;
smooth out vec4 passColor;

void main() {
    gl_Position = projview * vec4(position, 0.0, 1.0);
    passTexCoord = texcoord0;
    passColor = color;
}

#else

uniform sampler2D tex0;

smooth in vec2 passTexCoord;
smooth in vec4 passColor;

out vec4 outColor;

void main() {
	outColor = texture(tex0, passTexCoord) * passColor * ucolor;
}

#endif
")

(defparameter *color2d-shader-source* "
#ifdef VERTEX_SHADER

in vec2  position;
in vec4  color;

smooth out vec4 passColor;

void main() {
    gl_Position = projview * vec4(position, 0.0, 1.0);
    passColor = color;
}

#else

smooth in vec4 passColor;

out vec4 outColor;

void main() {
	outColor = passColor * ucolor;
}

#endif
")

(defparameter *basic-vertex-color-source*
  "
#ifdef VERTEX_SHADER

in vec3  position;
in vec4  color;

smooth out vec4 passColor;

void main() {
    gl_Position = projview * transform * vec4(position, 1.0);
    passColor = color;
}

#else

smooth in vec4 passColor;

out vec4 outColor;

void main() {
	outColor = passColor * ucolor;
}

#endif
")

(defparameter *basic-vertex-color-instanced-source*
  "
#ifdef VERTEX_SHADER

in vec3  position;
in vec4  color;
in mat4  instance_transform;

smooth out vec4 passColor;

void main() {
    gl_Position = projview * instance_transform * vec4(position, 1.0);
    passColor = color;
}

#else

smooth in vec4 passColor;

out vec4 outColor;

void main() {
	outColor = passColor * ucolor;
}

#endif
")

(defparameter *basic-texture-source*
    "
#ifdef VERTEX_SHADER

in vec3  position;
in vec2  texcoord0;

smooth out vec2 passTexCoord;

void main() {
    gl_Position = projview * transform * vec4(position, 1.0);
    passTexCoord = texcoord0;
}

#else

uniform sampler2D tex0;

smooth in vec2 passTexCoord;

out vec4 outColor;

void main() {
	outColor = texture(tex0, passTexCoord) * ucolor;
}

#endif
")
