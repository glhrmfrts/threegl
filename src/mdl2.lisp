(in-package #:threegl)

(defconstant +DTRIVERTX_V0+ 0)
(defconstant +DTRIVERTX_V1+ 1)
(defconstant +DTRIVERTX_V2+ 2)
(defconstant +DTRIVERTX_LNI+ 3)
(defconstant +DTRIVERTX_SIZE+ 4)

(bs:define-io-structure mdl2-st-vertex
  (s sint16)
  (t sint16))

(bs:define-io-structure mdl2-triangle
  (index-xyz (vector sint16 3))
  (index-st (vector sint16 3)))

(bs:define-io-structure mdl2-vertex
  (xyz (vector uint8 3))
  (light-normal-index uint8))

(bs:define-io-structure mdl2-header
  (ident sint32)
  (version sint32)

  (skinwidth sint32)
  (skinheight sint32)
  (framesize sint32)

  (num-skins sint32)
  (num-xyz sint32)
  (num-st sint32)
  (num-tris sint32)
  (num-glcmds sint32)
  (num-frames sint32)
  
  (ofs-skins sint32)
  (ofs-st sint32)
  (ofs-tris sint32)
  (ofs-frames sint32)
  (ofs-glcmds sint32)
  (ofs-end sint32))

(defun load-mdl2 (filename)
  (with-open-file (file filename :direction :input
                                 :if-does-not-exist :error
                                 :element-type '(unsigned-byte 8))
    (let* ((hdr (read-mdl2-header file)))
      (describe hdr)
      hdr)))
