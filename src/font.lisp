(in-package :threegl)

(bs:define-io-structure font-glyph-data
  (adv-x sint32)
  (adv-y sint32)
  (bearing-x sint32)
  (bearing-y sint32)
  (char-width sint32)
  (char-height sint32)
  (rect-x float32)
  (rect-y float32)
  (rect-width float32)
  (rect-height float32))

(defconstant +max-char-support+ 128)

(bs:define-io-structure font-atlas
  "GFNT"
  (size sint32)
  (square-size sint32)
  (tex-width sint32)
  (tex-height sint32)
  (new-line-height sint32)
  (glyph-data (vector font-glyph-data (* +max-char-support+ 1)))
  (pixels (vector uint8 (* (bs:slot :tex-width) (bs:slot :tex-height) 4))))

(defstruct font
  (fnt nil :type font-atlas)
  (tex nil :type texture))

(defun load-font (filename)
  (let* ((fnt (read-font-atlas filename))
         (tex (create-texture-2d
               (font-atlas-pixels fnt)
               :width (font-atlas-tex-width fnt)
               :height (font-atlas-tex-height fnt)
               :pformat :rgba8)))
    (make-font :fnt fnt :tex tex)))

