(in-package :threegl)

(defconstant +mesh-vertex-flag-position+ 1)
(defconstant +mesh-vertex-flag-normal+ 2)
(defconstant +mesh-vertex-flag-texcoord+ 4)
(defconstant +mesh-vertex-flag-color+ 8)

(defconstant +mesh-lump-flag-vertices+ 1)
(defconstant +mesh-lump-flag-indices+ 2)
(defconstant +mesh-lump-flag-objects+ 4)

(defconstant +mesh-index-none+ 0)
(defconstant +mesh-index-uint16+ 1)
(defconstant +mesh-index-uint32+ 2)

(defconstant +mesh-lump-index-vertices+ 0)
(defconstant +mesh-lump-index-indices+ 1)
(defconstant +mesh-lump-index-objects+ 2)

(defconstant +max-mesh-lumps+ 4)

(defconstant +mesh-object-size+ 72)

(bs:define-io-structure mesh-lump
  (offset uint32)
  (size uint32))

(bs:define-io-structure mesh-object
  (material-name (string 64))
  (first-index uint32)
  (index-count uint32))

(bs:define-io-structure mesh-header
  "QMSH"
  (lump-flags uint32)
  (vertex-flags uint32)
  (index-format uint32)
  (lumps (vector mesh-lump (* +max-mesh-lumps+ 1))))

(defstruct mesh-runtime-object
  (material nil :type material)
  (first-index 0 :type integer)
  (index-count 0 :type integer))

(defstruct mesh
  (geometry nil :type geometry-buffer)
  (objects nil :type '(list mesh-runtime-object)))

(defun load-mesh (filename)
  (with-open-file (file filename :direction :input)
    (labels

      ((compute-mesh-attrs (mdata)
        (remove-if-not #'listp
          (loop for flag in (list +mesh-vertex-flag-position+
                                  +mesh-vertex-flag-normal+
                                  +mesh-vertex-flag-texcoord+
                                  +mesh-vertex-flag-color+)
          collect
            (when (logand (mesh-header-vertex-flags mdata) flag)
              (case flag
                (+mesh-vertex-flag-position+ (make-position3d-vertex-attribute))
                (+mesh-vertex-flag-normal+ (make-normal-vertex-attribute))
                (+mesh-vertex-flag-texcoord+ (make-texcoord-vertex-attribute))
                (+mesh-vertex-flag-color+ (make-color-vertex-attribute)))))))

       (lump-offset (mdata lump)
        (mesh-lump-offset (aref (mesh-header-lumps mdata) lump)))

       (lump-size (mdata lump)
        (mesh-lump-size (aref (mesh-header-lumps mdata) lump)))

       (compute-index-byte-size (mdata)
        (case (mesh-header-index-format mdata)
          (+mesh-index-uint16+ 2)
          (+mesh-index-uint32+ 4)))

       (compute-index-format (mdata)
        (case (mesh-header-index-format mdata)
          (+mesh-index-uint16+ :uint16)
          (+mesh-index-uint32+ :uint32)))

       (read-lump-contents (cnt offs)
        (let ((seq (make-array cnt :element-type '(unsigned-byte 8))))
          (file-position file offs)
          (read-sequence seq file)
          seq))

       (read-objects (cnt offs)
          (file-position file offs)
          (loop for i from 0 below cnt
           collect
            (let ((obj (read-mesh-object file))) obj)))

       (create-mesh-runtime-objects (mdata objs)
        (loop for obj in objs
         collect
          (let ((mat (load-material (make-pathname :name (mesh-object-material-name obj)))))
            (make-mesh-runtime-object
              :material mat
              :first-index (mesh-object-first-index obj)
              :index-count (mesh-object-index-count obj))))))

        (let* ((mesh-data (read-mesh-header file))
               (attrs (compute-mesh-attrs mesh-data))

               (vertex-size (compute-vertex-byte-size attrs))
               (vertex-count (/ (lump-size mesh-data +mesh-lump-index-vertices+) vertex-size))
               (vertices (read-lump-contents vertex-count (lump-offset mesh-data +mesh-lump-index-vertices+)))

               (index-size (compute-index-byte-size mesh-data))
               (index-count (/ (lump-size mesh-data +mesh-lump-index-indices+) index-size))
               (indices (read-lump-contents index-count (lump-offset mesh-data +mesh-lump-index-indices+)))

               (object-count (/ (lump-size mesh-data +mesh-lump-index-objects+) +mesh-object-size+))
               (objects (read-objects object-count (lump-offset mesh-data +mesh-lump-index-objects+)))

               (geometry (create-geometry-buffer attrs index-format :static-draw))
               (runtime-objects (create-mesh-runtime-objects mesh-data objects)))

          (upload-geometry geometry
            :vertices (create-gl-array :uchar vertices)
            :indices (create-gl-array :uchar indices))

          (make-mesh :geometry geometry :objects objects)))))
