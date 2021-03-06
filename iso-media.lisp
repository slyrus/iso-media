
;;;
;;; A package for reading and writing ISO (MP4) media files
;;; by Cyrus Harmon <ch-lisp@bobobeach.com>
;;;
;;; The ISO Media spec can be found at:
;;; <http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip>
;;;
;;; ISO Media files are composed of boxes, which can contain other
;;; boxes. The boxes all begin with a type and size; specific box
;;; types add additional fields and then generally contain either more
;;; boxes, in the case of what I call container-boxes, or a big hunk
;;; of encoded media, which I call data-boxes.
;;;

(cl:in-package #:iso-media)

(defgeneric find-child (node type))

;; FIXME! Does we need ways to insert-before/after existing children?
(defgeneric append-child (node child))

(defgeneric remove-child (node child))

(defgeneric filter-children (node type))

;;
;; ISO file headers use a (32-bit) box size of 1 as a sentinel value
;; to indicate that the true value of the box size is stored in a
;; 64-bit large-size field.
(defun large-size-p (size) (= size 1))

;;
;; FIXME: Add support for size 0 boxes, which aren't really size 0,
;; but indicate that the contents run through to the end of the file!

;;
;; Use Peter Seibel's binary-data package to represent boxes. The
;; binary-data package can be found at:
;;  <https://github.com/gigamonkey/monkeylib-binary-data>
;;

(define-binary-type box-size ()
    (:reader (in)
             (loop with value = 0
                for low-bit downfrom (* 8 (1- 4)) to 0 by 8 do
                  (setf (ldb (byte 8 low-bit) value) (read-byte in))
                finally (return value)))
    (:writer (out value)
             (loop for low-bit downfrom (* 8 (1- 4)) to 0 by 8
                do (write-byte (ldb (byte 8 low-bit) value) out))))

(defclass bbox-transient-data ()
  ((parent :accessor parent :initarg :parent :initform nil)))

(define-tagged-binary-class bbox (bbox-transient-data)
  ((size u4)
   (box-type (iso-8859-1-string :length 4))
   (large-size (optional :type 'u8 :if (large-size-p size)))
   (user-type (optional :type '(raw-bytes :size 16)
                        :if (equal box-type "uuid"))))
  (:dispatch
   ;; unfortunately, Apple chose to encode iTunes metadata not
   ;; directly in specific boxes, but rather in generic |data| boxes
   ;; which are enclosed by boxes whose type defines the layout of the
   ;; data box. Yecch... In any event, if we're reading a data box, we
   ;; need to do something special here:
   (let ((parent (first *in-progress-objects*))
         (box-type-symbol (intern box-type (load-time-value *package*))))
     (case box-type-symbol
       ('|data| (find-data-box-class
                 box-type-symbol
                 (when parent
                   (intern (box-type parent)
                           (load-time-value *package*)))))
       (t (find-box-class box-type-symbol))))))

(defmethod print-object ((object bbox) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (size box-type) object
      (format stream "~s :position ~d :size ~d"
              box-type (box-position object) size))))

;;
;; Determining the size of the box header is a bit tricky. There are
;; multiple factors that influence the size of the box header. First,
;; all boxes have a size and type, but both the size and type can be
;; extended to a large size, in the case of the size, and a user type
;; in the case of a type. We need to know the size of the box header
;; in order to read in the box data, but we need to know the type of
;; the box (and the value of some of the header fields) in order to
;; compute the size of the box and its header. So, we use a generic
;; function with the #'+ method combination such that the value
;; returned upon calling header-size for a given object will be the
;; sum of the values returned from each header-size method for the
;; class and its superclasses.
(defgeneric header-size (object) (:method-combination +))

(defgeneric calculate-size (object) (:method-combination +))

(defmethod calculate-size + ((object bbox))
  (header-size object))

(defgeneric update-size (object))

(defmethod update-size ((object bbox))
  (setf (size object) (calculate-size object))
  (when (parent object)
    (update-size (parent object))))

;; All boxes have the following header, mentioned above, which contain
;; the type and size of the box, along with the large-size and the
;; user-type, if necessary.
(defmethod header-size + ((obj bbox))
  (with-slots (large-size user-type) obj
    (+ 8
       (if large-size 4 0)
       (if user-type 16 0))))

(defgeneric data-size (object))

;; For our default (non-container) box, the size of the actual data
;; for the box will be the size of the box minus the size of the
;; header.
(defmethod data-size ((obj bbox))
  (let ((size (or (large-size obj) (size obj))))
    (- size (header-size obj))))

;; The spec defines a "full box" which contains additional fields for a
;; version and various flags. 
(define-binary-class full-bbox-header (bbox)
  ((version u1)
   (flags u3)))

(defmethod header-size + ((obj full-bbox-header)) 4)

(define-binary-class skippable-data-mixin ()
  ((data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () (constantly nil))))))

(define-binary-class data-mixin ()
  ((data (raw-bytes :size (data-size (current-binary-object))))))

(define-binary-class full-bbox (skippable-data-mixin full-bbox-header) ())

(define-binary-class data-bbox (data-mixin bbox) ())

(defmethod calculate-size + ((box data-bbox))
  (+ (length (data box))))

(defun read-boxes (stream limit &optional parent)
  (loop with bytes-read = 0
     while (or (not limit) (< bytes-read limit))
     for child = (handler-case 
                     (read-value 'bbox stream)
                   (end-of-file () nil))
     when child do
       (progn
         (setf (parent child) parent)
         (incf bytes-read (size child)))
     while child collect child))

(defun write-boxes (stream box-list)
  (loop for x in box-list
     do (write-value 'bbox stream x)))

(define-binary-type box-list (limit)
  (:reader (in)
           (read-boxes in limit (current-binary-object)))
  (:writer (out value)
           (write-boxes out value)))

(define-binary-class container-mixin ()
  ((children box-list)))

(define-binary-class iso-container (container-mixin) ())

(defmethod parent ((object iso-container)))

(defmethod header-size + ((object iso-container)) 0)

(defmethod update-size ((object iso-container)))

(defparameter *print-box-children* t)

(defmethod print-object ((object iso-container) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (children) object
      (pprint-logical-block (stream nil)
        (format stream ":children ~a" children)))))

(defmethod find-child ((node iso-container) type)
  (find type
        (children node)
        :key #'box-type
        :test #'equal))

(defmethod find-child ((node bbox) type)
  (find type (children node) :key #'box-type :test #'equal))

(defmethod append-child ((node bbox) child)
  (if (children node)
      (rplacd (last (children node)) (list child))
      (setf (children node) (list child))))

(defmethod remove-child ((node bbox) child)
  (setf (children node)
        (remove child (children node))))

(defmethod filter-children ((node bbox) type)
  (remove-if-not (lambda (x) (equal x type))
                 (children node)
                 :key #'box-type))

(define-binary-class full-container-bbox (full-bbox-header)
  ((children (box-list :limit (data-size (current-binary-object))))))

(defmethod print-object ((object full-container-bbox) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (size box-type) object
      (pprint-logical-block (stream nil)
        (format stream "~s :position ~d :size ~d"
                box-type (box-position object) size)
        (when *print-box-children*
          (loop for child in (children object)
             do (pprint-newline :linear stream)
               (format stream "~x" child)))))))

(defmethod calculate-size + ((box full-container-bbox))
  (reduce #'+ (map 'list #'size (children box))))

(define-binary-class meta-bbox (full-container-bbox) ())

(define-binary-type terminated-string () (iso-8859-1-terminated-string :terminator +null+))

(define-binary-class handler-bbox (full-bbox-header)
  ((pre-defined u4)
   (handler-type u4)
   (reserved-1 u4)
   (reserved-2 u4)
   (reserved-3 u4)
   (name terminated-string)
   (data 
    (optional
     :type `(skippable-raw-bytes :size ,(data-size (current-binary-object))
                                 :predicate ,#'(lambda () (constantly nil)))
     :if (plusp (data-size (current-binary-object)))))))

(defmethod header-size + ((obj handler-bbox))
           (+ 20
              (or (when (and (slot-boundp obj 'name) (name obj))
                    (1+ (length (name obj))))
                  0)))

(define-binary-class container-bbox (bbox)
  ((children (box-list :limit (data-size (current-binary-object))))))

(defmethod print-object ((object container-bbox) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (size box-type) object
      (pprint-logical-block (stream nil)
        (format stream "~s :position ~d :size ~d"
                box-type (box-position object) size)
        (when *print-box-children*
          (loop for child in (children object)
             do (pprint-newline :linear stream)
               (format stream "~x" child)))))))

(defmethod calculate-size + ((box container-bbox))
  (reduce #'+ (map 'list #'size (children box))))

(define-binary-class sample-description-bbox (full-bbox-header)
  ((entry-count u4)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj sample-description-bbox)) 4)

(defmethod calculate-size + ((box sample-description-bbox))
  (+ (reduce #'+ (map 'list #'size (children box)))))

(defparameter *read-movie-data* t)

(define-binary-class movie-data-bbox (bbox)
  ((data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () *read-movie-data*)))))

(define-binary-class movie-header-bbox (full-bbox-header)
  ((creation-time (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (modification-time (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (timescale u4)
   (duration (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (rate u4)
   (volume u2)
   (reserved1 u2)
   (reserved2 u4)
   (reserved3 u4)
   (matrix1 u4)
   (matrix2 u4)
   (matrix3 u4)
   (matrix4 u4)
   (matrix5 u4)
   (matrix6 u4)
   (matrix7 u4)
   (matrix8 u4)
   (matrix9 u4)
   (pre-defined1 u4)
   (pre-defined2 u4)
   (pre-defined3 u4)
   (pre-defined4 u4)
   (pre-defined5 u4)
   (pre-defined6 u4)
   (next-track-id u4)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj movie-header-bbox)) 
           (+ (if (= (version obj) 1) 28 16)
              4 2 2 8 36 24 4))

(defmethod calculate-size + ((box movie-header-bbox))
  (+ (reduce #'+ (map 'list #'size (children box)))))

(define-binary-class track-header-bbox (full-bbox-header)
  ((creation-time
    (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (modification-time
    (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (track-id u4)
   (reserved1 u4)
   (duration
    (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (reserved2 u4)
   (reserved3 u4)
   (layer u2)
   (alternate-group u2)
   (volume u2)
   (reserved4 u2)
   (matrix1 u4)
   (matrix2 u4)
   (matrix3 u4)
   (matrix4 u4)
   (matrix5 u4)
   (matrix6 u4)
   (matrix7 u4)
   (matrix8 u4)
   (matrix9 u4)
   (width u4)
   (height u4)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj track-header-bbox)) 
           (+ (if (= (version obj) 1) 32 20)
              8 8 36 4 4))

(defmethod calculate-size + ((box track-header-bbox))
  (+ (reduce #'+ (map 'list #'size (children box)))))

(define-binary-class media-header-bbox (full-bbox-header)
  ((creation-time
    (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (modification-time
    (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (timescale u4)
   (duration (dynamic :choose (if (= 1 (version (current-binary-object))) 'u8 'u4)))
   (pad-and-language u2)
   (pre-defined u2)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj media-header-bbox)) 
           (+ (if (= (version obj) 1) 28 16) 4))

(defmethod calculate-size + ((box media-header-bbox))
  (+ (reduce #'+ (map 'list #'size (children box)))))

(define-binary-class apple-data-bbox-header (full-bbox-header)
  ((pad u4)))

(defmethod header-size + ((obj apple-data-bbox-header)) 4)

(define-binary-class apple-data-bbox (data-mixin apple-data-bbox-header) ())

(defmethod calculate-size + ((box apple-data-bbox))
  (+ (length (data box))))

(define-binary-class itunes-track-number-bbox (apple-data-bbox-header)
  ((pad1 u2)
   (track-num u2)
   (track-count u2)
   (pad2 u2)
   (data (raw-bytes :size (data-size (current-binary-object))))))

(defmethod header-size + ((obj itunes-track-number-bbox)) 8)

(define-binary-class itunes-disk-number-bbox (apple-data-bbox-header)
  ((pad1 u2)
   (disk-num u2)
   (disk-count u2)
   (data (raw-bytes :size (data-size (current-binary-object))))))

(defmethod header-size + ((obj itunes-disk-number-bbox)) 6)

(define-binary-class sample-chunk-descriptor ()
  ((first-chunk u4)
   (samples-per-chunk u4)
   (sample-description-index u4)))

(defparameter *sample-chunk-descriptor-size* 12)

;;; 8.17 Sample Size Boxes

;;; SampleSizeBox - stsz
(define-binary-class sample-size-box (full-bbox-header)
  ((sample-size u4)
   (sample-count u4)
   (entry-sizes
    (optional
     :type `(array :type u4 :size ,sample-count)
     :if (= sample-size 0)))))

(defmethod calculate-size + ((box sample-size-box))
           (with-accessors ((sample-size sample-size)
                            (sample-count sample-count))
               box
             (+ 8 (* (if (= sample-size 0)
                         (* 4 sample-count)
                         0)))))

;;;
;;; 8.18 Sample to Chunk Box

;;; SampleToChunkBox - stsc
(define-binary-class sample-to-chunk-box (full-bbox-header)
  ((entry-count u4)
   (sample-chunk-descriptors (array :type 'sample-chunk-descriptor :size entry-count))))

(defmethod calculate-size + ((box sample-to-chunk-box))
           (+ 4 (* (length (sample-chunk-descriptors box))
                   *sample-chunk-descriptor-size*)))

;;;
;;; 8.19 Chunk Offset Box

;;; ChunkOffsetBox - stco
(define-binary-class chunk-offset-box (full-bbox-header)
  ((entry-count u4)
   (chunk-offsets (array :type 'u4 :size entry-count))))

(defmethod calculate-size + ((box chunk-offset-box))
           (+ 4 (* (length (chunk-offsets box)) 4)))

;;; ChunkLargeOffsetBox - co64
(define-binary-class chunk-large-offset-box (full-bbox-header)
  ((entry-count u4)
   (chunk-offsets (array :type 'u8 :size entry-count))))

(defmethod calculate-size + ((box chunk-large-offset-box))
           (+ 4 (* (length (chunk-offsets box)) 8)))


;;;
(defparameter *copyright-symbol-string* (string (code-char 169)))

(defun make-copyright-symbol-string (suffix)
  (concatenate 'string *copyright-symbol-string* suffix))

(defun make-copyright-symbol-symbol (string)
  (intern (make-copyright-symbol-string string)
          (load-time-value *package*)))

(defgeneric find-box-class (box-type)
  (:method (box-type) 'data-bbox)
  (:method ((box-type (eql '|moov|))) 'container-bbox)
  (:method ((box-type (eql '|trak|))) 'container-bbox)
  (:method ((box-type (eql '|mdia|))) 'container-bbox)
  (:method ((box-type (eql '|minf|))) 'container-bbox)
  (:method ((box-type (eql '|stbl|))) 'container-bbox)
  (:method ((box-type (eql '|udta|))) 'container-bbox)
  (:method ((box-type (eql '|meta|))) 'meta-bbox)
  (:method ((box-type (eql '|hdlr|))) 'handler-bbox)
  (:method ((box-type (eql '|mdat|))) 'movie-data-bbox)
  (:method ((box-type (eql '|stsd|))) 'sample-description-bbox)
  (:method ((box-type (eql '|mvhd|))) 'movie-header-bbox)
  (:method ((box-type (eql '|tkhd|))) 'track-header-bbox)
  (:method ((box-type (eql '|mdhd|))) 'media-header-bbox)
  (:method ((box-type (eql '|data|))) 'apple-data-bbox)
  (:method ((box-type (eql '|ilst|))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "nam")))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "ART")))) 'container-bbox)
  (:method ((box-type (eql '|aART|))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "alb")))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "grp")))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "day")))) 'container-bbox)
  (:method ((box-type (eql '|trkn|))) 'container-bbox)
  (:method ((box-type (eql '|disk|))) 'container-bbox)
  (:method ((box-type (eql '|tmpo|))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "wrt")))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "cmt")))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "gen")))) 'container-bbox)
  (:method ((box-type (eql '|gnre|))) 'container-bbox)
  (:method ((box-type (eql '|cpil|))) 'container-bbox)
  (:method ((box-type (eql '|tvsh|))) 'container-bbox)
  (:method ((box-type (eql '|sonm|))) 'container-bbox)
  (:method ((box-type (eql '|soar|))) 'container-bbox)
  (:method ((box-type (eql '|soaa|))) 'container-bbox)
  (:method ((box-type (eql '|soal|))) 'container-bbox)
  (:method ((box-type (eql '|soco|))) 'container-bbox)
  (:method ((box-type (eql '|sosn|))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "lyr")))) 'container-bbox)
  (:method ((box-type (eql '|covr|))) 'container-bbox)
  (:method ((box-type (eql (make-copyright-symbol-symbol "too")))) 'container-bbox)
  (:method ((box-type (eql '|stsz|))) 'sample-size-box)
  (:method ((box-type (eql '|stsc|))) 'sample-to-chunk-box)
  (:method ((box-type (eql '|stco|))) 'chunk-offset-box)
  (:method ((box-type (eql '|co64|))) 'chunk-large-offset-box))

(defgeneric find-data-box-class (box-type parent-type)
  (:method (box-type parent) 'apple-data-bbox)
  (:method (box-type (parent (eql '|trkn|))) 'itunes-track-number-bbox)
  (:method (box-type (parent (eql '|disk|))) 'itunes-disk-number-bbox))

(macrolet ((def-find-data-box-class-method (type class)
             `(defmethod find-data-box-class (box-type (parent (eql ,type))) ,class)))
  (mapcar
   #'(lambda (type-and-class)
       (destructuring-bind (type . class) type-and-class
         (def-find-data-box-class-method type class)))
   `((,(make-copyright-symbol-symbol "nam") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "ART") . apple-data-bbox)
     (|aART| . apple-data-bbox)
     (,(make-copyright-symbol-symbol "alb") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "grp") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "day") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "wrt") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "cmt") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "lyr") . apple-data-bbox)
     (,(make-copyright-symbol-symbol "too") . apple-data-bbox)
     (|sonm| . apple-data-bbox)
     (|soar| . apple-data-bbox)
     (|soaa| . apple-data-bbox)
     (|soal| . apple-data-bbox)
     (|soco| . apple-data-bbox)
     (|sosn| . apple-data-bbox)
     (|tvsh| . apple-data-bbox))))

;;;
;;; functions to access data in iso-containers
(defun audio-sample-type (iso-container)
  (box-type
   (first
    (children
     (reduce #'find-child
             (list
              iso-container
              "moov" "trak" "mdia" "minf" "stbl" "stsd"))))))

(defun itunes-container-box (iso-container type)
  (reduce #'(lambda (x y) (when x (find-child x y)))
          (list iso-container "moov" "udta" "meta" "ilst" type "data")))

(macrolet 
    ((defitunes-getter (accessor-name accessor-type)
       `(defun ,accessor-name (iso-container)
          (let ((box (itunes-container-box iso-container ,accessor-type)))
            (when box
              (cond 
                ((and (= (flags box) 1)
                      (eql (type-of box) 'apple-data-bbox))
                 (map 'string #'code-char
                      (data box)))
                (t (data box))))))))
  (defitunes-getter track-name (make-copyright-symbol-string "nam"))
  (defitunes-getter artist (make-copyright-symbol-string "ART"))
  (defitunes-getter album-artist "aART")
  (defitunes-getter album-name (make-copyright-symbol-string "alb"))
  (defitunes-getter grouping (make-copyright-symbol-string "grp"))
  (defitunes-getter year-of-publication (make-copyright-symbol-string "day"))
  (defitunes-getter tempo "tmpo")
  (defitunes-getter composer-name (make-copyright-symbol-string "wrt"))
  (defitunes-getter comments (make-copyright-symbol-string "cmt"))
  (defitunes-getter genre (make-copyright-symbol-string "gen"))
  (defitunes-getter genre-code "gnre")
  (defitunes-getter compilation-part "cpil")
  (defitunes-getter show-name "tvsh")
  (defitunes-getter sort-track-name "sonm")
  (defitunes-getter sort-artist "soar")
  (defitunes-getter sort-album-artist "soaa")
  (defitunes-getter sort-album-name "soal")
  (defitunes-getter sort-composer-name "soco")
  (defitunes-getter sort-show-name "sosn")
  (defitunes-getter lyrics (make-copyright-symbol-string "lyr"))
  (defitunes-getter cover "covr")
  (defitunes-getter information (make-copyright-symbol-string "too")))


(defun make-data-box (iso-container accessor-type
                      &key force-new-box
                      (box-type 'apple-data-bbox)
                      (box-flags 1))
  (let ((ilst-box
         (reduce #'(lambda (x y) (when x (find-child x y)))
                 (list iso-container "moov" "udta" "meta" "ilst"))))
    (when ilst-box
      (let ((container-box
             (or (find-child ilst-box accessor-type)
                 (let ((box (make-instance 'container-bbox :size 0
                                           :box-type accessor-type
                                           :children nil
                                           :large-size nil
                                           :user-type nil)))
                   (append-child ilst-box box)
                   (setf (parent box) ilst-box)
                   box))))
        (when force-new-box
          (loop for b in (children container-box)
             do (remove-child container-box b)))
        (let ((data-box
               (or (find-child container-box "data")
                   (let ((box (make-instance box-type :size 0
                                             :box-type "data"
                                             :version 0
                                             :flags box-flags
                                             :pad 0
                                             :large-size nil
                                             :user-type nil)))
                     (append-child container-box box)
                     (setf (parent box) container-box)
                     box))))
          (values data-box ilst-box))))))

(macrolet 
    ((defitunes-setter (accessor-name accessor-type &key force-new-box)
       `(defun (setf ,accessor-name) (value iso-container)
          (if value
              (multiple-value-bind (data-box ilst-box)
                  (make-data-box iso-container ,accessor-type :force-new-box ,force-new-box)
                (if ilst-box
                    (prog1
                        (setf (data data-box)
                              (babel:string-to-octets value))
                      (update-size data-box)
                      (update-stco-box iso-container))
                    (error "Could not get ilst-box ~S." ,accessor-type)))
              ;; FIXME! Removing boxes breaks things horribly!!
              (let ((ilst-box
                     (reduce #'(lambda (x y) (when x (find-child x y)))
                             (list iso-container "moov" "udta" "meta" "ilst"))))
                (let ((container-box
                       (find-child ilst-box ,accessor-type)))
                  (when container-box
                    (remove-child ilst-box container-box)
                    (update-stco-box iso-container))))))))
  (defitunes-setter track-name (make-copyright-symbol-string "nam"))
  (defitunes-setter artist (make-copyright-symbol-string "ART"))
  (defitunes-setter album-artist "aART" :force-new-box t)
  (defitunes-setter album-name (make-copyright-symbol-string "alb"))
  (defitunes-setter grouping (make-copyright-symbol-string "grp"))
  (defitunes-setter year-of-publication (make-copyright-symbol-string "day"))
  (defitunes-setter tempo "tmpo")
  (defitunes-setter composer-name (make-copyright-symbol-string "wrt"))
  (defitunes-setter comments (make-copyright-symbol-string "cmt"))
  (defitunes-setter genre (make-copyright-symbol-string "gen"))
  (defitunes-setter genre-code "gnre")
  (defitunes-setter compilation-part "cpil")
  (defitunes-setter show-name "tvsh")
  (defitunes-setter sort-track-name "sonm")
  (defitunes-setter sort-artist "soar")
  (defitunes-setter sort-album-artist "soaa")
  (defitunes-setter sort-album-name "soal")
  (defitunes-setter sort-composer-name "soco")
  (defitunes-setter sort-show-name "sosn")
  (defitunes-setter lyrics (make-copyright-symbol-string "lyr"))
  (defitunes-setter cover "covr")
  (defitunes-setter information (make-copyright-symbol-string "too")))

;;;

(defun ancestors (box)
  (labels ((%ancestors (box &optional acc)
           (if (parent box)
               (%ancestors (parent box) (cons (parent box) acc))
               acc)))
    (%ancestors box)))

(defun elder-siblings (box)
  (let ((parent (parent box)))
    (when parent
      (let* ((children (children parent))
            (pos (position box children)))
        (when pos
          (subseq children 0 pos))))))

(defun box-position (box &optional (offset 0))
  (let ((offset1 (+ offset (reduce #'+ (mapcar #'size (elder-siblings box))))))
    (with-accessors ((parent parent)) box
      (if parent
          (box-position parent (+ offset1 (header-size parent)))
          offset1))))

(defun reductions (function sequence &rest args &key initial-value)
  (let ((l (list (or initial-value (first sequence)))))
    (apply #'reduce
           (lambda (a b)
             (let ((val (funcall function a b)))
               (push val l)
               val))
           sequence
           args)
    (nreverse l)))

;;;
;;; We have to update the stco (Sample-to-Chunk-Offset) box if we make
;;; changes to the boxes because the stco box has absolute (in
;;; file-relative position) offsets of the files chunks (which are, we
;;; assume, stored in the mdat box).
(defun update-stco-box (iso-container)
  (let ((stco (reduce #'find-child
                      (list
                       iso-container
                       "moov" "trak" "mdia" "minf" "stbl" "stco")))
        (mdat (find-child iso-container "mdat")))
    (when (and stco mdat)
      (let* ((offsets (chunk-offsets stco))
             (deltas (loop for (a b)
                        on (map 'list #'identity offsets)
                        while b collect (- b a))))
        (let ((new-offsets (reductions #'+ (cons (+ (box-position mdat)
                                      (header-size mdat))
                                   deltas))))
          (setf (chunk-offsets stco)
                (make-array (length new-offsets)
                            :initial-contents new-offsets)))))))

(defun track-number (iso-container)
  (let ((box (itunes-container-box iso-container "trkn")))
            (when box
              (list (track-num box) (track-count box)))))

(defun (setf track-number) (value-list iso-container)
  (if value-list
      (destructuring-bind (num &optional count) (if (listp value-list) 
                                                    value-list
                                                    (list value-list))
        (multiple-value-bind (data-box ilst-box)
            (make-data-box iso-container "trkn"
                           :box-type 'itunes-track-number-bbox
                           :box-flags 0)
          (if ilst-box
              (progn
                (setf (pad1 data-box) 0)
                (setf (pad2 data-box) 0)
                (setf (data data-box) nil)
                (setf (track-num data-box) (if (numberp num)
                                               num
                                               (parse-integer num)))
                (if count
                    (setf (track-count data-box) (if (numberp count)
                                                     count
                                                     (parse-integer count)))
                    (setf (track-count data-box) 0))
                (update-size data-box)
                (update-stco-box iso-container)
                value-list)
              (error "Could not get ilst-box trkn."))))))

(defun disk-number (iso-container)
  (let ((box (itunes-container-box iso-container "disk")))
            (when box
              (list (disk-num box) (disk-count box)))))

;;;
;;; main read and write routines
(defun read-iso-media-stream (stream)
  (read-value 'iso-container stream))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))

(defun optimize-iso-container (iso-container)
  (let ((mdat (find-child iso-container "mdat")))
    (when mdat
      (let ((children (remove mdat (children iso-container))))
        (setf (children iso-container)
              (append children (list mdat)))))))

(defun iso-container-optimized-p (iso-container)
  (not (find "mdat"
             (butlast (children iso-container))
             :key #'box-type
             :test #'equal)))

(defun write-iso-media-stream (stream obj &key (optimize t))
  (when optimize
    (optimize-iso-container obj)
    (update-stco-box obj))
  (write-value 'iso-container stream obj))

(defun write-iso-media-file (file obj &key (optimize t optimize-supplied-p))
  (with-open-file (out file 
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (apply #'write-iso-media-stream out obj
           (when optimize-supplied-p `(:optimize ,optimize)))
    file))

;;; routines for dumping ISO file structure (debugging support?)
;;; (defun dump-iso-container (container)
;;;  )
