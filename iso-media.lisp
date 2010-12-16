
;;
;; reading ISO media files
;; spec can be found here: http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip
;;

(cl:in-package #:iso-media)
 
(defgeneric find-child (node type))
(defgeneric filter-children (node type))

(defun large-size-p (size) (= size 1))

(define-tagged-binary-class bbox ()
  ((size u4)
   (box-type (iso-8859-1-string :length 4))
   (large-size (optional :type 'u8 :if (large-size-p size)))
   (user-type (optional :type '(raw-bytes :size 16)
                        :if (equal box-type "uuid"))))
  (:dispatch (find-box-class box-type)))

(defgeneric header-size (object)
  (:method-combination +))

(defmethod header-size + ((obj bbox))
  (with-slots (large-size user-type) obj
    (+ 8
       (if large-size 4 0)
       (if user-type 16 0))))

(defmethod data-size ((obj bbox))
  (- (size obj) (header-size obj)))

(defmethod print-object ((object bbox) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size size)
                 (box-type box-type)) object
      (format stream "~s :size ~d" box-type size))))

(define-binary-class full-bbox-header (bbox)
  ((version u1)
   (flags u3)))

(defmethod header-size + ((obj full-bbox-header)) 4)

(define-binary-class full-bbox (full-bbox-header)
  ((data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () (constantly nil)))q)))

(define-binary-class data-bbox (bbox)
  ((data (raw-bytes :size (data-size (current-binary-object))))))

(defun read-boxes (stream limit)
  (loop with bytes-read = 0
     while (or (not limit) (< bytes-read limit))
     for child = (handler-case 
                     (read-value 'bbox stream)
                   (end-of-file () nil))
     when child do
     (incf bytes-read (size child))
       while child collect child))

(defun write-boxes (stream box-list)
  (loop for x in box-list
     do (write-value 'bbox stream x)))

(define-binary-type box-list (limit)
  (:reader (in)
           (read-boxes in limit))
  (:writer (out value)
           (write-boxes out value)))

(define-binary-class iso-container ()
  ((children box-list)))

(defmethod print-object ((object iso-container) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (children) object
      (format stream ":children ~a" children))))

(defmethod find-child ((node iso-container) type)
  (find type
        (children node)
        :key #'box-type
        :test #'equal))

(defmethod find-child ((node bbox) type)
  (find type (children node) :key #'box-type :test #'equal))

(defmethod filter-children ((node bbox) type)
  (remove-if-not (lambda (x) (equal x type))
                 (children node)
                 :key #'box-type))

(define-binary-class full-container-bbox (full-bbox-header)
  ((children (box-list :limit (data-size (current-binary-object))))))

(define-binary-class meta-bbox (full-container-bbox) ())

(define-binary-type terminated-string () (iso-8859-1-terminated-string :terminator +null+))

(define-binary-class handler-bbox (full-bbox-header)
  ((pre-defined u4)
   (handler-type u4)
   (reserved-1 u4)
   (reserved-2 u4)
   (reserved-3 u4)
   (name (optional :type 'terminated-string
                   :if (plusp (data-size (current-binary-object)))))
   (data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () (constantly nil))))))

(defmethod header-size + ((obj handler-bbox))
           (+ 20
              (or (when (and (slot-boundp obj 'name) (name obj))
                    (1+ (length (name obj))))
                  0)))

(define-binary-class container-bbox (bbox)
  ((children (box-list :limit (data-size (current-binary-object))))))

(define-binary-class sample-description-bbox (full-bbox-header)
  ((entry-count u4)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj sample-description-bbox)) 4)

(defparameter *read-movie-data* nil)

(define-binary-class movie-data-bbox (full-bbox-header)
  ((data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () *read-movie-data*)))))

(define-binary-class movie-header-bbox (full-bbox-header)
  ((creation-time (dynamic :type-fn
                           #'(lambda ()
                             (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (modification-time (dynamic :type-fn
                               #'(lambda ()
                                   (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (timescale u4)
   (duration (dynamic :type-fn
                      #'(lambda ()
                        (if (= 1 (version (current-binary-object))) 'u8 'u4))))
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

(define-binary-class track-header-bbox (full-bbox-header)
  ((creation-time
    (dynamic :type-fn
             #'(lambda ()
                 (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (modification-time
    (dynamic :type-fn
             #'(lambda ()
                 (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (track-id u4)
   (reserved1 u4)
   (duration
    (dynamic :type-fn
             #'(lambda ()
                 (if (= 1 (version (current-binary-object))) 'u8 'u4))))
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

(define-binary-class media-header-bbox (full-bbox-header)
  ((creation-time
    (dynamic :type-fn
             #'(lambda ()
                 (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (modification-time
    (dynamic :type-fn
             #'(lambda ()
                 (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (timescale u4)
   (duration (dynamic :type-fn
                      #'(lambda ()
                        (if (= 1 (version (current-binary-object))) 'u8 'u4))))
   (pad-and-language u2)
   (pre-defined u2)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj media-header-bbox)) 
           (+ (if (= (version obj) 1) 28 16) 4))

(define-binary-class apple-data-bbox (full-bbox-header)
  ((pad u4)
   (data (raw-bytes :size (data-size (current-binary-object))))))

(defmethod header-size + ((obj apple-data-bbox)) 4)

(defparameter *copyright-symbol-string* #.(string (code-char 169)))

(defparameter *bbox-type-hash* (make-hash-table :test 'equal))
(progn 
  (clrhash *bbox-type-hash*)
  (map nil (lambda (x)
             (destructuring-bind (type class) x
               (setf (gethash type *bbox-type-hash*) class)))
       `(("moov" container-bbox)
         ("trak" container-bbox)
         ("mdia" container-bbox)
         ("minf" container-bbox)
         ("stbl" container-bbox)
         ("udta" container-bbox)
         ("meta" meta-bbox)
         ("hdlr" handler-bbox)
         ("mdat" movie-data-bbox)
         ("stsd" sample-description-bbox)
         ("mvhd" movie-header-bbox)
         ("tkhd" track-header-bbox)
         ("mdhd" media-header-bbox)

         ("ilst" container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "nam") container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "ART") container-bbox)
         ("aART" container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "alb") container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "grp") container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "day") container-bbox)
         ("trkn" container-bbox)
         ("disk" container-bbox)
         ("tmpo" container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "wrt") container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "cmt") container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "gen") container-bbox)
         ("gnre" container-bbox)
         ("cpil" container-bbox)
         ("tvsh" container-bbox)
         ("sonm" container-bbox)
         ("soar" container-bbox)
         ("soaa" container-bbox)
         ("soal" container-bbox)
         ("soco" container-bbox)
         ("sosn" container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "lyr") container-bbox)
         ("covr" container-bbox)
         (,(concatenate 'string *copyright-symbol-string* "too") container-bbox)
         
         ("data" apple-data-bbox))))

(defun find-box-class (box-type)
  (or (gethash box-type *bbox-type-hash*)
      'data-bbox))

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

(defun itunes-container-box-info (iso-container type)
  (data
   (reduce #'find-child
           (list iso-container "moov" "udta" "meta" "ilst" type "data"))))

(macrolet 
    ((defitunes-getter (accessor-name accessor-type)
       `(defun ,accessor-name (iso-container)
          (map 'string #'code-char
               (itunes-container-box-info iso-container ,accessor-type)))))
  (defitunes-getter track-name (concatenate 'string *copyright-symbol-string* "nam"))
  (defitunes-getter artist (concatenate 'string *copyright-symbol-string* "ART"))
  (defitunes-getter album-artist "aART")
  (defitunes-getter album-name (concatenate 'string *copyright-symbol-string* "alb"))
  (defitunes-getter grouping (concatenate 'string *copyright-symbol-string* "grp"))
  (defitunes-getter year-of-publication (concatenate 'string *copyright-symbol-string* "day"))
  (defitunes-getter track-number "trkn")
  (defitunes-getter disk-number "disk")
  (defitunes-getter tempo "tmpo")
  (defitunes-getter composer-name (concatenate 'string *copyright-symbol-string* "wrt"))
  (defitunes-getter comments (concatenate 'string *copyright-symbol-string* "cmt"))
  (defitunes-getter genre (concatenate 'string *copyright-symbol-string* "gen"))
  (defitunes-getter genre-code "gnre")
  (defitunes-getter compilation-part "cpil")
  (defitunes-getter show-name "tvsh")
  (defitunes-getter sort-track-name "sonm")
  (defitunes-getter sort-artist "soar")
  (defitunes-getter sort-album-artist "soaa")
  (defitunes-getter sort-album-name "soal")
  (defitunes-getter sort-composer-name "soco")
  (defitunes-getter sort-show-name "sosn")
  (defitunes-getter lyrics (concatenate 'string *copyright-symbol-string* "lyr"))
  (defitunes-getter cover "covr")
  (defitunes-getter information (concatenate 'string *copyright-symbol-string* "too")))

;;;
;;; main read and write routines
(defun read-iso-media-stream (stream)
  (read-value 'iso-container stream))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))

(defun write-iso-media-stream (stream obj)
  (write-value 'iso-container stream obj))

(defun write-iso-media-file (file obj)
  (with-open-file (out file 
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-iso-media-stream out obj)))

