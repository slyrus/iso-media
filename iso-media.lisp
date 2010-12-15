
;;
;; reading ISO media files
;; spec can be found here: http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip
;;

(cl:defpackage #:iso-media
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.binary-data.common-datatypes)
  (:export #:iso-container
           #:children

           #:box
           #:box-type
           #:box-size
           
           #:box-parent
           #:box-data
           #:make-box
           
           #:read-n-bytes
           #:read-32-bit-int

           #:media-type-string
           #:media-type-vector
           #:media-type-vector-to-int
           #:media-type-string-to-int

           #:find-box-type
           #:find-ancestor
           #:find-child

           #:read-box
           
           #:read-iso-media-stream
           #:read-iso-media-file
           
           #:audio-sample-type

           #:track-name
           #:artist
           #:album-artist
           #:album-name
           #:grouping
           #:year-of-publication
           #:track-number
           #:disk-number
           #:tempo
           #:composer-name
           #:comments
           #:genre
           #:genre-code
           #:compilation-part
           #:show-name
           #:sort-track-name
           #:sort-artist
           #:sort-album-artist
           #:sort-album-name
           #:sort-composer-name
           #:sort-show-name
           #:lyrics
           #:cover
           #:information))

(cl:in-package #:iso-media)

(defun media-type-string (type-int)
  (map 'string #'code-char type-int))

(defun media-type-vector (type-string)
  (map 'vector #'char-code type-string))

(defun media-type-vector-to-int (type-vector)
  (reduce (lambda (x y) (+ (ash x 8) y)) type-vector))

(defun media-type-string-to-int (type-string)
  (media-type-vector-to-int (map 'vector #'char-code type-string)))


(defun find-box-type (type box-list)
  (find (media-type-vector type)
        box-list
        :key #'box-type
        :test #'equalp))

(defgeneric find-child (node type))
(defgeneric find-ancestor (node type))

;;;
(defun skip-n-bytes (stream n)
  (when (plusp n)
    (file-position stream (+ (file-position stream) n))))

;;; 
(defclass iso-container ()
  ((children :accessor children :initarg :children)))

(defmethod print-object ((object iso-container) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (children) object
      (format stream ":children ~a" children))))

(defmethod find-child ((node iso-container) type)
  (find (media-type-vector type)
        (children node)
        :key #'box-type
        :test #'equalp))

(defmethod find-ancestor ((node iso-container) type)
  (declare (ignore type))
  nil)

;;; basic box class
(define-binary-type raw-bytes (size)
  (:reader (in)
    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf))
  (:writer (out buf)
    (write-sequence buf out)))

(define-binary-type skippable-raw-bytes (size predicate)
  (:reader (in)
           (if (funcall predicate)
               (let ((buf (make-array size :element-type '(unsigned-byte 8))))
                 (read-sequence buf in)
                 buf)
               (skip-n-bytes in size)))
  (:writer (out buf)
           (if (funcall predicate)
               (write-sequence buf out)
               (skip-n-bytes out size))))

(define-binary-type u8 () (unsigned-integer :bytes 8 :bits-per-byte 8))

(define-binary-type optional (type if)
  (:reader (in)
    (when if (read-value type in)))
  (:writer (out value)
    (when if (write-value type out value))))

(defun large-size-p (size) (= size 1))

(define-tagged-binary-class bbox ()
  ((size u4)
   (box-type (raw-bytes :size 4))
   (large-size (optional :type 'u8 :if (large-size-p size)))
   (user-type (optional :type '(raw-bytes :size 16)
                        :if (equalp box-type (media-type-vector "uuid")))))
  (:dispatch (find-box-class box-type)))

(defmethod print-object ((object bbox) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size size)
                 (box-type box-type)) object
      (format stream "~s :size ~d" (media-type-string box-type) size))))


(define-binary-class full-bbox-header (bbox)
  ((version u1)
   (flags u3)))

(define-binary-class full-bbox (full-bbox-header)
  ((data (skippable-raw-bytes :size (- size
                                       8
                                       (if large-size 4 0)
                                       (if user-type 16 0)
                                       4)
                              :predicate #'(lambda () (constantly nil))))))

(define-binary-class data-bbox (bbox)
  ((data (raw-bytes :size (- size
                             8
                             (if large-size 4 0)
                             (if user-type 16 0))))))

(defun read-boxes2 (stream limit)
  (loop with bytes-read = 0
     while (or (not limit) (< bytes-read limit))
     for child = (handler-case 
                     (read-value 'bbox stream)
                   (end-of-file () nil))
     when child do
     (incf bytes-read (size child))
       while child collect child))

(define-binary-type box-list (limit)
  (:reader (in)
           (read-boxes2 in limit))
  (:writer (out value)
           (declare (ignore out value))
           (error "not written yet!")))

(defmethod find-child ((node bbox) type)
  (find (media-type-vector type)
        (children node)
        :key #'box-type
        :test #'equalp))

(define-binary-class full-container-bbox (full-bbox-header)
  ((children (box-list :limit (- size 8
                                 (if large-size 4 0)
                                 (if user-type 16 0)
                                 4)))))

(define-binary-class meta-bbox (full-container-bbox) ())

(define-binary-class handler-bbox (full-bbox-header)
  ((pre-defined u4)
   (handler-type u4)
   (reserved-1 u4)
   (reserved-2 u4)
   (reserved-3 u4)
   (data (skippable-raw-bytes :size (- size
                                       8
                                       (if large-size 4 0)
                                       (if user-type 16 0)
                                       4 20) 
                              :predicate #'(lambda () (constantly nil))))))

(define-binary-class container-bbox (bbox)
  ((children (box-list :limit (- size 8
                                 (if large-size 4 0)
                                 (if user-type 16 0))))))


(define-binary-class sample-description-bbox (full-bbox-header)
  ((entry-count u4)
   (children (box-list :limit (- size 8
                                 (if large-size 4 0)
                                 (if user-type 16 0)
                                 4 4)))))

(defparameter *read-movie-data* nil)

(define-binary-class movie-data-bbox (bbox)
  ((data (skippable-raw-bytes :size (- size
                                       8
                                       (if large-size 4 0)
                                       (if user-type 16 0))
                              :predicate #'(lambda () *read-movie-data*)))))

(define-binary-class apple-data-bbox (full-bbox-header)
  ((pad u4)
   (data (raw-bytes :size (- size
                             8
                             (if large-size 4 0)
                             (if user-type 16 0)
                             4 4)))))

(defun find-box-class (box-type)
  (or (gethash box-type *bbox-type-hash*)
      'data-bbox))

(defun read-iso-media-stream2 (stream)
  (let ((container (make-instance 'iso-container)))
    (setf (children container)
          (read-boxes2 stream nil))
    container))

(defun read-iso-media-file2 (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream2 stream)))

(defparameter *copyright-symbol-string* #.(string (code-char 169)))

(defparameter *bbox-type-hash* (make-hash-table :test 'equalp))
(progn 
  (clrhash *bbox-type-hash*)
  (map nil (lambda (x)
             (destructuring-bind (type class) x
               (setf (gethash (media-type-vector type) *bbox-type-hash*) class)))
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
         ("mvhd" full-bbox)
         ("tkhd" full-bbox)
         ("mdhd" full-bbox)

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

;;;
;;; functions to access data in iso-containers
(defun audio-sample-type (iso-container)
  (media-type-string
   (box-type
    (first
     (children
      (reduce #'find-child
              (list
               iso-container
               "moov" "trak" "mdia" "minf" "stbl" "stsd")))))))

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

