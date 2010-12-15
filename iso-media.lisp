
;;
;; reading ISO media files
;; spec can be found here: http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip
;;

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

(defun large-size-p (size) (= size 1))

(define-tagged-binary-class bbox ()
  ((size u4)
   (box-type (raw-bytes :size 4))
   (large-size (optional :type 'u8 :if (large-size-p size)))
   (user-type (optional :type '(raw-bytes :size 16)
                        :if (equalp box-type (media-type-vector "uuid")))))
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
      (format stream "~s :size ~d" (media-type-string box-type) size))))

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
  (find (media-type-vector type)
        (children node)
        :key #'box-type
        :test #'equalp))

(defmethod find-ancestor ((node iso-container) type)
  (declare (ignore type))
  nil)

(defmethod find-child ((node bbox) type)
  (find (media-type-vector type)
        (children node)
        :key #'box-type
        :test #'equalp))

(define-binary-class full-container-bbox (full-bbox-header)
  ((children (box-list :limit (data-size (current-binary-object))))))

(define-binary-class meta-bbox (full-container-bbox) ())




(define-binary-class handler-bbox (full-bbox-header)
  ((pre-defined u4)
   (handler-type u4)
   (reserved-1 u4)
   (reserved-2 u4)
   (reserved-3 u4)
   (data (skippable-raw-bytes :size (data-size (current-binary-object)) 
                              :predicate #'(lambda () (constantly nil))))))

(defmethod header-size + ((obj handler-bbox)) 20)

(define-binary-class container-bbox (bbox)
  ((children (box-list :limit (data-size (current-binary-object))))))

(define-binary-class sample-description-bbox (full-bbox-header)
  ((entry-count u4)
   (children (box-list :limit (data-size (current-binary-object))))))

(defmethod header-size + ((obj sample-description-bbox)) 4)

(defparameter *read-movie-data* nil)

(define-binary-class movie-data-bbox (bbox)
  ((data (skippable-raw-bytes :size (data-size (current-binary-object))
                              :predicate #'(lambda () *read-movie-data*)))))

(define-binary-class apple-data-bbox (full-bbox-header)
  ((pad u4)
   (data (raw-bytes :size (data-size (current-binary-object))))))

(defmethod header-size + ((obj apple-data-bbox)) 4)

(defun read-iso-media-stream (stream)
  (let ((container (make-instance 'iso-container)))
    (setf (children container)
          (read-boxes stream nil))
    container))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))

(defun read-it (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'iso-container in)))

(defun write-it (file obj)
  (with-open-file (out file 
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-value 'iso-container out obj)))


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
(defun find-box-class (box-type)
  (or (gethash box-type *bbox-type-hash*)
      'data-bbox))

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

