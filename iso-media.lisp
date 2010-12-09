
(cl:defpackage #:iso-media
  (:use #:cl)
  (:export #:box
           #:box-type
           #:box-size
           #:box-children
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

           #:read-box-info
           #:read-box-data
           #:read-box
           #:read-boxes
           
           #:read-iso-media-stream
           #:read-iso-media-file))

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
(defclass iso-file ()
  ((iso-file-children :accessor iso-file-children :initarg :iso-file-children)))

(defmethod find-child ((node iso-file) type)
  (find (media-type-vector type)
        (iso-file-children node)
        :key #'box-type
        :test #'equalp))

;;; basic box class
(defclass box ()
  ((box-parent :accessor box-parent :initarg :box-parent)
   (box-size :accessor box-size :initarg :box-size)
   (box-type :accessor box-type :initarg :box-type)))

(defmethod print-object ((object box) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size box-size)
                 (type box-type)) object
      (format stream "~s :size ~d" (media-type-string type) size))))

(defun make-box (size type &key (class 'box) parent)
  (make-instance class
                 :box-parent parent
                 :box-size size
                 :box-type type))

(defmethod find-ancestor ((box box) (type vector))
  (let ((anc (box-parent box)))
    (when anc
      (if (equalp (box-type anc) type)
          anc
          (%find-ancestor anc type)))))

(defmethod find-ancestor ((box box) (type string))
  (find-ancestor box (media-type-vector type)))

;;; boxes with children
(defclass container-box (box)
  ((box-children :accessor box-children :initarg :box-children)))

(defmethod find-child ((box box) type)
  (find-box-type type (box-children box)))

;;; boxes with data
(defclass data-box (box)
  ((box-data :accessor box-data :initarg :box-data)))

;;; "full" box as per the spec
(defclass full-box (box)
  ((box-version :accessor box-version :initarg :box-version)
   (box-flags :accessor box-flags :initarg :box-flags)))

(defclass sample-description-box (full-box container-box)
  ((box-entry-count :accessor box-entry-count :initarg :box-entry-count)))

(defclass sample-entry-box (box) ())

(defclass audio-sample-entry-box (sample-entry-box) 
  ((channel-count :accessor channel-count :initarg :channel-count)
   (sample-size :accessor sample-size :initarg :sample-size)
   (sample-rate :accessor sample-rate :initarg :sample-rate)))

;;; stream reading utlities
(defun read-n-bytes (stream n)
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((bytes-read (read-sequence buf stream)))
      (values buf bytes-read))))

(defun read-32-bit-int (stream)
  (multiple-value-bind (buf bytes-read) 
      (read-n-bytes stream 4)
    (when (= bytes-read 4)
      (reduce (lambda (x y) (+ (ash x 8) y)) buf))))

;;
;; reading ISO media files
;; spec can be found here: http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip
;;
(defun read-box-info (stream)
  ;; FIXME need to handle 64-bit sizes here!!!
  (let* ((box-size (read-32-bit-int stream))
         (box-type (read-n-bytes stream 4)))
    (list box-size box-type)))

;; NOTE!!! remember that the size of the data we want to read is 8
;; less than the size of the box! We could fix that here, but
;; currently we're relying on the caller to make that adjustment!
(defun read-box-data-bytes (size stream)
  (read-n-bytes stream size))

(defun read-iso-media-stream-boxes (stream limit parent &optional acc)
  (if (plusp limit)
      (let ((box (read-next-box stream parent)))
        (read-iso-media-stream-boxes stream
                                     (- limit (box-size box))
                                     parent
                                     (cons box acc)))
      acc))

;;; machinery for reading boxes
(defparameter *box-type-hash* (make-hash-table :test 'equalp))
(map nil (lambda (x)
           (destructuring-bind (type class) x
             (setf (gethash (media-type-vector type) *box-type-hash*) class)))
     '(("moov" container-box)
       ("trak" container-box)
       ("mdia" container-box)
       ("minf" container-box)
       ("stbl" container-box)
       ("stsd" sample-description-box)))

(defgeneric %read-box (box type size stream))

(defmethod %read-box ((box data-box) type size stream)
  (setf (box-data box) (read-box-data-bytes (- size 8) stream)))

(defmethod %read-box ((box container-box) type size stream)
  (setf (box-children box) (nreverse (read-iso-media-stream-boxes stream (- size 8) box))))

(defmethod %read-box ((box full-box) type size stream)
  (setf (box-version box) (read-byte stream))
  (setf (box-flags box) (make-array 3
                                    :initial-contents (list (read-byte stream)
                                                            (read-byte stream)
                                                            (read-byte stream))))
  (read-n-bytes stream (- size 8 4)))

(defmethod %read-box ((box sample-description-box) type size stream)
  (setf (box-version box) (read-byte stream))
  (setf (box-flags box) (make-array 3
                                    :initial-contents (list (read-byte stream)
                                                            (read-byte stream)
                                                            (read-byte stream))))
  (setf (box-entry-count box) (read-32-bit-int stream))
  (setf (box-children box)
        (loop for i below (box-entry-count box)
           collect (read-next-box stream box))))

(defun read-next-box (stream parent)
  (destructuring-bind (box-size box-type)
      (read-box-info stream)
    (when (and box-size (plusp box-size))
      (let* ((class (or (gethash box-type *box-type-hash*) 'data-box))
             (box (make-box box-size box-type :class class :parent parent)))
        (%read-box box box-type box-size stream)
        box))))

;;; reading streams and files
(defun read-iso-media-stream (stream)
  (make-instance 'iso-file
                 :iso-file-children
                 (loop for box = (read-next-box stream nil)
                    while box 
                    collect box)))
  
(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))
