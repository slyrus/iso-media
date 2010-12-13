
(cl:defpackage #:iso-media
  (:use #:cl)
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
           
           #:audio-sample-type))

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
(defclass box ()
  ((box-parent :accessor box-parent :initarg :box-parent)
   (box-size :accessor box-size :initarg :box-size)
   (box-large-size :accessor box-large-size :initarg :box-large-size)
   (box-type :accessor box-type :initarg :box-type)
   (box-user-type :accessor box-user-type :initarg :box-user-type)))

(defmethod print-object ((object box) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size box-size)
                 (type box-type)) object
      (format stream "~s :size ~d" (media-type-string type) size))))

(defun make-box (size type &key (class 'box) parent user-type)
  (make-instance class
                 :box-parent parent
                 :box-size size
                 :box-type type
                 :box-user-type user-type))

(defmethod find-ancestor ((box box) (type vector))
  (let ((anc (box-parent box)))
    (when anc
      (if (equalp (box-type anc) type)
          anc
          (find-ancestor anc type)))))

(defmethod find-ancestor ((box box) (type string))
  (find-ancestor box (media-type-vector type)))

;;; boxes with children
(defclass container-box (box iso-container) ())

(defmethod find-child ((box box) type)
  (find-box-type type (children box)))

;;; boxes with data
(defclass data-box (box)
  ((box-data :accessor box-data :initarg :box-data)))

;;; "full" box as per the spec
(defclass full-box (box)
  ((box-version :accessor box-version :initarg :box-version)
   (box-flags :accessor box-flags :initarg :box-flags)))

(defclass sample-description-box (full-box container-box)
  ((box-entry-count :accessor box-entry-count :initarg :box-entry-count)))

(defclass movie-data-box (data-box)
  ((box-data-position :accessor box-data-position :initarg :box-data-position)))

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

(defun skip-n-bytes (stream n)
  (when (plusp n)
    (file-position stream (+ (file-position stream) n))))

(defun read-32-bit-int (stream)
  (multiple-value-bind (buf bytes-read) 
      (read-n-bytes stream 4)
    (when (= bytes-read 4)
      (reduce (lambda (x y) (+ (ash x 8) y)) buf))))

;;
;; reading ISO media files
;; spec can be found here: http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip
;;

(defparameter *read-movie-data* nil)

;;; machinery for reading boxes
(defparameter *box-type-hash* (make-hash-table :test 'equalp))
(progn 
  (clrhash *box-type-hash*)
  (map nil (lambda (x)
             (destructuring-bind (type class) x
               (setf (gethash (media-type-vector type) *box-type-hash*) class)))
       '(("moov" container-box)
         ("trak" container-box)
         ("mdia" container-box)
         ("minf" container-box)
         ("stbl" container-box)
         ("stsd" sample-description-box)
         ("mdat" movie-data-box))))

(defgeneric read-box-data (box stream pos))
(defgeneric read-box-children (box stream pos))

(defmethod read-box-data ((box box) stream pos)
  (setf (box-data box)
        (read-n-bytes stream (- (box-size box) pos)))
  (box-size box))

(defmethod read-box-data ((box movie-data-box) stream pos)
  (setf (box-data-position box) (file-position stream))
  (if *read-movie-data*
      (call-next-method)
      (progn
        (skip-n-bytes stream (- (box-size box) pos))
        (box-size box))))

(defmethod read-box-data ((box full-box) stream pos)
  (setf (box-version box) (read-byte stream))
  (setf (box-flags box) (make-array 3
                                    :initial-contents (list (read-byte stream)
                                                            (read-byte stream)
                                                            (read-byte stream))))
  (call-next-method box stream (+ pos 4)))


(defmethod read-box-data ((box sample-description-box) stream pos)
  (setf (box-version box) (read-byte stream))
  (setf (box-flags box) (make-array 3 :initial-contents (list (read-byte stream)
                                                              (read-byte stream)
                                                              (read-byte stream))))
  (setf (box-entry-count box) (read-32-bit-int stream))
  (setf (children box)
        (loop for i below (box-entry-count box)
           collect (read-next-box stream box)))
  (box-size box))

(defmethod read-box-children ((box box) stream pos)
  pos)

(defmethod read-box-children ((box container-box) stream pos)
  (multiple-value-bind (children cpos)
      (read-boxes stream box (- (box-size box) pos))
    (setf (children box) children)
    (+ pos cpos)))

(defun read-box-header (stream)
  "reads the header of a box and returns a list of values: box-size,
box-type, box-large-size, box-user-type and the number of bytes read
in the header (so far)."
  (declare (optimize (debug 3)))
  (let* ((box-size (read-32-bit-int stream))
         (box-type (read-n-bytes stream 4))
         (pos 8))
    (when box-size
       (destructuring-bind (box-size box-large-size pos)
          (cond ((= box-size 1)
                 (list box-size
                       (+ (* (read-32-bit-int stream) #xffffffff)
                          (read-32-bit-int stream))
                       (+ pos 8)))
                (t (list box-size nil pos)))
        (if (equalp box-type (media-type-vector "uuid"))
            (list box-size box-type box-large-size (read-n-bytes stream 16) (+ pos 16))
            (list box-size box-type box-large-size nil pos))))))

(defun read-boxes (stream parent &optional limit)
  (let ((pos 0))
    (values (loop
               while (or (not limit) (< pos limit))
               for box = (let ((box-info (read-box-header stream)))
                           (when box-info
                             (destructuring-bind (box-size box-type box-large-size box-user-type bpos)
                                 box-info
                               (let* ((class (or (gethash box-type *box-type-hash*) 'data-box))
                                      (box-size (or box-large-size box-size))
                                      (box (apply #'make-box box-size box-type :class class :parent parent
                                                  (when box-user-type (list :box-user-type box-user-type)))))
                                 (let ((pos (read-box-data box stream bpos)))
                                   (let ((pos (read-box-children box stream pos)))
                                     (skip-n-bytes stream (- box-size pos))))
                                 box))))
               while box 
               do (incf pos (box-size box))
               collect box)
            pos)))

;;; reading streams and files
(defun read-iso-media-stream (stream)
  (let ((container (make-instance 'iso-container)))
    (setf (children container)
          (read-boxes stream container))
    container))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))

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
