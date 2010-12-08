
(cl:defpackage #:iso-media
  (:use #:cl)
  (:export #:iso-media-box
           #:iso-media-box-type
           #:iso-media-box-size
           #:iso-media-box-data
           #:make-iso-media-box
           
           #:read-n-bytes
           #:read-32-bit-int

           #:media-type-string
           #:find-box-type

           #:read-iso-media-box-info
           #:read-iso-media-box-data
           #:read-iso-media-box
           #:read-iso-media-boxes
           
           #:do-iso-media-stream
           #:do-iso-media-file
           
           #:read-iso-media-stream
           #:read-iso-media-file))

(cl:in-package #:iso-media)

(defun media-type-string (type-int)
  (map 'string #'code-char type-int))

(defun find-box-type (type box-list)
  (find (map 'vector #'char-code type)
        box-list
        :key #'iso-media-box-type
        :test #'equalp))

(defclass iso-media-box ()
  ((iso-media-box-size :accessor iso-media-box-size :initarg :iso-media-box-size)
   (iso-media-box-type :accessor iso-media-box-type :initarg :iso-media-box-type)
   (iso-media-box-data :accessor iso-media-box-data :initarg :iso-media-box-data)))

(defun make-iso-media-box (size type data)
  (make-instance 'iso-media-box
                 :iso-media-box-size size
                 :iso-media-box-type type
                 :iso-media-box-data data))

(defmethod print-object ((object iso-media-box) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size iso-media-box-size)
                 (type iso-media-box-type)) object
      (format stream "~s :size ~d" (media-type-string type) size))))


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
(defun read-iso-media-box-info (stream)
  ;; FIXME need to handle 64-bit sizes here!!!
  (let* ((box-size (read-32-bit-int stream))
         (box-type (read-n-bytes stream 4)))
    (list box-size box-type)))

;; NOTE!!! remember that the size of the data we want to read is 8
;; less than the size of the box! We could fix that here, but
;; currently we're relying on the caller to make that adjustment!
(defun read-iso-media-box-data (size stream)
  (read-n-bytes stream size))

(defun read-iso-media-box (stream)
  (destructuring-bind (box-size box-type)
      (read-iso-media-box-info stream)
    (when box-size
      (let ((box-data (read-iso-media-box-data (- box-size 8) stream)))
        (make-iso-media-box box-size box-type box-data)))))

(defun do-iso-media-stream (stream fn)
  (loop for (size type) = (read-iso-media-box-info stream)
     while (and size (plusp size)) 
     collect (funcall fn size type stream)))

(defun do-iso-media-file (file fn)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (do-iso-media-stream stream fn)))

(defun read-iso-media-stream-boxes (stream limit &optional acc)
  (if (plusp limit)
      (let ((box (read-iso-media-box stream)))
        (read-iso-media-stream-boxes stream
                                     (- limit (iso-media-box-size box))
                                     (cons box acc)))
      acc))

(defun read-iso-media-stream (stream)
  (do-iso-media-stream
      stream
    (lambda (size type stream)
      (cond
        ((equalp type (map 'vector #'char-code "moov"))
         (make-iso-media-box size
                             type
                             (nreverse (read-iso-media-stream-boxes stream (- size 8)))))
        (t 
         (make-iso-media-box size type (read-iso-media-box-data (- size 8) stream)))))))

(defun read-iso-media-stream-raw (stream)
  (loop for box = (read-iso-media-box stream)
     while box
     collect box))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))
