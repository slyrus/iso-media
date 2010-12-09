
(cl:defpackage #:iso-media
  (:use #:cl)
  (:export #:box
           #:box-type
           #:box-size
           #:box-data
           #:make-box
           
           #:read-n-bytes
           #:read-32-bit-int

           #:media-type-string
           #:media-type-vector
           #:media-type-vector-to-int
           #:media-type-string-to-int

           #:find-box-type

           #:read-box-info
           #:read-box-data
           #:read-box
           #:read-boxes
           
           #:do-iso-media-stream
           #:do-iso-media-file
           
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

(defclass box ()
  ((box-size :accessor box-size :initarg :box-size)
   (box-type :accessor box-type :initarg :box-type)
   (box-data :accessor box-data :initarg :box-data)))

(defun make-box (size type data)
  (make-instance 'box
                 :box-size size
                 :box-type type
                 :box-data data))

(defmethod print-object ((object box) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots ((size box-size)
                 (type box-type)) object
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

(defun read-box-raw (stream)
  (destructuring-bind (box-size box-type)
      (read-box-info stream)
    (when box-size
      (let ((box-data (read-box-data-bytes (- box-size 8) stream)))
        (make-box box-size box-type box-data)))))

(defun read-iso-media-stream-raw (stream)
  (loop for box = (read-box-raw stream)
     while box
     collect box))

(defun do-iso-media-stream (stream fn)
  (loop for (size type) = (read-box-info stream)
     while (and size (plusp size)) 
     collect (funcall fn size type stream)))

(defun do-iso-media-file (file fn)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (do-iso-media-stream stream fn)))

(defun read-iso-media-stream-boxes (stream limit &optional acc)
  (if (plusp limit)
      (let ((box (read-box stream)))
        (read-iso-media-stream-boxes stream
                                     (- limit (box-size box))
                                     (cons box acc)))
      acc))

(defparameter *box-container-types*
  (map 'vector #'media-type-vector
       '("moov" "trak" "mdia" "minf" "stbl")))

(defun media-box-container-type-p (type)
  (find type *box-container-types* :test 'equalp))

(defparameter *box-type-vector-hash* (make-hash-table :test 'equalp))
(defparameter *box-type-string-hash* (make-hash-table :test 'equalp))

(map nil (lambda (x)
       (let ((vec (media-type-vector x)))
         (setf (gethash (media-type-vector x) *box-type-vector-hash*)
               vec)
         (setf (gethash x *box-type-string-hash*)
               vec)))
     '("stsd"))

(defgeneric %read-box (type-dispatch type size stream))

(defmethod %read-box ((type-dispatch (eql (gethash "stsd" *box-type-string-hash*)))
                                type size stream)
  (make-box size type (read-box-data-bytes (- size 8) stream)))

(defmethod %read-box ((type-dispatch (eql :container)) type size stream)
  (make-box size type (nreverse (read-iso-media-stream-boxes stream (- size 8)))))

(defun read-box-data (size type stream)
  (let ((disp (gethash type *box-type-vector-hash*)))
    (cond (disp
           (%read-box disp type size stream))
          ((media-box-container-type-p type)
           (%read-box :container type size stream))
          (t (make-box size type (read-box-data-bytes (- size 8) stream))))))

(defun read-box (stream)
  (destructuring-bind (box-size box-type)
      (read-box-info stream)
    (when box-size
      (read-box-data box-size box-type stream))))

(defun read-iso-media-stream (stream)
  (do-iso-media-stream
      stream
    (lambda (size type stream)
      (read-box-data size type stream))))


(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))
