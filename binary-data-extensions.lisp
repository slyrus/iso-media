
;; extensions  to gigamonkey's  binary-data package  that are  used by
;; iso-media but which, in theory, might be useful in other contexts.

(cl:in-package #:iso-media)

;;; [nb: I'm not sure this is the best name for this type, but, IIRC,
;;; binary-data has no built-in type for 8-bit bytes]
;;;
;;; raw-bytes
(define-binary-type raw-bytes (size)
  (:reader (in)
    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf))
  (:writer (out buf)
    (write-sequence buf out)))

;;; skippable raw-bytes
(defun skip-n-bytes (stream n)
  (when (plusp n)
    (file-position stream (+ (file-position stream) n))))

;;; if (funcall predicate) is true, then we read/write the sequence,
;;; otherwise, we skip size bytes in the stream.
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

;;; 8-byte unsigned integer
(define-binary-type u8 () (unsigned-integer :bytes 8 :bits-per-byte 8))

(defmethod read-value ((type list) in &rest args)
  (apply #'read-value (first type) in (append (rest type) args)))

(defmethod write-value ((type list) out value &rest args)
  (apply #'write-value (first type) out value (append (rest type) args)))

(define-binary-type optional (type if)
  (:reader (in)
           (when if (read-value type in)))
  (:writer (out value)
           (when if (write-value type out value))))

(define-binary-type dynamic (choose)
  (:reader (in)
           (read-value choose in))
  (:writer (out value)
           (write-value choose out value)))
;;; arrays
(define-binary-type array (type size)
  (:reader (in)
           (let ((arr (make-array size :element-type type)))
             (dotimes (i size)
               (setf (elt arr i)
                     (read-value type in)))
             arr))
  (:writer (out value)
           (dotimes (i (length value))
             (write-value type out (elt value i)))))

;;; signed integers
(defun convert-to-signed-integer (num bits)
  (let ((max (1- (ash 1 (1- bits)))))
    (if (> num max)
        (lognot (- (1- (ash 1 bits)) num))
        num)))

(defun convert-to-unsigned-integer (num bits)
  (if (minusp num)
      (+ (ash 1 bits) num)
      num))

(define-binary-type signed-integer (bytes bits-per-byte)
  (:reader (in)
           (convert-to-signed-integer
            (loop with value = 0
               for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
               (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
               finally (return value))
            (* bytes bits-per-byte)))
  (:writer (out value)
           (let ((value (convert-to-unsigned-integer value (* bytes bits-per-byte))))
             (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte
                do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))))

(define-binary-type s1 () (signed-integer :bytes 1 :bits-per-byte 8))
(define-binary-type s2 () (signed-integer :bytes 2 :bits-per-byte 8))
(define-binary-type s4 () (signed-integer :bytes 4 :bits-per-byte 8))

#+nil
(define-binary-type f4 ()
  (:reader (in)
           (ieee-floats:decode-float32 (read-value 'u4 in)))
  (:writer (out value)
           (write-value 'u4 out (ieee-floats:encode-float32 value))))

#+nil
(define-binary-type f8 ()
  (:reader (in)
           (ieee-floats:decode-float64 (read-value 'u8 in)))
  (:writer (out value)
           (write-value'u8 out (ieee-floats:encode-float64 value))))

