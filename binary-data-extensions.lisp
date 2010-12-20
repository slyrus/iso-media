
(cl:in-package #:iso-media)

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

(define-binary-type dynamic (choose)
  (:reader (in)
           (read-value choose in))
  (:writer (out value)
           (write-value choose out value)))
