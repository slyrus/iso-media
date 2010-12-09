
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op 'iso-media)
  (asdf:oos 'asdf:load-op 'alexandria))

(cl:defpackage #:iso-media-scratch
  (:use #:cl #:iso-media))

(cl:in-package #:iso-media-scratch)

(defparameter *test-file*
  #P"/Volumes/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")

(let ((file *test-file*))
  (read-iso-media-file file))

(let ((file *test-file*))
  (box-data
   (first
    (remove-if-not (lambda (box)
                     (equalp (box-type box)
                             (map 'vector #'char-code "moov")))
                   (read-iso-media-file file)))))

(let ((file *test-file*))
  (find-box-type "mvhd"
                 (box-data
                  (find-box-type "moov" (read-iso-media-file file)))))

(let ((file *test-file*))
  (find-box-type "trak"
                 (box-data
                  (find-box-type "moov" (read-iso-media-file file)))))


(let ((file *test-file*))
  (map 'list (lambda (x)
               (list (media-type-string (box-type x))
                     (map 'string #'code-char (box-data x))))
       (reduce (lambda (x y)
                 (box-data
                  (find-box-type y x)))
               (list
                (read-iso-media-file file)
                "moov" "trak" "mdia" "minf" "stbl"))))


(let ((file *test-file*))
  (map 'string #'code-char
       (reduce (lambda (x y)
                 (box-data
                  (find-box-type y x)))
               (list
                (read-iso-media-file file)
                "moov" "trak" "mdia" "minf" "stbl" "stsd"))))
