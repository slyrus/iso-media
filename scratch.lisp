
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
  (iso-media-box-data
   (first
    (remove-if-not (lambda (box)
                     (equalp (iso-media-box-type box)
                             (map 'vector #'char-code "moov")))
                   (read-iso-media-file file)))))

(let ((file *test-file*))
  (find-box-type "mvhd"
                 (iso-media-box-data
                  (find-box-type "moov" (read-iso-media-file file)))))

(let ((file *test-file*))
  (find-box-type "trak"
                 (iso-media-box-data
                  (find-box-type "moov" (read-iso-media-file file)))))


(let ((file *test-file*))
  (map 'list (lambda (x)
               (list (media-type-string (iso-media-box-type x))
                     (map 'string #'code-char (iso-media-box-data x))))
       (iso-media-box-data
        (find-box-type "stbl"
                       (iso-media-box-data
                        (find-box-type "minf"
                                       (iso-media-box-data
                                        (find-box-type "mdia"
                                                       (iso-media-box-data
                                                        (find-box-type "trak"
                                                                       (iso-media-box-data
                                                                        (find-box-type "moov"
                                                                                       (read-iso-media-file file)))))))))))))


(let ((file *test-file*))
  (map 'string #'code-char
       (reduce (lambda (x y)
                 (iso-media-box-data
                  (find-box-type y x)))
               (list
                (read-iso-media-file file)
                "moov" "trak" "mdia" "minf" "stbl" "stsd"))))
