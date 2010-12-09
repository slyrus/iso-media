
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
  (find-child (find-child (read-iso-media-file file) "moov") "mvhd"))

(let ((file *test-file*))
  (find-child (find-child (read-iso-media-file file) "moov") "trak"))


(let ((file *test-file*))
  (iso-media::find-ancestor
   (find-child
    (reduce #'find-child
            (list
             (read-iso-media-file file)
             "moov" "trak" "mdia" "minf" "stbl"))
    "stsd")
   "trak"))


(let ((file *test-file*))
  (reduce #'find-child
          (list
           (read-iso-media-file file)
           "moov" "trak" "mdia" "minf" "stbl")))
