
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op 'iso-media))

(cl:defpackage #:iso-media-scratch
  (:use #:cl #:iso-media))

(cl:in-package #:iso-media-scratch)

(defparameter *test-file*
  (asdf:component-pathname
   (reduce #'asdf:find-component
           (list nil "iso-media" "test" "audio-test.mp4"))))

(let ((file *test-file*))
  (read-iso-media-file file))

(let ((file *test-file*))
  (find-child (read-iso-media-file file) "moov"))

(let ((file *test-file*))
  (children
   (find-child (read-iso-media-file file) "moov")))

(let ((file *test-file*))
  (find-child (find-child (read-iso-media-file file) "moov") "mvhd"))

(let ((file *test-file*))
  (find-child (find-child (read-iso-media-file file) "moov") "trak"))

(let ((file *test-file*))
  (reduce #'find-child
          (list
           (read-iso-media-file file)
           "moov" "trak" "mdia" "minf" "stbl")))

(let ((file *test-file*))
  (children
   (reduce #'find-child
           (list
            (read-iso-media-file file)
            "moov" "trak" "mdia" "minf" "stbl" "stsd"))))

(let ((file *test-file*))
  (find-child
   (find-ancestor
    (find-child
     (reduce #'find-child
             (list
              (read-iso-media-file file)
              "moov" "trak" "mdia" "minf" "stbl"))
     "stsd")
    "minf")
   "smhd"))


(let ((file *test-file*))
  (iso-media::audio-sample-type (read-iso-media-file file)))

