
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


#+nil 
(defparameter *cc*
  (read-iso-media-file
   #p"/Volumes/iTunes_music/Archive/The Clash/Live_ From Here to Eternity/01 Complete Control.m4a"))

(defparameter *cc*  (read-iso-media-file *test-file*))

(find-child (find-child *cc* "moov") "mvhd")

(map 'string #'code-char
     (box-data
      (reduce #'find-child (list *cc* "moov" "udta" "meta" "ilst" (concatenate 'string iso-media::*copyright-symbol-string* "ART") "data"))))


(let ((file #p"/Volumes/iTunes_music/Archive/The Clash/Live_ From Here to Eternity/01 Complete Control.m4a"))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (iso-media::read-iso-media-stream stream)))

(let ((file *test-file*))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (children
     (iso-media::read-iso-media-stream stream))))


(map 'string #'code-char
     (box-data
      (reduce #'find-child (list *q* "moov" "udta" "meta" "ilst" (concatenate 'string iso-media::*copyright-symbol-string* "ART") "data"))))


(reduce #'find-child (list *q* "moov" "udta" "meta"))


(map 'list (lambda (x)
             (map 'string #'code-char (box-type x)))
     (children (find-child *q* "moov")))
