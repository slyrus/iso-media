
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
   #p"/Users/sly/Music/iTunes/iTunes Music/The Clash/Live At Shea Stadium/02 London Calling \\[Live].m4a"
   #+nil #p"/Volumes/iTunes_music/Archive/The Clash/Live_ From Here to Eternity/01 Complete Control.m4a"))

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
     (data
      (reduce #'find-child (list *cc* "moov" "udta" "meta" "ilst" (concatenate 'string iso-media::*copyright-symbol-string* "ART") "data"))))


(reduce #'find-child (list *cc* "moov" "udta" "meta"))

(map 'list (lambda (x)
             (box-type x))
     (children (find-child *cc* "moov")))

(let ((file #p"/Users/sly/Music/iTunes/iTunes Music/The Clash/Live At Shea Stadium/02 London Calling \\[Live].m4a"))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (iso-media::read-iso-media-stream stream)))


(defparameter *moose* (make-instance 'iso-container :children nil))
(push (make-instance 'iso-media::data-bbox :box-type "bogs"
                     :size (+ 8 12)
                     :data (map 'vector #'char-code "012345678901")
                     :large-size nil
                     :user-type nil)
      (children *moose*))

(defclass foo1 ()
  ((slot1 :accessor slot1 :initarg :slot1 :initform nil)))

(defclass foo2 ()
  ((slot2 :accessor slot2 :initarg :slot2 :initform nil)))

(defclass moose (foo1 foo2)
  ((slot3 :accessor slot3 :initarg :slot3 :initform nil)))

(defgeneric test-it (obj)
  (:method-combination progn :most-specific-last))

(defmethod test-it progn ((obj foo1))
  (print 'ive-got-a-foo1))

(defmethod test-it progn ((obj foo2))
  (print 'ive-got-a-foo2))

(defmethod test-it progn ((obj moose))
  (print 'ive-got-a-moose))

(defparameter *moose* (make-instance 'moose))



;;;

(with-open-file (out #p"/tmp/bar" :direction :output :if-exists :supersede)
  (write-string "01234567" out))

(binary-data:define-binary-class a ()
  ((a-slot com.gigamonkeys.binary-data.common-datatypes:iso-8859-1-char)))

(binary-data:define-binary-class b ()
  ((b-slot com.gigamonkeys.binary-data.common-datatypes:iso-8859-1-char)))

(binary-data:define-binary-class c (a)
  ((c-slot com.gigamonkeys.binary-data.common-datatypes:iso-8859-1-char)))

(compute-applicable-methods #'binary-data::write-object (list (make-instance 'c) t))

(describe
 (make-instance 'c :a-slot #\a :c-slot #\c))

(describe
 (with-open-file (in #p"/tmp/bar" :element-type '(unsigned-byte 8))
   (binary-data:read-value 'c in)))


(binary-data:define-binary-class d (a b)
  ((d-slot com.gigamonkeys.binary-data.common-datatypes:iso-8859-1-char)))

(compute-applicable-methods #'binary-data::write-object (list (make-instance 'd) t))

(describe
 (with-open-file (in #p"/tmp/bar" :element-type '(unsigned-byte 8))
   (binary-data:read-value 'd in)))

(let ((obj (make-instance 'd :a-slot #\0 :b-slot #\1 :d-slot #\2)))
  (with-open-file (in #p"/tmp/moose" :element-type '(unsigned-byte 8)
                      :direction :output :if-exists :supersede)
    (binary-data:write-value 'd in obj)))


(binary-data:define-binary-class array-test ()
  ((foo (iso-media::array
         :type 'com.gigamonkeys.binary-data.common-datatypes:iso-8859-1-char
         :size 256))))

(let ((obj (make-instance 'array-test)))
  (setf (foo obj) (make-array 256 :initial-element #\z))
  (with-open-file (in #p"/tmp/moose" :element-type '(unsigned-byte 8)
                      :direction :output :if-exists :supersede)
    (binary-data:write-value 'array-test in obj)))

(iso-media:write-iso-media-file "/tmp/foo3.mp4" *cc*)


(defparameter *pc*
  (read-iso-media-file
   #P "/Volumes/iTunes_music/Archive/Podcasts/Alan Watts Podcast/Buddhism as Dialogue #1.m4a"))

(defparameter *sc*
  (read-iso-media-file
   #P"/Volumes/iTunes_music/Archive/1998, August 8, Merriweather Post Pavillion, Columbia, MD/1998, August 8, Merriweather Post Pavillion, Columbia, MD/1-02 Shafty.m4a"))


(cl:defpackage #:iso-media-test (:use #:cl #:iso-media))

(cl:in-package #:iso-media-test)

(defparameter *tears-of-rage*
  (iso-media:read-iso-media-file "/Users/sly/projects/iso-media/test/tears-of-rage.m4a"))

(setf (album-artist *tears-of-rage*) "The Band")

(write-iso-media-file "/Users/sly/projects/iso-media/test/tears-of-rage-test.m4a" *tears-of-rage*)

(defparameter *tears-of-rage-test*
  (iso-media:read-iso-media-file "/Users/sly/projects/iso-media/test/tears-of-rage-test.m4a"))

(album-artist *tears-of-rage-test*)
