
(asdf:defsystem #:iso-media
  :name "iso-media"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.0.1"
  :licence "BSD"
  :description "A library for reading ISO Media (such as MP4) Files"
  :depends-on (com.gigamonkeys.binary-data)
  :serial t
  :components
  ((:cl-source-file "package")
   (:cl-source-file "binary-data-extensions")
   (:cl-source-file "iso-media")   
   (:module 
    :test
    :components
    ((:static-file "audio-test.mp4")))))



