
iso-media
=========

A library for parsing ISO Media files. ISO Media files include MP4
audio and video files. The specification for ISO Media files can be
found at
[http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip](http://standards.iso.org/ittf/PubliclyAvailableStandards/c041828_ISO_IEC_14496-12_2005(E).zip).

The iso-media library can be found at: [https://github.com/slyrus/iso-media](https://github.com/slyrus/iso-media).

This library is released under a BSD-style license, the text of which
can be found in the LICENSE file accomponying the source code.

Dependencies
============

## binary-data

* github page: [https://github.com/gigamonkey/monkeylib-binary-data](https://github.com/gigamonkey/monkeylib-binary-data)
* git source repository: [git://github.com/gigamonkey/monkeylib-binary-data.git](git://github.com/gigamonkey/monkeylib-binary-data.git)

## alexandria

* common-lisp.net page: [http://common-lisp.net/project/alexandria/](http://common-lisp.net/project/alexandria/)
* git source repository: [http://common-lisp.net/gitweb?p=projects/alexandria/alexandria.git;a=summary](http://common-lisp.net/gitweb?p=projects/alexandria/alexandria.git;a=summary)

alexandria and binary-data are available through quicklisp and it is
the hope of the author that iso-media will be available through
quicklisp in the near future.


Using iso-media
===============

To load iso-media:

    (asdf:load-system 'iso-media)

iso-media Example
=================

    ISO-MEDIA-TEST> (defpackage #:iso-media-test (:use #:cl #:iso-media))
    #<PACKAGE "ISO-MEDIA-TEST">
    ISO-MEDIA-TEST> (in-package #:iso-media-test)
    #<PACKAGE "ISO-MEDIA-TEST">
    ISO-MEDIA-TEST> (defparameter *tears-of-rage*
      (iso-media:read-iso-media-file "/Users/sly/projects/iso-media/test/tears-of-rage.m4a"))
    *TEARS-OF-RAGE*
    ISO-MEDIA-TEST> *tears-of-rage*
    #<ISO-CONTAINER :children (#<DATA-BBOX "ftyp" :size 24>
                               #<MOVIE-DATA-BBOX "mdat" :size 4822003>
                               #<CONTAINER-BBOX "moov" :size 58303>)>

