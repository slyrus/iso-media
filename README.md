
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

* github page: https://github.com/slyrus/monkeylib-binary-data/tree/alexandria
* git source repository: git://github.com/slyrus/monkeylib-binary-data.git

## alexandria

* common-lisp.net page: http://common-lisp.net/project/alexandria/
* git source repository: http://common-lisp.net/gitweb?p=projects/alexandria/alexandria.git;a=summary

alexandria is available through quicklisp and it is the hope of the
author that both binary-data and iso-media are available through
quicklisp in the near future.

NOTE
====

The iso-media library depends on a fork of said library that uses the
alexandria library instead of Peter Seibel's monkeylib-macro-utilities
library. Hopefully the core binary-data library will incorporate this
change in the near future.


Cyrus Harmon
ch-lisp@bobobeach.com
