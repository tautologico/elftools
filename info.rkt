#lang info

(define collection "elftools")
(define deps '("base"))
#;(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define build-deps '("racket-doc" "rackunit-lib"))
#;(define scribblings '(("scribblings/rz80.scrbl" ())))
(define pkg-desc "Tools for reading & writing ELF files")
(define version "0.1")
(define pkg-authors '(andrei))
