;;
;; Command-line tool to show ELF header
;; (similar to readelf -h)
;;

#lang racket

(require "elf.rkt")

(define (show-header hdr)
  (displayln (format "Class: ~a - Version: ~a - ABI: ~a" 0 0 0)))

(module+ main
  (when (< (vector-length (current-command-line-arguments)) 1)
    (error "The file name must be specified at the command line")))
