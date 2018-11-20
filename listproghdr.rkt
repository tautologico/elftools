;;
;; Command-line tool that shows all program (segment) headers
;;

#lang racket

(require "elf.rkt")
(require "constants.rkt")

(define (show-program-header f hdr i)
  (define prog-hdr (elf:read-program-header f hdr i))
  (define type (elf:prog-header-type prog-hdr))
  (printf "=====\n")
  (printf "Segment #~a\n" i)
  (printf "Type          : ~a - ~a\n" (option-value type)
                                  (option-description type))
  (printf "Offset        : ~a\n" (elf:prog-header-offset prog-hdr))
  (printf "Flags         : ~a\n" (elf:prog-header-flags prog-hdr))  ;; TODO flags
  (printf "Virt. Address : ~a\n" (elf:prog-header-vaddr prog-hdr))
  (printf "Phys. Address : ~a\n" (elf:prog-header-paddr prog-hdr))
  (printf "File Size     : ~a\n" (elf:prog-header-filesz prog-hdr))
  (printf "Memory Size   : ~a\n" (elf:prog-header-memsz prog-hdr))
  (printf "Align         : ~a\n" (elf:prog-header-align prog-hdr)))

(define (show-program-headers f hdr)
  (for ([i (in-range (elf:header-ph-num hdr))])
    (show-program-header f hdr i)))

(module+ main
  (when (< (vector-length (current-command-line-arguments)) 1)
    (error "The file name must be specified at the command line"))

  (define path (vector-ref (current-command-line-arguments) 0))
  (define-values (f h) (elf:open-file path))
  (show-program-headers f h)
  (close-input-port f))
