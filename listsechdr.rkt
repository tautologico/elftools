;;
;; Command-line tool that shows all section headers
;;

#lang racket

(require "elf.rkt")
(require "constants.rkt")

(define (show-section-header f hdr i)
  (define sec-hdr (elf:read-section-header f hdr i))
  (printf "=====\n")
  (printf "Section #~a | Name: ~a\n" i (elf:section-name sec-hdr))
  (printf "Type      : ~a - ~a\n" (option-value (elf:sec-header-type sec-hdr))
                                  (option-description (elf:sec-header-type sec-hdr)))
  (printf "Flags     : ~a\n" (elf:sec-header-flags sec-hdr))  ;; TODO flags
  (printf "Address   : ~a\n" (elf:sec-header-addr sec-hdr))
  (printf "Offset    : ~a\n" (elf:sec-header-offset sec-hdr))
  (printf "Size      : ~a\n" (elf:sec-header-size sec-hdr))
  (printf "Link      : ~a\n" (elf:sec-header-link sec-hdr))
  (printf "Info      : ~a\n" (elf:sec-header-info sec-hdr))
  (printf "Align     : ~a\n" (elf:sec-header-addr-align sec-hdr))
  (printf "Entry size: ~a\n" (elf:sec-header-entry-size sec-hdr)))

(define (show-section-headers f hdr)
  (for ([i (in-range (elf:header-sh-num hdr))])
    (show-section-header f hdr i)))

(module+ main
  (when (< (vector-length (current-command-line-arguments)) 1)
    (error "The file name must be specified at the command line"))

  (define path (vector-ref (current-command-line-arguments) 0))
  (define-values (f h) (elf:open-file path))
  (show-section-headers f h)
  (close-input-port f))
