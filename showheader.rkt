;;
;; Command-line tool to show ELF header
;; (similar to readelf -h)
;;

#lang racket

(require "constants.rkt")
(require "elf.rkt")

(define (show-id id)
  (printf "Class: ~a - Version: ~a\n"
          (option-description (elf:id-class id))
          (elf:id-version id))
  (printf "Encoding: ~a\n"
          (option-description (elf:id-encoding id)))
  (printf "ABI: ~a - ABI Version: ~a\n"
          (option-description (elf:id-abi id))
          (elf:id-abi-version id)))

(define (show-header hdr)
  (show-id (elf:header-id hdr))
  (printf "Type: ~a\n" (option-description (elf:header-type hdr)))
  (printf "Machine: ~a\n" (option-description (elf:header-machine hdr)))
  (printf "Entry point: 0x~a\n" (~r (elf:header-entry hdr) #:base 16))
  (printf "Program header offset: ~a\n" (elf:header-prog-header-off hdr))
  (printf "Section header offset: ~a\n" (elf:header-sec-header-off hdr))
  (printf "Flags: ~a\n" (elf:header-flags hdr))
  (printf "ELF header size: ~a\n"  (elf:header-size hdr))
  (printf "Program header size: ~a\n" (elf:header-ph-size hdr))
  (printf "# of program headers: ~a\n" (elf:header-ph-num hdr))
  (printf "Section header size: ~a\n" (elf:header-sh-size hdr))
  (printf "# of section headers: ~a\n" (elf:header-sh-num hdr))
  (printf "Index of string table with section names: ~a\n"
          (elf:header-sec-names-index hdr)))

(module+ main
  (when (< (vector-length (current-command-line-arguments)) 1)
    (error "The file name must be specified at the command line"))

  (define path (vector-ref (current-command-line-arguments) 0))
  (define-values (f h) (elf:open-file path))
  (show-header h)
  (close-input-port f))
