;;
;; Show information about ELF file
;;

#lang racket

(require racket/gui)

(require "elf.rkt")


;; variables
(define file-path "test")

(define main-win (new frame%
                      [label "ELF Show"]
                      [width 600]
                      [height 400]))

(define (tab-change-callback b e)
  (displayln (format "Changed tab, selected tab: ~a" (send b get-selection))))

(define tab-panel (new tab-panel%
                       [parent main-win]
                       [callback tab-change-callback]
                       [choices (list "General" "Sections" "Segments")]))

(define general-gbox (new group-box-panel%
                          [parent tab-panel]
                          [label "General information"]
                          [alignment '(left top)]))

(define (fill-general-info path gbox)
  (new text-field% [parent gbox] [label "File: "] [init-value path] [enabled #t])
  (new message% [parent gbox] [label "Quo Vadis?"])
  (void))

(define (read-elf in)
  (elf:read-header in))

(define (read-elf-file path)
  (call-with-input-file path read-elf))

(module+ main
  (if (read-elf-file file-path)
      (displayln "ELF file magic ok")
      (displayln "Not an ELF file"))
  (fill-general-info file-path general-gbox)
  (send main-win show #t))

