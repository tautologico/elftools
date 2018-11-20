;;
;; ELF file structures
;;

#lang racket

(provide elf:read
         (struct-out elf:header)
         (struct-out elf:id)
         (struct-out elf:sec-header)
         (struct-out elf:prog-header)
         (prefix-out elf: open-file)
         (prefix-out elf: read-header)
         (prefix-out elf: read-section-header)
         (prefix-out elf: read-section)
         (prefix-out elf: section-name)
         (prefix-out elf: read-program-header)
         (prefix-out elf: read-segment))

(require "constants.rkt")

(module+ test
  (require rackunit))

;; ELF header identification
(struct elf:id
  (class encoding version abi abi-version) #:transparent)

;; ELF header
(struct elf:header
  (id type machine version entry
   prog-header-off sec-header-off flags
   size ph-size ph-num
   sh-size sh-num sec-names-index)
  #:transparent)

;; Test if the file is big endian based on header ident
(define (big-endian? ident)
  (eq? (option-symbol (elf:id-encoding ident))
       'big-endian))

(define (64-bit? ident)
  (eq? (option-symbol (elf:id-class ident)) '64-bit))

;; Read an Elf32_Half data item
(define (read-half in ident)
  (define bs (read-bytes 2 in))
  (integer-bytes->integer bs #f (big-endian? ident)))

;; Read an Elf32_Word data item
(define (read-word in ident)
  (define bs (read-bytes 4 in))
  (integer-bytes->integer bs #f (big-endian? ident)))

;; Read an address depending on byte order and word-size
(define (read-address in ident)
  (define size (if (64-bit? ident) 8 4))
  (define bs (read-bytes size in))
  (integer-bytes->integer bs #f (big-endian? ident)))

(define (check-elf-magic? in)
  (define bs (read-bytes 4 in))
  (equal? bs #"\177ELF"))

(define (read-header-ident in)
  (define class-byte   (read-byte in))
  (define order-byte   (read-byte in))
  (define version-byte (read-byte in))
  (define abi          (read-byte in))
  (define abi-version  (read-byte in))
  (read-bytes 7 in)   ; discard padding bytes in identification
  (elf:id (integer->option file-class-map class-byte)
          (integer->option data-encoding-map order-byte)
          version-byte
          (integer->option abi-map abi)
          abi-version))

;; TODO: processor-specific flags
(define (read-header in)
  (unless (check-elf-magic? in) (error "Invalid ELF magic number: not an ELF file"))
  (define ident   (read-header-ident in))
  (define type    (integer->option type-map    (read-half in ident)))
  (define machine (integer->option machine-map (read-half in ident)))
  (define version (read-word in ident))
  (define entry   (read-address in ident))
  (define phoff   (read-address in ident))
  (define shoff   (read-address in ident))
  (define flags   (read-word in ident))
  (define size    (read-half in ident))
  (define ph-size (read-half in ident))
  (define ph-num  (read-half in ident))
  (define sh-size (read-half in ident))
  (define sh-num  (read-half in ident))
  (define index   (read-half in ident))
  (elf:header ident type machine version entry phoff shoff flags
              size ph-size ph-num sh-size sh-num index))


;; --- Section headers -------------------------------------

(struct elf:sec-header
  (name type flags addr offset size link info addr-align entry-size)
  #:transparent)

(define (section-header-offset in elf-header index)
  (when (>= index (elf:header-sh-num elf-header))
    (error "invalid index for section header entry: " index))
  (+ (elf:header-sec-header-off elf-header)
     (* (elf:header-sh-size elf-header)
        index)))

(define (read-section-header in elf-header index)
  (define off (section-header-offset in elf-header index))
  (define id  (elf:header-id elf-header))
  (file-position in off)
  (define name   (read-word in id))
  (define type   (integer->option sec-type-map (read-word in id)))
  (define flags  (read-address in id))
  (define addr   (read-address in id))
  (define offset (read-address in id))
  (define size   (read-address in id))
  (define link   (read-word in id))
  (define info   (read-word in id))
  (define align  (read-address in id))
  (define esize  (read-address in id))
  (elf:sec-header name type flags addr offset size link info align esize))

;; read section contents for section identified in sec-hdr,
;; from port in
(define (read-section in sec-hdr)
  (file-position in (elf:sec-header-offset sec-hdr))
  (read-bytes (elf:sec-header-size sec-hdr) in))

;; --- Section names ---------------------------------------

(define section-names #f)

(define (get-string-by-index str-table index)
  (define (find-end i)
    (if (zero? (bytes-ref str-table i))
        i
        (find-end (add1 i))))
  (bytes->string/utf-8 str-table #\X index (find-end index)))

(define (read-section-names in hdr)
  (define str-index (elf:header-sec-names-index hdr))
  (define sec-header (read-section-header in hdr str-index))
  (set! section-names (read-section in sec-header)))

(define (section-name-string-from-index index)
  (get-string-by-index section-names index))

(define (section-name sec-hdr)
  (section-name-string-from-index (elf:sec-header-name sec-hdr)))


;; --- convenience functions -------------------------------

;; Read ELF file from input port in, return the elf header
(define (elf:read in)
  (define h (read-header in))
  (read-section-names in h)
  h)

;; Read ELF file from file path, return the input port and elf header
(define (open-file path)
  (define f (open-input-file path))
  (define h (elf:read f))
  (values f h))

;; --- tests -----------------------------------------------

(module+ test
  (define hdr1-bytes
    (bytes #x7f #x45 #x4c #x46 #x02 #x01 #x01 #x00
           #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
           #x02 #x00 #x3e #x00 #x01 #x00 #x00 #x00
           #x30 #x04 #x40 #x00 #x00 #x00 #x00 #x00
           #x40 #x00 #x00 #x00 #x00 #x00 #x00 #x00
           #xd8 #x19 #x00 #x00 #x00 #x00 #x00 #x00
           #x00 #x00 #x00 #x00 #x40 #x00 #x38 #x00
           #x09 #x00 #x40 #x00 #x1f #x00 #x1c #x00))

  (define hdr1 (call-with-input-bytes hdr1-bytes read-header))

  (check-equal? (elf:id-class (elf:header-id hdr1))
                (symbol->option file-class-map '64-bit))
  (check-equal? (elf:id-encoding (elf:header-id hdr1))
                (symbol->option data-encoding-map 'little-endian))
  (check-equal? (elf:id-version (elf:header-id hdr1))
                1)
  (check-equal? (elf:id-abi (elf:header-id hdr1))
                0)
  (check-equal? (elf:id-abi-version (elf:header-id hdr1))
                0)

  (check-equal? (elf:header-type hdr1) (symbol->option type-map 'executable))
  (check-equal? (elf:header-machine hdr1) (symbol->option machine-map 'x86-64))
  (check-equal? (elf:header-version hdr1)         1)
  (check-equal? (elf:header-entry hdr1)           #x400430)
  (check-equal? (elf:header-prog-header-off hdr1) 64)
  (check-equal? (elf:header-sec-header-off hdr1)  6616)
  (check-equal? (elf:header-flags hdr1)           0)
  (check-equal? (elf:header-size hdr1)            64)
  (check-equal? (elf:header-ph-size hdr1)         56)
  (check-equal? (elf:header-ph-num hdr1)          9)
  (check-equal? (elf:header-sh-size hdr1)         64)
  (check-equal? (elf:header-sh-num hdr1)          31)
  (check-equal? (elf:header-sec-names-index hdr1) 28)

  ;; string table testing
  (define strtable
    (bytes-append
     #"\0.symtab\0.strtab\0.shstrtab\0.interp\0.note.ABI-tag\0"
     #".note.gnu.build-id\0.gnu.hash\0.dynsym\0.dynstr\0.gnu.version\0"
     #".gnu.version_r\0.rela.dyn\0.rela.plt\0.init\0.plt.got\0.text\0"
     #".fini\0.rodata\0.eh_frame_hdr\0.eh_frame\0.init_array\0.fini_array\0"
     #".jcr\0.dynamic\0.got.plt\0.data\0.bss\0.comment\0"))

  (check-equal? (get-string-by-index strtable 0) "")
  (check-equal? (get-string-by-index strtable 1) ".symtab")
  (check-equal? (get-string-by-index strtable 5) "tab")
  (check-equal? (get-string-by-index strtable 27) ".interp")
  (check-equal? (get-string-by-index strtable 49) ".note.gnu.build-id"))

;; --- Program headers -------------------------------------

(struct elf:prog-header
  (type offset vaddr paddr filesz memsz flags align)
  #:transparent)

(define (program-header-offset hdr index)
  (when (>= index (elf:header-ph-num hdr))
    (error "Invalid index for program header: " index))
  (+ (elf:header-prog-header-off hdr)
     (* (elf:header-ph-size hdr)
        index)))

(define (read-program-header in hdr index)
  (define off (program-header-offset hdr index))
  (define id  (elf:header-id hdr))
  (file-position in off)
  (define type (integer->option segment-type-map (read-word in id)))
  (define flags #f)
  (define offset #f)
  (if (64-bit? id)
      (begin
        (set! flags (decode-flags (read-word in id) segment-flags))
        (set! offset (read-address in id)))
      (set! offset (read-word in id)))
  (define vaddr  (read-address in id))
  (define paddr  (read-address in id))
  (define filesz (read-address in id))
  (define memsz  (read-address in id))
  (unless (64-bit? id) (set! flags (decode-flags (read-word in id) segment-flags)))
  (define align  (read-address in id))
  (elf:prog-header type offset vaddr paddr filesz memsz flags align))

(define (read-segment in prog-hdr)
  (file-position in (elf:prog-header-offset prog-hdr))
  (read-bytes (elf:prog-header-filesz prog-hdr) in))
