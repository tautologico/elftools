;;
;; ELF file format constants
;;

#lang racket

(provide file-class-map data-encoding-map abi-map type-map machine-map)
(provide sec-type-map)
(provide option-value option-symbol option-description)
(provide integer->option symbol->option)

(struct option-map (int->option symbol->option))

(define-syntax-rule (define-option-map name (i sym str) ...)
  (define name
    (option-map 
     (apply hasheqv (flatten (list (list i (vector i sym str)) ...)))
     (apply hasheqv (flatten (list (list sym (vector i sym str)) ...))))))

(define (make-unknown-option int-val)
  (vector int-val 'unknown "Unknown value"))

;; TODO
;; Integer maps could contain ranges to test for processor-specific
;; or user-defined options
(define (integer->option m i)
  (cond [(hash-has-key? (option-map-int->option m) i)
         (hash-ref (option-map-int->option m) i)]
        [else (make-unknown-option i)]))

(define (decode-integer-flags m i)
  0)

(define (symbol->option m s)
  (hash-ref (option-map-symbol->option m) s))

(define (option-value d)       (vector-ref d 0))
(define (option-symbol d)      (vector-ref d 1))
(define (option-description d) (vector-ref d 2))

(define-option-map file-class-map
  (0 'invalid-class "Invalid class")
  (1 '32-bit        "32-bit")
  (2 '64-bit        "64-bit"))

(define-option-map data-encoding-map
  (0 'invalid-encoding "Invalid encoding")
  (1 'little-endian    "Two's complement, little-endian order")
  (2 'big-endian       "Two's complement, big-endian order"))

(define-option-map abi-map
  (0  'unspecified-abi  "No extensions or unspecified ABI")
  (1  'hpux             "Hewlett-Packard HP-UX")
  (2  'netbsd           "NetBSD")
  (3  'gnu              "GNU")
  (6  'solaris          "Sun Solaris")
  (7  'aix              "AIX")
  (8  'irix             "IRIX")
  (9  'freebsd          "FreeBSD")
  (10 'tru64            "Compaq TRU-64 Unix")
  (11 'modesto          "Novell Modesto")
  (12 'openbsd          "Open BSD")
  (13 'openvms          "Open VMS")
  (14 'nsk              "Hewlett-Packard Non-Stop Kernel")
  (15 'aros             "Amiga Research OS")
  (16 'fenixos          "FenixOS")
  (17 'cloudabi         "Nuxi Cloud ABI")
  (18 'openvos          "Stratus Technologies OpenVOS"))
  
(define-option-map type-map
  (0 'none        "No file type")
  (1 'relocatable "Relocatable file")
  (2 'executable  "Executable file")
  (3 'shared      "Shared object file")
  (4 'core        "Core file"))

;; List of machine codes from the specification at
;; http://www.sco.com/developers/gabi/latest/ch4.eheader.html
(define-option-map machine-map
  (0    'none       "No machine")
  (1    'm32        "AT&T WE 32100")
  (2    'sparc      "SPARC")
  (3    'x86        "Intel x86")
  (4    'm68k       "Motorola 68000")
  (5    'm88k       "Motorola 88000")
  (6    'iamcu      "Intel MCU")
  (7    '860        "Intel 80860")
  (8    'mips       "MIPS I Architecture")
  (9    's370       "IBM System/370 Processor")
  (10   'mips-rs3   "MIPS RS3000 Little Endian")
  (15   'parisc     "Hewlett-Packard PA-RISC")
  (17   'vpp500     "Fujitsu VPP-500")
  (18   'sparc32    "Enhanced instruction set SPARC")
  (19   '960        "Intel 80960")
  (#x14 'ppc        "PowerPC")
  (#x15 'ppc64      "PowerPC 64-bit")
  (#x16 's390       "IBM S/390")
  (#x17 'spu        "IBM SPU/SPC")
  (#x24 'v800       "NEC V800")
  (#x25 'fr20       "Fujitsu FR20")
  (#x26 'rh32       "TRW RH-32")
  (#x27 'rce        "Motorola RCE")
  (#x28 'arm        "ARM 32-bit (AArch32)")
  (#x29 'alpha      "Digital Alpha")
  (#x2A 'sh         "Hitachi SH")
  (#x2B 'sparcv9    "SPARC Version 9")
  (#x32 'ia-64      "Intel Itanium")
  (#x3E 'x86-64     "AMD x86-64")
  (#x50 'mmix       "Knuth's educational 64-bit processor MMIX")
  (#xB7 'aarch64    "ARM 64-bit (AArch64)")
  (#xB9 'avr32      "Atmel 32-bit microprocessor family")
  (#xBD 'microblaze "Xilinx MicroBlaze 32-bit RISC soft processor core")
  (#xBE 'cuda       "NVidia CUDA architecture")
  (#xDC 'z80        "Zilog Z80")
  (#xE0 'amdgpu     "AMD GPU architecture")
  (#xF3 'risc-v     "RISC-V"))

(define-option-map sec-type-map
  (0    'null-sec   "Section table entry unused")
  (1    'progbits   "Program data")
  (2    'symtab     "Symbol table")
  (3    'strtab     "String table")
  (4    'rela       "Relocation entries with addends")
  (5    'hash       "Symbol hash table")
  (6    'dynamic    "Dynamic linking information")
  (7    'note       "Notes")
  (8    'nobits     "Program space with no data")
  (9    'rel        "Relocation entries without addends")
  (10   'shlib      "Reserved (SHLIB)")
  (11   'dymsym     "Dynamic linker symbol table")
  (14   'initarray  "Array of initialization functions")
  (15   'finiarray  "Array of finalization functions")
  (16   'preinitarr "Array of pre-initialization functions")
  (17   'group      "Section group")
  (18   'stshndx    "Symbol table SHN_INDEX references"))

;; TODO: OS-specific and processor-specific flags
(define-option-map section-attr-flags
  (#x01  'write      "WRITE")
  (#x02  'alloc      "ALLOC")
  (#x04  'exec       "EXECINSTR")
  (#x10  'merge      "MERGE")
  (#x20  'strings    "STRINGS")
  (#x40  'infolink   "INFO_LINK")
  (#x80  'linkorder  "LINK_ORDER")
  (#x100 'osnonconf  "OS_NONCONFORMING")
  (#x200 'group      "GROUP")
  (#x400 'tls        "TLS")
  (#x800 'compressed "COMPRESSED"))


