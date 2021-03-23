#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "machine.rkt"
  "pmp.rkt"
  "execute.rkt"
  "parameters.rkt"
  "print-utils.rkt"
  "machine.rkt"
  "instr.rkt")
(require (only-in racket/base 
                  parameterize for in-range for* define-syntax-rule symbol?))
(require (only-in racket
                  class init super-new define/public send object%))
(require profile)


(define m (parameterize
            ([ramsize-log2 32])
            (init-machine)))

(define-symbolic* sym-idx (bitvector 64))

(define (samplefn b)
  (if (bvult (bv #x100 64) b) #t #f))

(define model
  (verify
    (begin
      (assume (or (bvult sym-idx (bv #x80020000 64)) (bvult (bv #x8003FFFF 64) sym-idx)))
      (assert (not (pmp-check (machine-pmp m) 0 sym-idx sym-idx))))))

(printf "Model: ~a~n" model)

; (printf "~a~n" (evaluate sym-idx model))
; (printf "~a~n" (pmp-check (machine-pmp m) (machine-mode m) (bv #x0 64) (bv #x0 64)))

; (define model
;   (verify
;     (begin
;       ; (assume (or (bvult sym-idx (bv #x20000 64)) (bvult (bv #x3FFFF 64) sym-idx)))
;       (assert (not (samplefn sym-idx))))))

; (printf "~a~n" model)
; (printf "~a~n" (evaluate sym-idx model))


; ; Set up machine
; (define m (parameterize
;             ([ramsize-log2 32])
;             (init-machine)))
; (define gprs (machine-gprs m))
; (set-gprs-i! gprs 1 (bv #x000000000000003 64))
; (set-gprs-i! gprs 18 (bv #x0000000000000100 64))
; (printf "~a~n" (machine-gprs m))
; (printf "~a~n" (memory-read (machine-ram m) (bv #x400 32)))

; ; Step
; (parameterize
;   ([ramsize-log2 32])
;   (sb-instr m 1 18 (bv #x400 12)))
; (printf "~a~n" (memory-read (machine-ram m) (bv #x400 32)))


; (define program (file->bytearray "build/addiw.bin"))
; (printf "* Running addiw.bin test ~n")
; (define m
;  (parameterize
;      ([use-debug-mode #f]
;       [debug-instr #t]
;       [use-fnmem #f])
;    (init-machine-with-prog program)))
; (printf "RES: ~a~n" (parameterize
;    ([use-debug-mode #f]
;     [debug-instr #t]
;     [use-fnmem #f])
;  (execute-until-mret m)))
