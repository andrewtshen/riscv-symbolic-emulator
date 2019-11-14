#lang rosette/safe

(require
  serval/lib/unittest
  serval/lib/core
  serval/riscv/base
  serval/riscv/interp
  serval/riscv/objdump
  (only-in racket/base struct-copy for)
  ; "generated/monitors/kernel/verif/asm-offsets.rkt"
  (prefix-in kernel:
    (combine-in
      "kernel.asm.rkt"
      "kernel.globals.rkt"
      "kernel.map.rkt")))


(define (verify-correct)
  (define cpu (init-cpu kernel:symbols kernel:globals))
  (define gprs (cpu-gprs cpu))
  ; Set program counter to architecturally-defined reset vector
  (set-cpu-pc! cpu (bv #x0000000020400000 64))

  ; Display initial CPU state
  (displayln "\nState of CPU before startup:")
  (displayln (vector-ref gprs 15))
  (displayln (vector-ref gprs 16))
  (displayln (vector-ref gprs 17))

  ; (displayln "kernel instructions: ")
  ; (displayln kernel:instructions)

  (interpret-objdump-program cpu kernel:instructions)

  ; Dsiplay final CPU state
  (displayln "\nState of CPU after startup:")
  (define x2 (vector-ref gprs 13))
  (define x3 (vector-ref gprs 14))
  (define x4 (vector-ref gprs 15))
  (define x5 (vector-ref gprs 16))
  (define x6 (vector-ref gprs 17))

  (displayln x4)
  (displayln x5)
  (displayln x6)
  ; (displayln (bveq (bvadd x4 x5) x6))

  (define sign (cond
    [(positive? (bitvector->integer x2)) 1]
    [(negative? (bitvector->integer x2)) -1]
    [else 0]))
  (displayln x2)
  (displayln sign)

  ; (displayln "\nSolving add")
  ; (solve (begin 
  ;   (assert (not (bveq (bvadd x4 x5) x6)))
  ;   ))

  (displayln "\nSolving sign")
  (solve (begin
    (assert (bveq (integer->bitvector sign) x3))
    ))
  )

; (define (cpu-ecall c callno args)
;   (define c2 (struct-copy cpu c))
;   (set-cpu-pc! c2 (csr-ref c2 'mtvec))
;   (csr-set! c2 'mcause (bv EXC_ECALL_S 64))
;   (gpr-set! c2 'a7 callno)
;   (for ([reg '(a0 a1 a2 a3 a4 a5 a6)] [arg args])
;     (gpr-set! c2 reg arg))
;   (interpret-objdump-program c2 kernel:instructions)
;   c2)

; (define (sanity-check)
;   (define cpu (init-cpu kernel:symbols kernel:globals))
;   (displayln cpu)
;   (gpr-set! cpu 'a0 (bv 1 (XLEN)))

;   (define asserted (with-asserts-only (interpret-objdump-program cpu kernel:instructions)))
;   (check-unsat? (verify (assert (apply && asserted))))

;   (void))

(module+ test
  ; (sanity-check)
  (displayln "Testing!"))
  (verify-correct)
