#lang rosette/safe

(require
  serval/lib/unittest
  serval/lib/core
  serval/riscv/base
  serval/riscv/interp
  serval/riscv/objdump
  (only-in racket/base struct-copy for)
  "generated/monitors/legOS/verif/asm-offsets.rkt"
  (prefix-in legOS:
    (combine-in
      "generated/monitors/legOS.asm.rkt"
      "generated/monitors/legOS.globals.rkt"
      "generated/monitors/legOS.map.rkt")))

(provide forall/cpu cpu-ecall find-block-by-name cpu-mregions
         mblock-iload __NR_get_and_set)

(define (forall/cpu k)
  (define cpu (init-cpu legOS:symbols legOS:globals))
  (gpr-set! cpu 'a0 (bv 1 (XLEN)))

  (interpret-objdump-program cpu legOS:instructions)
  (k cpu))

(define (cpu-ecall c callno args)
  (define c2 (struct-copy cpu c))
  (set-cpu-pc! c2 (csr-ref c2 'mtvec))
  (csr-set! c2 'mcause (bv EXC_ECALL_S 64))
  (gpr-set! c2 'a7 callno)
  (for ([reg '(a0 a1 a2 a3 a4 a5 a6)] [arg args])
    (gpr-set! c2 reg arg))
  (interpret-objdump-program c2 legOS:instructions)
  c2)

(define (sanity-check)
  (define cpu (init-cpu legOS:symbols legOS:globals))
  ; (display cpu)
  (gpr-set! cpu 'a0 (bv 1 (XLEN)))

  (define asserted (with-asserts-only (interpret-objdump-program cpu legOS:instructions)))
  (check-unsat? (verify (assert (apply && asserted))))

  (void))

(module+ test
  (sanity-check)
  (displayln "Hello World!")
  (displayln "Testing!"))
