#lang rosette/safe

(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

(define (uf-memory-write! mem addr val)
  (lambda (addr*)
    (if (equal? addr addr*)
      val
      (mem addr*))))

(define (uf-memory-read mem addr)
  (mem addr))

(define (array-memory-write! mem addr val)
  (vector-set! mem addr val))

(define (array-memory-read mem addr)
  (vector-ref mem addr))

(define array_memory (vector 0 1 2))
(define-symbolic* idx integer?)
(define-symbolic* pos val integer?)

(printf "Before Write: ~v~n" (array-memory-read array_memory idx))
(array-memory-write! array_memory pos val)
(printf "After Write: ~v~n" (array-memory-read array_memory idx))

; >>> Before Write: (ite* (⊢ (= 0 idx$0) 0) (⊢ (= 1 idx$0) 1) (⊢ (= 2 idx$0) 2))
; >>> After Write: (ite* (⊢ (= 0 idx$0) (ite (= 0 pos$0) val$0 0)) (⊢ (= 1 idx$0) (ite (= 1 pos$0) val$0 1)) (⊢ (= 2 idx$0) (ite (= 2 pos$0) val$0 2)))

(define uf_memory (fresh-symbolic fnmem (~> integer? integer?)))
(uf-memory-write! uf_memory 0 0)
(uf-memory-write! uf_memory 1 1)
(uf-memory-write! uf_memory 2 2)

(printf "Before Write: ~v~n" (uf-memory-read uf_memory idx))
(uf-memory-write! uf_memory pos val)
(printf "After Write: ~v~n" (uf-memory-read uf_memory idx))

; >>> Before Write: (app fnmem$0 idx$0)
; >>> After Write: (app fnmem$0 idx$0)
