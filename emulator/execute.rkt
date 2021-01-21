#lang rosette 

(require
  "init.rkt"
  "machine.rkt"
  "parameters.rkt")

; Execute each individual instruction symbolically and update the program count to the proper place.
; Used rv8.io and https://content.riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf for implementing instructions
; Conventions used for decoding are as follows: v_var represents the bitvector value stored in register var. Otherwise,
; the variable var contains the index into the register that it refers to.

; Helper function to convert bitvectors to naturals after indexing instruction
(define (list-ref-nat instr idx)
  (bitvector->natural (list-ref instr idx)))

(define (list-ref-int instr idx)
  (bitvector->integer (list-ref instr idx)))

; execute symbolic instruction
(define (execute m instr)
  (cond
    [(equal? instr null)
      (illegal-instr m)]
    [else
      (define opcode (list-ref instr 0))
      (define pc (get-pc m))
      (cond
        ; SPECIAL Format
        [(eq? opcode 'ecall)
          ; TODO: real ecall implementation
          (illegal-instr m)]
        [(eq? opcode 'ebreak)
          ; TODO: ebreak instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'uret)
          ; TODO: uret instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mret)
          (define mstatus (get-csr m 'mstatus))
          (define MPP (extract 12 11 mstatus))
          ; this is always user mode
          ; TODO: fix this and set mstatus to its actual value of MPP, for now we are setting to 0
          ; since we always but it to user mode
          ; (set-machine-mode! m (bitvector->natural MPP))
          (set-machine-mode! m 0)
          (set-pc! m (bvsub (get-csr m 'mepc) (base-address)))
          instr]
        [(eq? opcode 'dret)
          ; TODO: dret instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'sfence_vma)
          ; TODO: sfence_vma instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'wfi)
          ; TODO: wfi instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'csrrw)
          (cond
            [(equal? (machine-mode m) 1)
              (define rd (list-ref-nat instr 1))
              (define rs1 (list-ref-nat instr 2))
              (define v_rs1 (gprs-get-x m rs1))
              (define csr (list-ref instr 3))
              (define v_csr (get-csr m csr))
              (cond
                [(and (not (eq? v_rs1 null)) (not (eq? v_csr null)))
                  (when (not (zero? rd))
                    (gprs-set-x! m rd v_csr))
                  (set-csr! m csr v_rs1)
                  (set-pc! m (bvadd pc (bv 4 64)))
                  instr]
                [else
                  (illegal-instr m)])]
            [else
              (illegal-instr m)])]
        [(eq? opcode 'csrrs)
          (cond
            [(equal? (machine-mode m) 1)
              (define rd (list-ref-nat instr 1))
              (define rs1 (list-ref-nat instr 2))
              (define bitmask (gprs-get-x m rs1))
              (define csr (list-ref instr 3))
              (define v_csr (get-csr m csr))
              (cond
                [(and (not (eq? bitmask null)) (not (eq? v_csr null)))
                  (gprs-set-x! m rd v_csr)
                  (set-csr! m csr (bvor v_csr bitmask))
                  (set! v_csr (zero-extend (get-csr m csr) (bitvector 64)))
                  (set-pc! m (bvadd pc (bv 4 64)))
                  instr]
                [else
                  (illegal-instr m)])]
            [else
              (illegal-instr m)])]
        [(eq? opcode 'csrrc)
          ; TODO: csrrc instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'csrrwi)
          ; TODO: csrrwi instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'csrrsi)
          ; TODO: csrrsi instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'csrrci)
          ; TODO: csrrci instruction not implemented yet
          (illegal-instr m)]

        ; I Format
        [(eq? opcode 'addi)
          (define rd (list-ref-nat instr 1))
          (define rs1 (list-ref-nat instr 2))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          ; nop op pseudo code condition
          (cond
            [(not (and (equal? rd 0) (equal? rs1 0) (bveq imm (bv 0 64))))
              (define v_rs1 (gprs-get-x m rs1))
              (cond
                [(not (eq? v_rs1 null))
                  (set-pc! m (bvadd pc (bv 4 64)))
                  (gprs-set-x! m rd (bvadd v_rs1 imm))
                  instr]
                [else null])]
            [else
              (set-pc! m (bvadd pc (bv 4 64)))
              instr])]
        [(eq? opcode 'slli)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
          (define shifted (bvshl v_rs1 imm))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'srli)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
          (define shifted (bvlshr v_rs1 imm))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'srai)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
          (define shifted (bvashr v_rs1 imm))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'slti)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (if (bvslt v_rs1 imm)
            (gprs-set-x! m rd 1)
            (gprs-set-x! m rd 0))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr] 
        [(eq? opcode 'sltiu)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (if (bvult v_rs1 imm)
            (gprs-set-x! m rd 1)
            (gprs-set-x! m rd 0))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr] 
        [(eq? opcode 'xori)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (gprs-set-x! m rd (bvxor v_rs1 imm))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'ori)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (gprs-set-x! m rd (bvor v_rs1 imm))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'andi)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (gprs-set-x! m rd (bvand v_rs1 imm))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'addiw)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define 32bit_sum (extract 31 0 (bvadd v_rs1 imm)))
          (gprs-set-x! m rd (sign-extend 32bit_sum (bitvector 64)))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'slliw)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
          (define imm (zero-extend (list-ref instr 3) (bitvector 32)))
          (define shifted (sign-extend (bvshl v_rs1 imm) (bitvector 64)))
          (gprs-set-x! m rd shifted)
          (if (not (bveq (extract 5 5 imm) (bv 0 1)))
            (illegal-instr m)
            (begin
              (set-pc! m (bvadd pc (bv 4 64)))
              instr))]
        [(eq? opcode 'srliw)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
          (define imm (zero-extend (list-ref instr 3) (bitvector 32)))
          (define shifted (sign-extend (bvlshr v_rs1 imm) (bitvector 64)))
          (gprs-set-x! m rd shifted)
          (if (not (bveq (extract 5 5 imm) (bv 0 1)))
            (illegal-instr m)
            (begin 
              (set-pc! m (bvadd pc (bv 4 64)))
              instr))]
        [(eq? opcode 'sraiw)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
          (define imm (zero-extend (list-ref instr 3) (bitvector 32)))
          (define shifted (sign-extend (bvashr v_rs1 imm) (bitvector 64)))
          (gprs-set-x! m rd shifted)
          (if (not (bveq (extract 5 5 imm) (bv 0 1)))
            (illegal-instr m)
            (begin 
              (set-pc! m (bvadd pc (bv 4 64)))
              instr))]
        [(eq? opcode 'lb)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 1)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'lh)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 2)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'lw)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 4)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'ld)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 8)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'lbu)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 1)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'lhu)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 2)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'lwu)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          (define addr (bvadd v_rs1 imm))
          (define adj_addr (bvsub addr (base-address)))
          (define nbytes 4)
          ; stronger case that covers all possible values that val can take
          (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
          (gprs-set-x! m rd val)
            (set-pc! m (bvadd pc (bv 4 64)))
            instr]
        [(eq? opcode 'jalr)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
          ; addr is bitvector 64 correct address
          (define addr (bvand (bvadd v_rs1 imm) (bvnot (bv 1 64))))
          ; adj_addr is adjusted for offset
          (define adj_addr (bvsub addr (base-address)))

          (define save (bvadd pc (bv 4 64)))
          (cond
            [(not (equal? rd 0))
              (gprs-set-x! m rd save)])
          (set-pc! m adj_addr)
          instr]

        ; R Format
        [(eq? opcode 'add)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (gprs-set-x! m rd (bvadd v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'sub)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (gprs-set-x! m rd (bvsub v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'sll)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (define shifted (bvshl v_rs1 v_rs2))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'slt)
            ; TODO: slt instruction not implemented yet
            (illegal-instr m)]
        [(eq? opcode 'sltu)
          ; TODO: sltu instruction not implemented yet
            (illegal-instr m)]
        [(eq? opcode 'xor)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (gprs-set-x! m rd (bvxor v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'srl)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (define shifted (bvlshr v_rs1 v_rs2))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'sra)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (define shifted (bvashr v_rs1 v_rs2))
          (gprs-set-x! m rd shifted)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'or)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (gprs-set-x! m rd (bvor v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'and)
          (define rd (list-ref-nat instr 1))
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
          (gprs-set-x! m rd (bvand v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'andw)
          ; TODO: andw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'subw)
          ; TODO: subw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'sllw)
          ; TODO: sllw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'srlw)
          ; TODO: srlw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'sraw)
          ; TODO: sraw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mul)
          ; TODO: mul instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mulh)
          ; TODO: mulh instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mulhsu)
          ; TODO: mulhsu instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mulhu)
          ; TODO: mulhu instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'div)
          ; TODO: div instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'divu)
          ; TODO: divu instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'rem)
          ; TODO: rem instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'remu)
          ; TODO: remu instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'mulw)
          ; TODO: mulw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'divw)
          ; TODO: divw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'divuw)
          ; TODO: divuw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'remw)
          ; TODO: remw instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'remuw)
          ; TODO: remuw instruction not implemented yet
          (illegal-instr m)]

        ; B Format
        [(eq? opcode 'beq)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (if (equal? v_rs1 v_rs2)
            (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
            (set-pc! m (bvadd pc (bv 4 64))))
          instr]
        [(eq? opcode 'bne)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (if (not (equal? v_rs1 v_rs2))
            (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
            (set-pc! m (bvadd pc (bv 4 64))))
          instr]
        [(eq? opcode 'blt)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (if (bvslt v_rs1 v_rs2)
            (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
            (set-pc! m (bvadd pc (bv 4 64))))
          instr]
        [(eq? opcode 'bge)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (if (bvsge v_rs1 v_rs2)
            (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
            (set-pc! m (bvadd pc (bv 4 64))))
          instr]
        [(eq? opcode 'bltu)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (if (bvult v_rs1 v_rs2)
            (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
            (set-pc! m (bvadd pc (bv 4 64))))
          instr]
        [(eq? opcode 'bgeu)
          (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
          (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
          (define imm (list-ref instr 3))
          (cond
            [(and (not (eq? v_rs1 null) (not (eq? v_rs2 null))))
              (if (bvuge v_rs1 v_rs2)
                (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
                (set-pc! m (bvadd pc (bv 4 64))))
              instr]
            [else null])]

        ; U Format
        [(eq? opcode 'lui)
          (define rd (list-ref-nat instr 1))
          ; extend immediate by 12 bits
          (define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
          (gprs-set-x! m rd imm)
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]
        [(eq? opcode 'auipc)
          (define rd (list-ref-nat instr 1))
          ; extend immediate by 12 bits, then zero-extend to 64 bits
          (define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
          (gprs-set-x! m rd (bvadd pc (base-address) imm))
          (set-pc! m (bvadd pc (bv 4 64)))
          instr]

        ; S Format
        [(eq? opcode 'sb)
          (define nbits 8)
          (define success
            (if (use-sym-optimizations)
              (begin
                (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
                (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
                (machine-ram-write! m adj_addr v_rs2 nbits))
              (begin
                (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
                (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
                (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
                (define addr (bvadd v_rs1 imm))
                ; (when (and (not (use-fnmem)) (bveq (bvsmod addr (bv #x100 64)) (bv 0 64)))
                ;   (printf "addr: ~a~n" addr))
                (define adj_addr (bvsub addr (base-address)))
                (machine-ram-write! m adj_addr v_rs2 nbits))))
          (cond
            [(not success)
              (illegal-instr m)]
            [else
              (set-pc! m (bvadd pc (bv 4 64)))
              instr])]
        [(eq? opcode 'sh)
          (define nbits 16)
          (define success
            (if (use-sym-optimizations)
              (begin
                (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
                (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
                (machine-ram-write! m adj_addr v_rs2 nbits))
              (begin 
                (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
                (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
                (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
                (define addr (bvadd v_rs1 imm))
                (define adj_addr (bvsub addr (base-address)))
                (machine-ram-write! m adj_addr v_rs2 nbits))))
          (cond
            [(not success)
              (illegal-instr m)]
            [else
              (set-pc! m (bvadd pc (bv 4 64)))
              instr])]
        [(eq? opcode 'sw)
          (define nbits 32)
          (define success
            (if (use-sym-optimizations)
              (begin
                (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
                (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
                (machine-ram-write! m adj_addr v_rs2 nbits))
              (begin 
                (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
                (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
                (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
                (define addr (bvadd v_rs1 imm))
                (define adj_addr (bvsub addr (base-address)))
                (machine-ram-write! m adj_addr v_rs2 nbits))))
          (cond
            [(not success)
              (illegal-instr m)]
            [else
              (set-pc! m (bvadd pc (bv 4 64)))
              instr])]
        [(eq? opcode 'sd)
          (define nbits 64)
          (define success
            (if (use-sym-optimizations)
              (begin
                (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
                (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
                (machine-ram-write! m adj_addr v_rs2 nbits))
              (begin 
                (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
                (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
                (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
                (define addr (bvadd v_rs1 imm))
                (define adj_addr (bvsub addr (base-address)))
                (machine-ram-write! m adj_addr v_rs2 nbits))))
          (cond
            [(not success)
              (illegal-instr m)]
            [else
              (set-pc! m (bvadd pc (bv 4 64)))
              instr])]

        ; J Format
        [(eq? opcode 'jal)
          (define rd (list-ref-nat instr 1))
          (define imm (sign-extend (concat (list-ref instr 2) (bv 0 1)) (bitvector 64)))
          ; adjust for (base-address)
          (define save_addr (bvadd (bvadd pc (bv 4 64)) (base-address)))
          ; imm is the offset from pc, so we don't need to do anything with (base-address)
          (define jump_addr (bvadd imm pc))
          (cond
            [(not (equal? rd 0))
              (gprs-set-x! m rd save_addr)])
          (set-pc! m jump_addr)
          instr]

        ; FENCE Format
        [(eq? opcode 'FENCE)
          ; TODO: FENCE instruction not implemented yet
          (illegal-instr m)]
        [(eq? opcode 'FENCE_I)
          ; TODO: FENCE_I instruction not implemented yet
          (illegal-instr m)]
        [else
            (illegal-instr m)])]))
(provide execute)
