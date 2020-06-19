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
(define (execute instr m)
  (when (not (equal? instr null))
    (define opcode (list-ref instr 0))
    (define pc (get-pc m))
    (cond
      ; SPECIAL Format
      [(eq? opcode 'ecall)
        (when (use-debug-mode) (printf " --> ecall ~n"))
        ; TODO: real ecall implementation
        (illegal-instr m)]
      [(eq? opcode 'ebreak)
        (when (use-debug-mode) (printf " --> ebreak ~n"))
        ; TODO: ebreak instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'uret)
        (when (use-debug-mode) (printf " --> uret ~n"))
        ; TODO: uret instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mret)
        (when (use-debug-mode) (printf " --> mret ~n"))
        (define mstatus (get-csr m 'mstatus))
        (define MPP (extract 12 11 mstatus))
        ; this is always user mode
        ; TODO: fix this and set mstatus to concrete value
        ; (set-machine-mode! m (bitvector->natural MPP))
        (set-machine-mode! m 0)
        (set-pc! m (bvsub (get-csr m 'mepc) (base-address)))
        instr]
      [(eq? opcode 'dret)
        (when (use-debug-mode) (printf " --> dret ~n"))
        ; TODO: dret instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'sfence_vma)
        (when (use-debug-mode) (printf " --> sfence_vma ~n"))
        ; TODO: sfence_vma instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'wfi)
        (when (use-debug-mode) (printf " --> wfi ~n"))
        ; TODO: wfi instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'csrrw)
        (when (use-debug-mode) (printf " --> csrrw ~n"))
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
        (when (use-debug-mode) (printf " --> csrrs ~n"))
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
        (when (use-debug-mode) (printf " --> csrrc ~n"))
        ; TODO: csrrc instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'csrrwi)
        (when (use-debug-mode) (printf " --> csrrwi ~n"))
        ; TODO: csrrwi instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'csrrsi)
        (when (use-debug-mode) (printf " --> csrrsi ~n"))
        ; TODO: csrrsi instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'csrrci)
        (when (use-debug-mode) (printf " --> csrrci ~n"))
        ; TODO: csrrci instruction not implemented yet
        (illegal-instr m)]

      ; I Format
      [(eq? opcode 'addi)
        (when (use-debug-mode) (printf " --> addi ~n"))
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
        (when (use-debug-mode) (printf " --> slli ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
        (define shifted (bvshl v_rs1 imm))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'srli)
        (when (use-debug-mode) (printf " --> srli ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
        (define shifted (bvlshr v_rs1 imm))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'srai)
        (when (use-debug-mode) (printf " --> srai ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
        (define shifted (bvashr v_rs1 imm))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'slti)
        (when (use-debug-mode) (printf " --> slti ~n"))
        ; TODO: slti instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'sltiu)
        (when (use-debug-mode) (printf " --> sltiu ~n"))
        ; TODO: sltiu instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'xori)
        (when (use-debug-mode) (printf " --> xori ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
        (gprs-set-x! m rd (bvxor v_rs1 imm))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'ori)
        (when (use-debug-mode) (printf " --> ori ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
        (gprs-set-x! m rd (bvor v_rs1 imm))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'andi)
        (when (use-debug-mode) (printf " --> andi ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
        (gprs-set-x! m rd (bvand v_rs1 imm))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'addiw)
        (when (use-debug-mode) (printf " --> addiw ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (sign-extend (list-ref instr 3) (bitvector 64)))
        (define 32bit_sum (extract 31 0 (bvadd v_rs1 imm)))
        (gprs-set-x! m rd (sign-extend 32bit_sum (bitvector 64)))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'slliw)
        (when (use-debug-mode) (printf " --> slliw ~n"))
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
        (when (use-debug-mode) (printf " --> srliw ~n"))
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
        (when (use-debug-mode) (printf " --> sraiw ~n"))
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
        (when (use-debug-mode) (printf " --> lb ~n"))
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
        (when (use-debug-mode) (printf " --> lh ~n"))
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
        (when (use-debug-mode) (printf " --> lw ~n"))
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
        (when (use-debug-mode) (printf " --> ld ~n"))
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
        (when (use-debug-mode) (printf " --> lbu ~n"))
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
        (when (use-debug-mode) (printf " --> lhu ~n"))
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
        (when (use-debug-mode) (printf " --> lwu ~n"))
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
        (when (use-debug-mode) (printf " --> jalr ~n"))
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
        (when (use-debug-mode) (printf " --> add ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (gprs-set-x! m rd (bvadd v_rs1 v_rs2))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'sub)
        (when (use-debug-mode) (printf " --> sub ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (gprs-set-x! m rd (bvsub v_rs1 v_rs2))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'sll)
        (when (use-debug-mode) (printf " --> sll ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (define shifted (bvshl v_rs1 v_rs2))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'slt)
        (when (use-debug-mode) (printf " --> slt ~n"))
          ; TODO: slt instruction not implemented yet
          (illegal-instr m)]
      [(eq? opcode 'sltu)
        (when (use-debug-mode) (printf " --> sltu ~n"))
        ; TODO: sltu instruction not implemented yet
          (illegal-instr m)]
      [(eq? opcode 'xor)
        (when (use-debug-mode) (printf " --> xor ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (gprs-set-x! m rd (bvxor v_rs1 v_rs2))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'srl)
        (when (use-debug-mode) (printf " --> srl ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (define shifted (bvlshr v_rs1 v_rs2))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'sra)
        (when (use-debug-mode) (printf " --> sra ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (define shifted (bvashr v_rs1 v_rs2))
        (gprs-set-x! m rd shifted)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'or)
        (when (use-debug-mode) (printf " --> or ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (gprs-set-x! m rd (bvor v_rs1 v_rs2))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'and)
        (when (use-debug-mode) (printf " --> and ~n"))
        (define rd (list-ref-nat instr 1))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
        (gprs-set-x! m rd (bvand v_rs1 v_rs2))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'andw)
        (when (use-debug-mode) (printf " --> andw ~n"))
        ; TODO: andw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'subw)
        (when (use-debug-mode) (printf " --> subw ~n"))
        ; TODO: subw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'sllw)
        (when (use-debug-mode) (printf " --> sllw ~n"))
        ; TODO: sllw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'srlw)
        (when (use-debug-mode) (printf " --> srlw ~n"))
        ; TODO: srlw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'sraw)
        (when (use-debug-mode) (printf " --> sraw ~n"))
        ; TODO: sraw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mul)
        (when (use-debug-mode) (printf " --> mul ~n"))
        ; TODO: mul instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mulh)
        (when (use-debug-mode) (printf " --> mulh ~n"))
        ; TODO: mulh instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mulhsu)
        (when (use-debug-mode) (printf " --> mulhsu ~n"))
        ; TODO: mulhsu instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mulhu)
        (when (use-debug-mode) (printf " --> mulhu ~n"))
        ; TODO: mulhu instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'div)
        (when (use-debug-mode) (printf " --> div ~n"))
        ; TODO: div instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'divu)
        (when (use-debug-mode) (printf " --> divu ~n"))
        ; TODO: divu instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'rem)
        (when (use-debug-mode) (printf " --> rem ~n"))
        ; TODO: rem instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'remu)
        (when (use-debug-mode) (printf " --> remu ~n"))
        ; TODO: remu instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'mulw)
        (when (use-debug-mode) (printf " --> mulw ~n"))
        ; TODO: mulw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'divw)
        (when (use-debug-mode) (printf " --> divw ~n"))
        ; TODO: divw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'divuw)
        (when (use-debug-mode) (printf " --> divuw ~n"))
        ; TODO: divuw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'remw)
        (when (use-debug-mode) (printf " --> remw ~n"))
        ; TODO: remw instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'remuw)
        (when (use-debug-mode) (printf " --> remuw ~n"))
        ; TODO: remuw instruction not implemented yet
        (illegal-instr m)]

      ; B Format
      [(eq? opcode 'beq)
        (when (use-debug-mode) (printf " --> beq ~n"))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (list-ref instr 3))
        (if (equal? v_rs1 v_rs2)
          (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
          (set-pc! m (bvadd pc (bv 4 64))))
        instr]
      [(eq? opcode 'bne)
        (when (use-debug-mode) (printf " --> bne ~n"))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (list-ref instr 3))
        (if (not (equal? v_rs1 v_rs2))
          (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
          (set-pc! m (bvadd pc (bv 4 64))))
        instr]
      [(eq? opcode 'blt)
        (when (use-debug-mode) (printf " --> blt ~n"))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (list-ref instr 3))
        (if (bvslt v_rs1 v_rs2)
          (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
          (set-pc! m (bvadd pc (bv 4 64))))
        instr]
      [(eq? opcode 'bge)
        (when (use-debug-mode) (printf " --> bge ~n"))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (list-ref instr 3))
        (if (bvsge v_rs1 v_rs2)
          (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
          (set-pc! m (bvadd pc (bv 4 64))))
        instr]
      [(eq? opcode 'bltu)
        (when (use-debug-mode) (printf " --> bltu ~n"))
        (define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
        (define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
        (define imm (list-ref instr 3))
        (if (bvult v_rs1 v_rs2)
          (set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
          (set-pc! m (bvadd pc (bv 4 64))))
        instr]
      [(eq? opcode 'bgeu)
        (when (use-debug-mode) (printf " --> bgeu ~n"))
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
        (when (use-debug-mode) (printf " --> lui ~n"))
        (define rd (list-ref-nat instr 1))
        ; extend immediate by 12 bits
        (define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
        (gprs-set-x! m rd imm)
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]
      [(eq? opcode 'auipc)
        (when (use-debug-mode) (printf " --> auipc ~n"))
        (define rd (list-ref-nat instr 1))
        ; extend immediate by 12 bits, then zero-extend to 64 bits
        (define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
        (gprs-set-x! m rd (bvadd pc (base-address) imm))
        (set-pc! m (bvadd pc (bv 4 64)))
        instr]

      ; S Format
      [(eq? opcode 'sb)
        (when (use-debug-mode) (printf " --> sb ~n"))
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
              (define adj_addr (bvsub addr (base-address)))
              (machine-ram-write! m adj_addr v_rs2 nbits))))
        (cond
          [(not success)
            (illegal-instr m)]
          [else
            (set-pc! m (bvadd pc (bv 4 64)))
            instr])]
      [(eq? opcode 'sh)
        (when (use-debug-mode) (printf " --> sh ~n"))
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
        (when (use-debug-mode) (printf " --> sw ~n"))
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
        (when (use-debug-mode) (printf " --> sd ~n"))
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
        (when (use-debug-mode) (printf " --> jal ~n"))
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
        (when (use-debug-mode) (printf " --> FENCE ~n"))
        ; TODO: FENCE instruction not implemented yet
        (illegal-instr m)]
      [(eq? opcode 'FENCE_I)
        (when (use-debug-mode) (printf " --> FENCE_I ~n"))
        ; TODO: FENCE_I instruction not implemented yet
        (illegal-instr m)]
      [else
          (illegal-instr m)])))
(provide execute)
