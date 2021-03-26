#lang rosette/safe

(require
  "machine.rkt"
  "parameters.rkt")

(define (c.fld-instr m)
  ; TODO: Implement Instruction
  (list 'c.fld))
(provide c.fld-instr)

(define (c.lw-instr m rd offset)
  (define pc (machine-pc m))
  (define adj_addr (bvsub offset (base-address)))
  (define nbytes 4)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.lw rd offset))
(provide c.lw-instr)

(define (c.ld-instr m rd offset)
  (define pc (machine-pc m))
  (define adj_addr (bvsub offset (base-address)))
  (define nbytes 8)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.ld rd offset))
(provide c.ld-instr)

(define (c.fsd-instr m)
  ; TODO: Implement Instruction
  (list 'c.fsd))
(provide c.fsd-instr)

(define (c.sw-instr m rs2 offset)
  (define pc (machine-pc m))
  (define nbits 32)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin 
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define adj_addr (bvsub offset (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.sw rs2 offset))
(provide c.sw-instr)

(define (c.sd-instr m rs2 offset)
  (define pc (machine-pc m))
  (define nbits 64)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin 
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define adj_addr (bvsub offset (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.sd rs2 offset))
(provide c.sd-instr)

(define (c.addi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define se_imm (sign-extend imm (bitvector 64)))
  (cond
    [(not (and (bvzero? rd) (bvzero? rs1) (bveq se_imm (bv 0 64))))
     (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
     (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 se_imm))
     (set-machine-pc! m (bvadd pc (bv 2 64)))]
    [else
     ; nop op pseudo code condition
     (set-machine-pc! m (bvadd pc (bv 2 64)))])
  (list 'c.addi rd rs1 imm))
(provide c.addi-instr)

(define (c.addiw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define se_imm (sign-extend imm (bitvector 32)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvadd v_rs1 se_imm) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.addiw rd rs1 imm))
(provide c.addiw-instr)

(define (c.lui-instr m rd imm)
  (define pc (machine-pc m))
  ; extend immediate by 12 bits
  (define se_imm (sign-extend (concat imm (bv 0 12)) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd se_imm)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.lui rd imm))
(provide c.lui-instr)

(define (c.srli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvlshr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.srli rd rs1 imm))
(provide c.srli-instr)

(define (c.srai-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvashr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.srai rd rs1 imm))
(provide c.srai-instr)

(define (c.andi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.andi rd rs1 imm))
(provide c.andi-instr)

(define (c.add-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.add rd rs1 rs2))
(provide c.add-instr)

(define (c.sub-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvsub v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.sub rd rs1 rs2))
(provide c.sub-instr)

(define (c.xor-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvxor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.xor rd rs1 rs2))
(provide c.xor-instr)

(define (c.or-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.or rd rs1 rs2))
(provide c.or-instr)

(define (c.and-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.and rd rs1 rs2))
(provide c.and-instr)

(define (c.subw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvsub v_rs1 v_rs2) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.subw-instr rd rs1 rs2))
(provide c.subw-instr)

(define (c.addw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvadd v_rs1 v_rs2) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.addw rd rs1 rs2))
(provide c.addw-instr)

(define (c.jal-instr m rd offset)
  (define pc (machine-pc m))
  ; adjust for (base-address)
  (define save_addr (bvadd pc (bv 2 64) (base-address)))
  ; imm is the offset from pc, so we don't need to do anything with (base-address)
  (define jump_addr (bvadd offset pc))
  (when (not (bvzero? rd))
    (set-gprs-i! (machine-gprs m) rd save_addr))
  (set-machine-pc! m jump_addr)
  (list 'c.jal rd offset))
(provide c.jal-instr)

(define (c.jalr-instr m rd rs1 offset)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  
  ; Remove least significant bit
  (define addr (bvand (bvadd v_rs1 offset) (bvnot (bv 1 64))))
  ; Adjust for address offset
  (define adj_addr (bvsub addr (base-address)))
  
  (define save (bvadd pc (bv 2 64) (base-address)))
  (set-gprs-i! (machine-gprs m) rd save)
  (set-machine-pc! m adj_addr)
  (list 'c.jalr rd rs1 offset))
(provide c.jalr-instr)

(define (c.beq-instr m rs1 rs2 offset)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bveq v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul offset (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 2 64))))
  (list 'c.beq rs1 rs2 offset))
(provide c.beq-instr)

(define (c.bne-instr m rs1 rs2 offset)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (not (bveq v_rs1 v_rs2))
      (set-machine-pc! m (bvadd pc (bvmul offset (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 2 64))))
  (list 'c.bne rs1 rs2 offset))
(provide c.bne-instr)

(define (c.slli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvshl v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.slli rd rs1 imm))
(provide c.slli-instr)

(define (c.fldsp-instr m)
  ; TODO: Implement Instruction
  (list 'c.fldsp))
(provide c.fldsp-instr)

(define (c.fsdsp-instr m)
  ; TODO: Implement Instruction
  (list 'c.fsdsp))
(provide c.fsdsp-instr)

(define (c.swsp-instr m)
  ; TODO: Implement Instruction
  (list 'c.swsp))
(provide c.swsp-instr)

(define (c.sdsp-instr m)
  ; TODO: Implement Instruction
  (list 'c.sdsp))
(provide c.sdsp-instr)

(define (c.nop-instr m)
  (define pc (machine-pc m))
  (set-machine-pc! m (bvadd pc (bv 2 64)))
  (list 'c.nop))
(provide c.nop-instr)
