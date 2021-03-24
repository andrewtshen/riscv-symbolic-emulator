#lang rosette 

(require
  "init.rkt"
  "machine.rkt"
  "parameters.rkt"
  "csrs.rkt") 

; Execute each individual instruction symbolically and update the program count to the proper place.
; Used rv8.io and https://content.riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf for implementing instructions
; Conventions used for decoding are as follows: v_var represents the bitvector value stored in register var. Otherwise,
; the variable var contains the index into the register that it refers to.


;; Execute Instruction


;; SPECIAL Format
(define (ecall-instr m)
  ; TODO: real ecall implementation
  (define pc (machine-pc m))
  (list 'ecall))
(provide ecall-instr)

(define (ebreak-instr m)
  ; TODO: ebreak instruction not implemented yet
  (define pc (machine-pc m))
  (list 'ebreak))
(provide ebreak-instr)

(define (uret-instr m)
  ; TODO: uret instruction not implemented yet
  (define pc (machine-pc m))
  (list 'uret))
(provide uret-instr)

(define (mret-instr m)
  (define mode (machine-mode m))
  (cond
    [(equal? mode (bv 1 3))
     (define pc (machine-pc m))
     (define mstatus (machine-csr m MSTATUS))
     (define MPP (extract 12 11 mstatus))
     ; this is always user mode
     ; TODO: fix this and set mstatus to its actual value of MPP, for now we are setting to 0
     ; since we always but it to user mode
     ; (set-machine-mode! m (bitvector->natural MPP))
     (set-machine-mode! m (bv 0 3))
     (set-machine-pc! m (bvsub (machine-csr m MEPC) (base-address)))
     (list 'mret)]
    [else
     ; Throw illegal instruction if not machine mode
     'illegal-instruction]))
(provide mret-instr)

(define (dret-instr m)
  ; TODO: dret instruction not implemented yet
  (define pc (machine-pc m))
  (list 'dret))
(provide dret-instr)

(define (sfence_vma-instr m)
  ; TODO: sfence_vma instruction not implemented yet
  (define pc (machine-pc m))
  (list 'sfence_vma))
(provide sfence_vma-instr)

(define (wfi-instr m)
  ; TODO: wfi instruction not implemented yet
  (define pc (machine-pc m))
  (list 'wfi))
(provide wfi-instr)

(define (csrrw-instr m rd rs1 csr)
  (define pc (machine-pc m))
  (when (bveq (machine-mode m) (bv 1 3))
    (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
    (when (not (bvzero? rd))
      (define v_csr (machine-csr m csr))
      (set-gprs-i! (machine-gprs m) rd (zero-extend v_csr (bitvector 64))))
    ; TODO: Implement specific setting permissions for CSR bits
    (set-machine-csr! m csr v_rs1)
    (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'csrrw rd rs1 csr))
(provide csrrw-instr)

(define (csrrs-instr m rd rs1 csr)
  (define pc (machine-pc m))
  (when (bveq (machine-mode m) (bv 1 3))
    (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
    (define v_csr (machine-csr m csr))
    ; TODO: see if zero-extend is excessive
    (set-gprs-i! (machine-gprs m) rd (zero-extend v_csr (bitvector 64)))
    
    ; TODO: Implement specific setting permissions for CSR bits
    (when (not (bvzero? rs1))
      (set-machine-csr! m csr (bvor v_csr v_rs1)))
    (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'csrrs rd rs1 csr))
(provide csrrs-instr)

(define (csrrc-instr m)
  ; TODO: csrrc instruction not implemented yet
  (define pc (machine-pc m))
  (list 'csrrc))
(provide csrrc-instr)

(define (csrrwi-instr m rd rs1 csr)
  (define pc (machine-pc m))
  (when (bveq (machine-mode m) (bv 1 3))
    (define ze_rs1 (zero-extend rs1 (bitvector 64)))
    (when (not (bvzero? rd))
      (define v_csr (machine-csr m csr))
      (set-gprs-i! (machine-gprs m) rd (zero-extend v_csr (bitvector 64))))
    ; TODO: Implement specific setting permissions for CSR bits
    (set-machine-csr! m csr ze_rs1)
    (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'csrrwi rd rs1 csr))
(provide csrrwi-instr)

(define (csrrsi-instr m)
  ; TODO: csrrsi instruction not implemented yet
  (define pc (machine-pc m))
  (list 'csrrsi))
(provide csrrsi-instr)

(define (csrrci-instr m)
  ; TODO: csrrci instruction not implemented yet
  (define pc (machine-pc m))
  (list 'csrrci))
(provide csrrci-instr)


;; I Format
(define (addi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define se_imm (sign-extend imm (bitvector 64)))
  (cond
    [(not (and (bvzero? rd) (bvzero? rs1) (bveq se_imm (bv 0 64))))
     (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
     (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 se_imm))
     (set-machine-pc! m (bvadd pc (bv 4 64)))]
    [else
     ; nop op pseudo code condition
     (set-machine-pc! m (bvadd pc (bv 4 64)))])
  (list 'addi rd rs1 imm))
(provide addi-instr)

(define (slli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvshl v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'slli rd rs1 imm))
(provide slli-instr)

(define (srli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvlshr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'srli rd rs1 imm))
(provide srli-instr)

(define (srai-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvashr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'srai rd rs1 imm))
(provide srai-instr)

(define (slti-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (if (bvslt v_rs1 se_imm)
      (set-gprs-i! (machine-gprs m) rd (bv 1 64))
      (set-gprs-i! (machine-gprs m) rd (bv 0 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'slti rd rs1 imm))
(provide slti-instr)

(define (sltiu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (if (bvult v_rs1 se_imm)
      (set-gprs-i! (machine-gprs m) rd (bv 1 64))
      (set-gprs-i! (machine-gprs m) rd (bv 0 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sltiu rd rs1 imm))
(provide sltiu-instr)

(define (xori-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvxor v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'xori rd rs1 imm))
(provide xori-instr)

(define (ori-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvor v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'ori rd rs1 imm))
(provide ori-instr)

(define (andi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'addi rd rs1 imm))
(provide andi-instr)

(define (addiw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define se_imm (sign-extend imm (bitvector 32)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvadd v_rs1 se_imm) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'addiw rd rs1 imm))
(provide addiw-instr)

(define (slliw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvshl v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      'illegal-instruction
      (begin 
        (set-machine-pc! m (bvadd pc (bv 4 64)))
        (list 'slliw rd rs1 imm))))
(provide slliw-instr)

(define (srliw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvlshr v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      'illegal-instruction
      (begin
        (set-machine-pc! m (bvadd pc (bv 4 64)))
        (list 'srliw rd rs1 imm))))
(provide srliw-instr)

(define (sraiw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvashr v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      'illegal-instruction
      (begin
        (set-machine-pc! m (bvadd pc (bv 4 64)))
        (list 'sraiw rd rs1 imm))))
(provide sraiw-instr)

(define (lb-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 1)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'm rd rs1 imm))
(provide lb-instr)

(define (lh-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 2)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'm rd rs1 imm))
(provide lh-instr)

(define (lw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 4)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'm rd rs1 imm))
(provide lw-instr)

(define (ld-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 8)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'm rd rs1 imm))
(provide ld-instr)

(define (lbu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 1)
  ; stronger case that covers all possible values that val can take
  (define val (zero-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'lbu rd rs1 imm))
(provide lbu-instr)

(define (lhu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 2)
  ; stronger case that covers all possible values that val can take
  (define val (zero-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'lhu rd rs1 imm))
(provide lhu-instr)

(define (lwu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 4)
  ; stronger case that covers all possible values that val can take
  (define val (zero-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'lwu rd rs1 imm))
(provide lwu-instr)

(define (jalr-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  
  ; Remove least significant bit
  (define addr (bvand (bvadd v_rs1 se_imm) (bvnot (bv 1 64))))
  ; Adjust for address offset
  (define adj_addr (bvsub addr (base-address)))
  
  (define save (bvadd pc (bv 4 64) (base-address)))
  (set-gprs-i! (machine-gprs m) rd save)
  (set-machine-pc! m adj_addr)
  (list 'jalr rd rs1 imm))
(provide jalr-instr)


;; R Format
(define (add-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'add rd rs1 rs2))
(provide add-instr)

(define (sub-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvsub v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sub rd rs1 rs2))
(provide sub-instr)

(define (sll-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  ; Use only low 6 bits
  (define shamt (bvand v_rs2 (bv #b111111 64)))
  (define shifted (bvshl v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sll rd rs1 rs2))
(provide sll-instr)

(define (srl-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (define shamt (bvand v_rs2 (bv #b111111 64)))
  (define shifted (bvlshr v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'srl rd rs1 rs2))
(provide srl-instr)

(define (sra-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (define shamt (bvand v_rs2 (bv #b111111 64)))
  (define shifted (bvashr v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sra rd rs1 rs2))
(provide sra-instr)

(define (slt-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvslt v_rs1 v_rs2)
      (set-gprs-i! (machine-gprs m) rd (bv 1 64))
      (set-gprs-i! (machine-gprs m) rd (bv 0 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'slt rd rs1 rs2))
(provide slt-instr)

(define (sltu-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvult v_rs1 v_rs2)
      (set-gprs-i! (machine-gprs m) rd (bv 1 64))
      (set-gprs-i! (machine-gprs m) rd (bv 0 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sltu rd rs1 rs2))
(provide sltu-instr)

(define (xor-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvxor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'xor rd rs1 rs2))
(provide xor-instr)

(define (or-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'm rd rs1 rs2))
(provide or-instr)

(define (and-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'and rd rs1 rs2))
(provide and-instr)

(define (andw-instr m)
  ; TODO: andw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'andw))
(provide andw-instr)

(define (addw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvadd v_rs1 v_rs2) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'addw rd rs1 rs2))
(provide addw-instr)

(define (subw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend (bvsub v_rs1 v_rs2) (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'subw-instr rd rs1 rs2))
(provide subw-instr)

(define (sllw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  ; Use only low 6 bits
  (define shamt (bvand v_rs2 (bv #b11111 32)))
  (define shifted (bvshl v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd (sign-extend shifted (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sllw rd rs1 rs2))
(provide sllw-instr)

(define (srlw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (define shamt (bvand v_rs2 (bv #b11111 32)))
  (define shifted (bvlshr v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd (sign-extend shifted (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'srlw rd rs1 rs2))
(provide srlw-instr)

(define (sraw-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define v_rs2 (extract 31 0 (get-gprs-i (machine-gprs m) rs2)))
  (define shamt (bvand v_rs2 (bv #b11111 32)))
  (define shifted (bvashr v_rs1 shamt))
  (set-gprs-i! (machine-gprs m) rd (sign-extend shifted (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sraw rd rs1 rs2))
(provide sraw-instr)

(define (mul-instr m)
  ; TODO: mul instruction not implemented yet
  (define pc (machine-pc m))
  (list 'mul))
(provide mul-instr)

(define (mulh-instr m)
  ; TODO: mulh instruction not implemented yet
  (define pc (machine-pc m))
  (list 'mulh))
(provide mulh-instr)

(define (mulhsu-instr m)
  ; TODO: mulhsu instruction not implemented yet
  (define pc (machine-pc m))
  (list 'mulhsu))
(provide mulhsu-instr)

(define (mulhu-instr m)
  ; TODO: mulhu instruction not implemented yet
  (define pc (machine-pc m))
  (list 'mulhu))
(provide mulhu-instr)

(define (div-instr m)
  ; TODO: div instruction not implemented yet
  (define pc (machine-pc m))
  (list 'div))
(provide div-instr)

(define (divu-instr m)
  ; TODO: divu instruction not implemented yet
  (define pc (machine-pc m))
  (list 'divu))
(provide divu-instr)

(define (rem-instr m)
  ; TODO: rem instruction not implemented yet
  (define pc (machine-pc m))
  (list 'rem))
(provide rem-instr)

(define (remu-instr m)
  ; TODO: remu instruction not implemented yet
  (define pc (machine-pc m))
  (list 'remu))
(provide remu-instr)

(define (mulw-instr m)
  ; TODO: mulw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'mulw))
(provide mulw-instr)

(define (divw-instr m)
  ; TODO: divw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'divw))
(provide divw-instr)

(define (divuw-instr m)
  ; TODO: divuw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'divuw))
(provide divuw-instr)

(define (remw-instr m)
  ; TODO: remw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'remw))
(provide remw-instr)

(define (remuw-instr m)
  ; TODO: remuw instruction not implemented yet
  (define pc (machine-pc m))
  (list 'remuw))
(provide remuw-instr)


;; B Format
(define (beq-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bveq v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'beq rs1 rs2 imm))
(provide beq-instr)

(define (bne-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (not (bveq v_rs1 v_rs2))
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'bne rs1 rs2 imm))
(provide bne-instr)

(define (blt-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvslt v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'blt rs1 rs2 imm))
(provide blt-instr)

(define (bge-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvsge v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'bge rs1 rs2 imm))
(provide bge-instr)

(define (bltu-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvult v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'bltu rs1 rs2 imm))
(provide bltu-instr)

(define (bgeu-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvuge v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64))))
  (list 'bgeu rs1 rs2 imm))
(provide bgeu-instr)


; U Format
(define (lui-instr m rd imm)
  (define pc (machine-pc m))
  ; extend immediate by 12 bits
  (define se_imm (sign-extend (concat imm (bv 0 12)) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd se_imm)
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'lui rd imm))
(provide lui-instr)

(define (auipc-instr m rd imm)
  (define pc (machine-pc m))
  ; extend immediate by 12 bits, then sign-extend to 64 bits
  (define se_imm (sign-extend (concat imm (bv 0 12)) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvadd pc (base-address) se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'auipc rd imm))
(provide auipc-instr)


;; S Format
(define (sb-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define nbits 8)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin
        (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define se_imm (sign-extend imm (bitvector 64)))
        (define addr (bvadd v_rs1 se_imm))
        (define adj_addr (bvsub addr (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sb rs1 rs2 imm))
(provide sb-instr)

(define (sh-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define nbits 16)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin 
        (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define se_imm (sign-extend imm (bitvector 64)))
        (define addr (bvadd v_rs1 se_imm))
        (define adj_addr (bvsub addr (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sh rs1 rs2 imm))
(provide sh-instr)

(define (sw-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define nbits 32)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin 
        (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define se_imm (sign-extend imm (bitvector 64)))
        (define addr (bvadd v_rs1 se_imm))
        (define adj_addr (bvsub addr (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sw rs1 rs2 imm))
(provide sw-instr)

(define (sd-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (define nbits 64)
  (if (use-sym-optimizations)
      (begin
        (define-symbolic* v_rs2 (bitvector 64)) ; fetch arbitrary instruction
        (define-symbolic* adj_addr (bitvector 64)) ; fetch arbitrary instruction
        (machine-ram-write! m adj_addr v_rs2 nbits))
      (begin 
        (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
        (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
        (define se_imm (sign-extend imm (bitvector 64)))
        (define addr (bvadd v_rs1 se_imm))
        (define adj_addr (bvsub addr (base-address)))
        (machine-ram-write! m adj_addr v_rs2 nbits)))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'sd rs1 rs2 imm))
(provide sd-instr)


;; J Format
(define (jal-instr m rd imm)
  (define pc (machine-pc m))
  (define se_imm (sign-extend (concat imm (bv 0 1)) (bitvector 64)))
  ; adjust for (base-address)
  (define save_addr (bvadd pc (bv 4 64) (base-address)))
  ; imm is the offset from pc, so we don't need to do anything with (base-address)
  (define jump_addr (bvadd se_imm pc))
  (when (not (bvzero? rd))
    (set-gprs-i! (machine-gprs m) rd save_addr))
  (set-machine-pc! m jump_addr)
  (list 'jal rd imm))
(provide jal-instr)


;; FENCE Format
(define (FENCE-instr m)
  ; TODO: FENCE instruction not implemented yet
  (define pc (machine-pc m))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'FENCE))
(provide FENCE-instr)

(define (FENCE_I-instr m)
  ; TODO: FENCE_I instruction not implemented yet
  (define pc (machine-pc m))
  (set-machine-pc! m (bvadd pc (bv 4 64)))
  (list 'FENCE_I))
(provide FENCE_I-instr)
