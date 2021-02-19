#lang rosette 

(require
  "init.rkt"
  "machine.rkt"
  "parameters.rkt") 

; Execute each individual instruction symbolically and update the program count to the proper place.
; Used rv8.io and https://content.riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf for implementing instructions
; Conventions used for decoding are as follows: v_var represents the bitvector value stored in register var. Otherwise,
; the variable var contains the index into the register that it refers to.

; Execute Instruction
; SPECIAL Format
(define (ecall-instr m)
  ; TODO: real ecall implementation
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'ecall)))
(provide ecall-instr)

(define (ebreak-instr m)
  ; TODO: ebreak instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'ebreak)))
(provide ebreak-instr)

(define (uret-instr m)
  ; TODO: uret instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'uret)))
(provide uret-instr)

(define (mret-instr m)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mret))
  (define mstatus (machine-csr m 'mstatus))
  (define MPP (extract 12 11 mstatus))
  ; this is always user mode
  ; TODO: fix this and set mstatus to its actual value of MPP, for now we are setting to 0
  ; since we always but it to user mode
  ; (set-machine-mode! m (bitvector->natural MPP))
  (set-machine-mode! m 0)
  (set-machine-pc! m (bvsub (machine-csr m 'mepc) (base-address))))
(provide mret-instr)

(define (dret-instr m)
  ; TODO: dret instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'dret)))
(provide dret-instr)

(define (sfence_vma-instr m)
  ; TODO: sfence_vma instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'sfence_vma)))
(provide sfence_vma-instr)

(define (wfi-instr m)
  ; TODO: wfi instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'wfi)))
(provide wfi-instr)

(define (csrrw-instr m rd rs1 csr)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'csrrw rd rs1 csr))
  (when (equal? (machine-mode m) 1)
    (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
    (when (not (zero? rd))
      (define v_csr (machine-csr m csr))
      (set-gprs-i! (machine-gprs m) rd (zero-extend v_csr (bitvector 64))))
    ; TODO: Implement specific setting permissions for CSR bits
    (set-machine-csr! m csr v_rs1)
    (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide csrrw-instr)

(define (csrrs-instr m rd rs1 csr)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'csrrs rd rs1 csr))
  (when (equal? (machine-mode m) 1)
    (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
    (define v_csr (machine-csr m csr))
    (set-gprs-i! (machine-gprs m) rd (zero-extend v_csr (bitvector 64)))
    
    ; TODO: Implement specific setting permissions for CSR bits
    (when (not (zero? rs1))
      (set-machine-csr! m csr (bvor v_csr v_rs1)))
    (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide csrrs-instr)

(define (csrrc-instr m)
  ; TODO: csrrc instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'csrrc)))
(provide csrrc-instr)

(define (csrrwi-instr m)
  ; TODO: csrrwi instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'csrrwi)))
(provide csrrwi-instr)

(define (csrrsi-instr m)
  ; TODO: csrrsi instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'csrrsi)))
(provide csrrsi-instr)

(define (csrrci-instr m)
  ; TODO: csrrci instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'csrrci)))
(provide csrrci-instr)


; I Format
(define (addi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'addi rd rs1 imm))
  (define se_imm (sign-extend imm (bitvector 64)))
  ; nop op pseudo code condition
  (cond
    [(not (and (equal? rd 0) (equal? rs1 0) (bveq se_imm (bv 0 64))))
     (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
     (when (not (eq? v_rs1 null))
       (set-machine-pc! m (bvadd pc (bv 4 64)))
       (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 se_imm)))]
    [else
     (set-machine-pc! m (bvadd pc (bv 4 64)))]))
(provide addi-instr)

(define (slli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'slli rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvshl v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide slli-instr)

(define (srli-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'srli))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvlshr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide srli-instr)

(define (srai-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'srai rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define ze_imm (zero-extend imm (bitvector 64)))
  (define shifted (bvashr v_rs1 ze_imm))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide srai-instr)

(define (slti-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'slti rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (if (bvslt v_rs1 se_imm)
      (set-gprs-i! (machine-gprs m) rd 1)
      (set-gprs-i! (machine-gprs m) rd 0))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide slti-instr)

(define (sltiu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sltiu rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (if (bvult v_rs1 imm)
      (set-gprs-i! (machine-gprs m) rd 1)
      (set-gprs-i! (machine-gprs m) rd 0))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sltiu-instr)

(define (xori-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'xori rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvxor v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide xori-instr)

(define (ori-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a  ~a ~a ~a~n" 'ori rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvor v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide ori-instr)

(define (andi-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'andi))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 se_imm))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide andi-instr)

(define (addiw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'addiw rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define 32bit_sum (extract 31 0 (bvadd v_rs1 se_imm)))
  (set-gprs-i! (machine-gprs m) rd (sign-extend 32bit_sum (bitvector 64)))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide addiw-instr)

(define (slliw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'slliw rd rs1 imm))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvshl v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      null ; TODO: Generate Illegal Instruction 
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide slliw-instr)

(define (srliw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'srliw rd rs1 imm))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvlshr v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      null ; TODO: Generate Illegal Instruction
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide srliw-instr)

(define (sraiw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sraiw rd rs1 imm))
  (define v_rs1 (extract 31 0 (get-gprs-i (machine-gprs m) rs1)))
  (define ze_imm (zero-extend imm (bitvector 32)))
  (define shifted (sign-extend (bvashr v_rs1 ze_imm) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (if (not (bveq (extract 5 5 imm) (bv 0 1)))
      null ; TODO: Generate Illegal Instruction
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide sraiw-instr)

(define (lb-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'm rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 1)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lb-instr)

(define (lh-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'm rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 2)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lh-instr)

(define (lw-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'm rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 4)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lw-instr)

(define (ld-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'm rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 8)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide ld-instr)

(define (lbu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'lbu rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 1)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lbu-instr)

(define (lhu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'lhu rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 2)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lhu-instr)

(define (lwu-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'lwu rd rs1 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  (define addr (bvadd v_rs1 se_imm))
  (define adj_addr (bvsub addr (base-address)))
  (define nbytes 4)
  ; stronger case that covers all possible values that val can take
  (define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
  (set-gprs-i! (machine-gprs m) rd val)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lwu-instr)

(define (jalr-instr m rd rs1 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'jalr))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define se_imm (sign-extend imm (bitvector 64)))
  
  ; remove least significant bit
  (define addr (bvand (bvadd v_rs1 se_imm) (bvnot (bv 1 64))))
  ; adjust for address offset
  (define adj_addr (bvsub addr (base-address)))
  
  (define save (bvadd pc (bv 4 64)))
  (when (not (equal? rd 0))
    (set-gprs-i! (machine-gprs m) rd save))
  (set-machine-pc! m adj_addr))
(provide jalr-instr)


; R Format
(define (add-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'add rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvadd v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide add-instr)

(define (sub-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sub rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvsub v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sub-instr)

(define (sll-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sll rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (define shifted (bvshl v_rs1 v_rs2))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sll-instr)

(define (slt-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'slt rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvslt v_rs1 v_rs2)
      (set-gprs-i! (machine-gprs m) rd 1)
      (set-gprs-i! (machine-gprs m) rd 0))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide slt-instr)

(define (sltu-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sltu rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvult v_rs1 v_rs2)
      (set-gprs-i! (machine-gprs m) rd 1)
      (set-gprs-i! (machine-gprs m) rd 0))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sltu-instr)

(define (xor-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'xor rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvxor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide xor-instr)

(define (srl-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'srl rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (define shifted (bvlshr v_rs1 v_rs2))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide srl-instr)

(define (sra-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sra rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (define shifted (bvashr v_rs1 v_rs2))
  (set-gprs-i! (machine-gprs m) rd shifted)
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sra-instr)

(define (or-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'm rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvor v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide or-instr)

(define (and-instr m rd rs1 rs2)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'and rd rs1 rs2))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (set-gprs-i! (machine-gprs m) rd (bvand v_rs1 v_rs2))
  (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide and-instr)

(define (andw-instr m)
  ; TODO: andw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'andw)))
(provide andw-instr)

(define (subw-instr m)
  ; TODO: subw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'subw)))
(provide subw-instr)

(define (sllw-instr m)
  ; TODO: sllw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'sllw)))
(provide sllw-instr)

(define (srlw-instr m)
  ; TODO: srlw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'srlw)))
(provide srlw-instr)

(define (sraw-instr m)
  ; TODO: sraw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'sraw)))
(provide sraw-instr)

(define (mul-instr m)
  ; TODO: mul instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mul)))
(provide mul-instr)

(define (mulh-instr m)
  ; TODO: mulh instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mulh)))
(provide mulh-instr)

(define (mulhsu-instr m)
  ; TODO: mulhsu instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mulhsu)))
(provide mulhsu-instr)

(define (mulhu-instr m)
  ; TODO: mulhu instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mulhu)))
(provide mulhu-instr)

(define (div-instr m)
  ; TODO: div instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'div)))
(provide div-instr)

(define (divu-instr m)
  ; TODO: divu instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'divu)))
(provide divu-instr)

(define (rem-instr m)
  ; TODO: rem instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'rem)))
(provide rem-instr)

(define (remu-instr m)
  ; TODO: remu instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'remu)))
(provide remu-instr)

(define (mulw-instr m)
  ; TODO: mulw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'mulw)))
(provide mulw-instr)

(define (divw-instr m)
  ; TODO: divw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'divw)))
(provide divw-instr)

(define (divuw-instr m)
  ; TODO: divuw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'divuw)))
(provide divuw-instr)

(define (remw-instr m)
  ; TODO: remw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'remw)))
(provide remw-instr)

(define (remuw-instr m)
  ; TODO: remuw instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'remuw)))
(provide remuw-instr)


; B Format
(define (beq-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'beq rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m)rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m)rs2))
  (if (equal? v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide beq-instr)

(define (bne-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'bne rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m)rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m)rs2))
  (if (not (equal? v_rs1 v_rs2))
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide bne-instr)

(define (blt-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'blt rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m)rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m)rs2))
  (if (bvslt v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide blt-instr)

(define (bge-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'bge rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m)rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m)rs2))
  (if (bvsge v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide bge-instr)

(define (bltu-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'bltu rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvult v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide bltu-instr)

(define (bgeu-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'bgeu rs1 rs2 imm))
  (define v_rs1 (get-gprs-i (machine-gprs m) rs1))
  (define v_rs2 (get-gprs-i (machine-gprs m) rs2))
  (if (bvuge v_rs1 v_rs2)
      (set-machine-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
      (set-machine-pc! m (bvadd pc (bv 4 64)))))
(provide bgeu-instr)


; U Format
(define (lui-instr m rd imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a~n" 'lui rd imm))
 ; extend immediate by 12 bits
 (define ze_imm (zero-extend (concat imm (bv 0 12)) (bitvector 64)))
 (set-gprs-i! (machine-gprs m) rd ze_imm)
 (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide lui-instr)

(define (auipc-instr m rd imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a~n" 'auipc rd imm))
 ; extend immediate by 12 bits, then zero-extend to 64 bits
 (define ze_imm (zero-extend (concat imm (bv 0 12)) (bitvector 64)))
 (set-gprs-i! (machine-gprs m) rd (bvadd pc (base-address) ze_imm))
 (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide auipc-instr)


; S Format
(define (sb-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sb rs1 rs2 imm))
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
    (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sb-instr)

(define (sh-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sh rs1 rs2 imm))
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
    (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sh-instr)

(define (sw-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sw rs1 rs2 imm))
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
    (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sw-instr)

(define (sd-instr m rs1 rs2 imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a ~a~n" 'sd rs1 rs2 imm))
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
    (set-machine-pc! m (bvadd pc (bv 4 64))))
(provide sd-instr)


; J Format
(define (jal-instr m rd imm)
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a ~a ~a~n" 'jal rd imm))
 (define se_imm (sign-extend (concat imm (bv 0 1)) (bitvector 64)))
 ; adjust for (base-address)
 (define save_addr (bvadd (bvadd pc (bv 4 64)) (base-address)))
 ; imm is the offset from pc, so we don't need to do anything with (base-address)
 (define jump_addr (bvadd se_imm pc))
 (when (not (equal? rd 0))
   (set-gprs-i! (machine-gprs m) rd save_addr))
 (set-machine-pc! m jump_addr))
(provide jal-instr)


; FENCE Format
(define (FENCE-instr m)
  ; TODO: FENCE instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'FENCE)))
(provide FENCE-instr)

(define (FENCE_I-instr m)
  ; TODO: FENCE_I instruction not implemented yet
  (define pc (machine-pc m))
  (when (debug-instr)
    (printf "~a~n" 'FENCE_I)))
(provide FENCE_I-instr)
