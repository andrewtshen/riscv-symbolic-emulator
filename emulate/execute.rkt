#lang rosette/safe

(require (only-in racket/base error))

(require 
	"load.rkt"
	"machine.rkt")

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
	(define opcode (list-ref instr 0))
	(define pc (get-pc m))
	(cond
		; RET Format
		[(equal? opcode "ecall")
			(error "ecall instruction not implemented yet")]
		[(equal? opcode "ebreak")
			(error "ebreak instruction not implemented yet")]
		[(equal? opcode "uret")
			(error "uret instruction not implemented yet")]
		[(equal? opcode "mret")
			(set-pc! m 0)]
		[(equal? opcode "dret")
			(error "dret instruction not implemented yet")]
		[(equal? opcode "sfence_vma")
			(error "sfence_vma instruction not implemented yet")]
		[(equal? opcode "wfi")
			(error "wfi instruction not implemented yet")]
		[(equal? opcode "csrrw")
			(define rd (list-ref-nat instr 1))
			(define rs1 (list-ref-nat instr 2))
			(define v_rs1 (gprs-get-x m rs1))
			(define csr (list-ref instr 3))
			(cond
				[(zero? rd) null]
				[else
					(define v_csr (zero-extend (get-csr m csr) (bitvector 64)))
					(gprs-set-x! m rd v_csr)])
			(set-csr! m (list-ref instr 3) (zero-extend v_rs1 (bitvector 64)))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "csrrs")
			(define rd (list-ref-nat instr 1))
			(define rs1 (list-ref-nat instr 2))
			(define bitmask (gprs-get-x m rs1))
			(define csr (list-ref instr 3))
			(define v_csr (zero-extend (get-csr m csr) (bitvector 64)))
			(gprs-set-x! m rd v_csr)
			
			(set-csr! m csr (bvor v_csr bitmask))
			(set! v_csr (zero-extend (get-csr m csr) (bitvector 64)))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "csrrc")
			(error "csrrc instruction not implemented yet")]
		[(equal? opcode "csrrwi")
			(error "csrrwi instruction not implemented yet")]
		[(equal? opcode "csrrsi")
			(error "csrrsi instruction not implemented yet")]
		[(equal? opcode "csrrci")
			(error "csrrci instruction not implemented yet")]

		; I Format
		[(equal? opcode "addi")
			(define rd (list-ref-nat instr 1))
			(define rs1 (list-ref-nat instr 2))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			; nop op pseudo code
			(if (and (equal? rd 0) (equal? rs1 0) (bveq imm (bv 0 64)))
				(set-pc! m (+ pc 4))
				(gprs-set-x! m rd (bvadd (gprs-get-x m rs1) imm)))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "slli")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvshl v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "srli")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvlshr v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "srai")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvashr v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "slti")
			(error "slti instruction not implemented yet")]
		[(equal? opcode "sltiu")
			(error "sltiu instruction not implemented yet")]
		[(equal? opcode "xori")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvxor v_rs1 imm))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "ori")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvor v_rs1 imm))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "andi")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvand v_rs1 imm))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "addiw")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define 32bit_sum (extract 31 0 (bvadd v_rs1 imm)))
			(gprs-set-x! m rd (sign-extend 32bit_sum (bitvector 64)))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "slliw")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvshl v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(cond
				[(not (bveq (extract 5 5 imm) (bv 0 1)))
					(error "TODO generate illegal instruction error")])
			(set-pc! m (+ pc 4))]
		[(equal? opcode "srliw")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvlshr v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(cond
				[(not (bveq (extract 5 5 imm) (bv 0 1)))
					(error "TODO generate illegal instruction error")])
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sraiw")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvashr v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(cond
				[(not (bveq (extract 5 5 imm) (bv 0 1)))
					(error "TODO generate illegal instruction error")])
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lb")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 1)
			(define val (sign-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lh")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 2)
			(define val (sign-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lw")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 4)
			(define val (sign-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "ld")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 8)
			(define val (bytearray-read (machine-ram m) adj_addr nbytes))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lbu")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 1)
			(define val (zero-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lhu")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 2)
			(define val (zero-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lwu")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbytes 4)
			(define val (zero-extend (bytearray-read (machine-ram m) adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "jalr")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			; addr is bitvector 64 correct address
			(define addr (bvand (bvadd v_rs1 imm) (bvnot (bv 1 64))))
			; adj_addr is adjusted for offset
			(define adj_addr (- (bitvector->natural addr) base_address))

			(define save (bv (+ pc 4) 64))
			(cond
				[(not (equal? rd 0))
					(gprs-set-x! m rd save)])

			(set-pc! m adj_addr)]

		; R Format
		[(equal? opcode "add")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvadd v_rs1 v_rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sub")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvsub v_rs1 v_rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sll")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvshl v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "slt")
			(error "sltu instruction not implemented yet")]
		[(equal? opcode "sltu")
			(error "xor instruction not implemented yet")]
		[(equal? opcode "xor")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvxor v_rs1 v_rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "srl")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvlshr v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sra")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvashr v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "or")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvor v_rs1 v_rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "and")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvand v_rs1 v_rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "andw")
			(error "and instruction not implemented yet")]
		[(equal? opcode "subw")
			(error "subw instruction not implemented yet")]
		[(equal? opcode "sllw")
			(error "sllw instruction not implemented yet")]
		[(equal? opcode "srlw")
			(error "srlw instruction not implemented yet")]
		[(equal? opcode "sraw")
			(error "sraw instruction not implemented yet")]
		[(equal? opcode "mul")
			(error "mul instruction not implemented yet")]
		[(equal? opcode "mulh")
			(error "mulh instruction not implemented yet")]
		[(equal? opcode "mulhsu")
			(error "mulhsu instruction not implemented yet")]
		[(equal? opcode "mulhu")
			(error "mulhu instruction not implemented yet")]
		[(equal? opcode "div")
			(error "div instruction not implemented yet")]
		[(equal? opcode "divu")
			(error "divu instruction not implemented yet")]
		[(equal? opcode "rem")
			(error "rem instruction not implemented yet")]
		[(equal? opcode "remu")
			(error "remu instruction not implemented yet")]
		[(equal? opcode "mulw")
			(error "mulw instruction not implemented yet")]
		[(equal? opcode "divw")
			(error "divw instruction not implemented yet")]
		[(equal? opcode "divuw")
			(error "divuw instruction not implemented yet")]
		[(equal? opcode "remw")
			(error "remw instruction not implemented yet")]
		[(equal? opcode "remuw")
			(error "sraw instruction not implemented yet")]

		; B Format
		[(equal? opcode "beq")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (equal? v_rs1 v_rs2)
				(set-pc! m (+ pc (* imm 2)))
					(set-pc! m (+ pc 4)))]
		[(equal? opcode "bne")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (not (equal? v_rs1 v_rs2))
				(set-pc! m (+ pc (* imm 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "blt")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (bvslt v_rs1 v_rs2)
				(set-pc! m (+ pc (* imm 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bge")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (bvsge v_rs1 v_rs2)
				(set-pc! m (+ pc (* imm 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bltu")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (bvult v_rs1 v_rs2)
				(set-pc! m (+ pc (* imm 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bgeu")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref-int instr 3))
			(if (bvuge v_rs1 v_rs2)
				(set-pc! m (+ pc (* imm 2)))
				(set-pc! m (+ pc 4)))]

		; U Format
		[(equal? opcode "lui")
			(define rd (list-ref-nat instr 1))
			; extend immediate by 12 bits
			(define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
			(gprs-set-x! m rd imm)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "auipc")
			(define rd (list-ref-nat instr 1))
			; extend immdiate by 3 bytes/12 bits
			(define imm (zero-extend (concat (list-ref instr 2) (bv 0 3)) (bitvector 64)))
			(gprs-set-x! m rd (bvadd (bv pc 64) (bv base_address 64) imm))
			(set-pc! m (+ pc 4))]

		; S Format
		[(equal? opcode "sb")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbits 8)
			(bytearray-write! (machine-ram m) adj_addr v_rs2 nbits)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sh")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbits 16)
			(bytearray-write! (machine-ram m) adj_addr v_rs2 nbits)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sw")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbits 32)
			(bytearray-write! (machine-ram m) adj_addr v_rs2 nbits)
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sd")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bitvector->natural (bvadd v_rs1 imm)))
			(define adj_addr (- addr base_address))
			(define nbits 64)
			(bytearray-write! (machine-ram m) adj_addr v_rs2 nbits)
			(set-pc! m (+ pc 4))]

		; J Format
		[(equal? opcode "jal")
			(define rd (list-ref-nat instr 1))
			(define imm (sign-extend (concat (list-ref instr 2) (bv 0 1)) (bitvector 64)))
			; adjust for base_address
			(define save_addr (bvadd (bv (+ pc 4) 64) (bv base_address 64)))
			; imm is the offset from pc, so we don't need to do anything with base_address
			(define jump_addr (bitvector->natural (bvadd imm (bv pc 64))))
			(cond
				[(not (equal? rd 0))
					(gprs-set-x! m rd save_addr)])
			(set-pc! m jump_addr)]

		; FENCE Format
		[(equal? opcode "FENCE")
			(error "FENCE instruction not implemented yet")]
		[(equal? opcode "FENCE_I")
			(error "FENCE_I instruction not implemented yet")]
	))
(provide execute)
