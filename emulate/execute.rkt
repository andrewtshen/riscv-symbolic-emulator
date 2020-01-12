#lang rosette/safe

(require (only-in racket/base error))

(require "load.rkt")

; Execute each individual instruction symbolically
; and update the program count to the proper place.
; Used rv8.io for implementing instructions

; helper to convert bitvectors to naturals after indexing instruction
(define (list-ref-nat instr idx)
	(bitvector->natural (list-ref instr idx)))

; execute symbolic instruction
(define (execute instr m)
	(define opcode (list-ref instr 0))
	(define pc (get-pc m))
	(cond
		; RET Format
		[(equal? opcode "ecall")
			(error "slti instruction not implemented yet")]
		[(equal? opcode "ebreak")
			(error "sltiu instruction not implemented yet")]
		[(equal? opcode "uret")
			(error "xori instruction not implemented yet")]
		[(equal? opcode "mret")
			(set-pc! m 0)]
		[(equal? opcode "dret")
			(error "srai instruction not implemented yet")]
		[(equal? opcode "sfence_vma")
			(error "ori instruction not implemented yet")]
		[(equal? opcode "wfi")
			(error "andi instruction not implemented yet")]
		[(equal? opcode "csrrw")
			(error "addiw instruction not implemented yet")]
		[(equal? opcode "csrrs")
			(error "slliw instruction not implemented yet")]
		[(equal? opcode "csrrc")
			(error "srliw instruction not implemented yet")]
		[(equal? opcode "csrrwi")
			(error "sraiw instruction not implemented yet")]
		[(equal? opcode "csrrsi")
			(error "lb instruction not implemented yet")]
		[(equal? opcode "csrrci")
			(error "lh instruction not implemented yet")]

		; I Format
		[(equal? opcode "addi")
			(define rd (bitvector->natural (list-ref instr 1)))
			(define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvadd rs1 imm))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "lui")
			(error "lui instruction not implemented yet")]
		[(equal? opcode "slti")
			(error "slti instruction not implemented yet")]
		[(equal? opcode "sltiu")
			(error "sltiu instruction not implemented yet")]
		[(equal? opcode "xori")
			(error "xori instruction not implemented yet")]
		[(equal? opcode "slri")
			(error "slri instruction not implemented yet")]
		[(equal? opcode "srai")
			(error "srai instruction not implemented yet")]
		[(equal? opcode "ori")
			(error "ori instruction not implemented yet")]
		[(equal? opcode "andi")
			(error "andi instruction not implemented yet")]
		[(equal? opcode "addiw")
			(error "addiw instruction not implemented yet")]
		[(equal? opcode "slliw")
			(error "slliw instruction not implemented yet")]
		[(equal? opcode "srliw")
			(error "srliw instruction not implemented yet")]
		[(equal? opcode "sraiw")
			(error "sraiw instruction not implemented yet")]
		[(equal? opcode "lb")
			(error "lb instruction not implemented yet")]
		[(equal? opcode "lh")
			(error "lh instruction not implemented yet")]
		[(equal? opcode "lw")
			(error "lw instruction not implemented yet")]
		[(equal? opcode "ld")
			(error "ld instruction not implemented yet")]
		[(equal? opcode "lbu")
			(error "lbu instruction not implemented yet")]
		[(equal? opcode "lhu")
			(error "lhu instruction not implemented yet")]
		[(equal? opcode "lwu")
			(error "lwu instruction not implemented yet")]

		; R Format
		[(equal? opcode "add")
			(define rd (list-ref-nat instr 1))
			(define rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvadd rs1 rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sub")
			(define rd (bitvector->natural (list-ref instr 1)))
			(define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
			(define rs2 (gprs-get-x m (bitvector->natural (list-ref instr 3))))
			(gprs-set-x! m rd (bvsub rs1 rs2))
			(set-pc! m (+ pc 4))]
		[(equal? opcode "sll")
			(error "slt instruction not implemented yet")]
		[(equal? opcode "slt")
			(error "sltu instruction not implemented yet")]
		[(equal? opcode "sltu")
			(error "xor instruction not implemented yet")]
		[(equal? opcode "xor")
			(error "srl instruction not implemented yet")]
		[(equal? opcode "srl")
			(error "sra instruction not implemented yet")]
		[(equal? opcode "sra")
			(error "or instruction not implemented yet")]
		[(equal? opcode "or")
			(error "add instruction not implemented yet")]
		[(equal? opcode "and")
			(error "and instruction not implemented yet")]
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
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(if (equal? rs1 rs2)
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bne")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(if (not (equal? rs1 rs2))
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "blt")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(if (bvslt rs1 rs2)
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bge")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(if (bvsge rs1 rs2)
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bltu")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(if (bvult rs1 rs2)
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]
		[(equal? opcode "bgeu")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			(printf "rs1: ~a~n" rs1)
			(printf "rs2: ~a~n" rs2)
			(printf "offset: ~a~n" offset)
			(if (bvuge rs1 rs2)
				(set-pc! m (+ pc (* offset 2)))
				(set-pc! m (+ pc 4)))]

		; U Format
		[(equal? opcode "lui")
			(error "lui instruction not implemented yet")]
		[(equal? opcode "auipc")
			(define rd (list-ref-nat instr 1))
			(define imm (zero-extend (list-ref instr 2) (bitvector 64)))
			(gprs-set-x! m rd (bvadd (bv pc 64) imm))
			(set-pc! m (+ pc 4))]

		; S Format
		[(equal? opcode "sb")
			(error "sb instruction not implemented yet")]
		[(equal? opcode "sh")
			(error "sh instruction not implemented yet")]
		[(equal? opcode "sw")
			(error "sw instruction not implemented yet")]
		[(equal? opcode "sd")
			(error "sd instruction not implemented yet")]

		; FENCE Format
		[(equal? opcode "FENCE")
			(error "FENCE instruction not implemented yet")]
		[(equal? opcode "FENCE_I")
			(error "FENCE_I instruction not implemented yet")]
	))
(provide execute)
