#lang rosette/safe

(require "load.rkt")

; Execute each individual instruction symbolically
; and update the program count to the proper place.
; Used rv8.io for implementing instructions

(define (list-ref-nat instr idx)
	(bitvector->natural (list-ref instr idx)))

(define (execute instr m)
	(define opcode (list-ref instr 0))
	(define pc (get-pc m))
	(cond
		; RET Fromat
		[(equal? opcode "mret")
			(set-pc! m 0)]

		; I Format
		[(equal? opcode "addi")
			(define rd (bitvector->natural (list-ref instr 1)))
			(define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvadd rs1 imm))
			(set-pc! m (+ pc 2))]

		; R Format
		[(equal? opcode "add")
			(define rd (list-ref-nat instr 1))
			(define rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvadd rs1 rs2))
			(set-pc! m (+ pc 2))]
		[(equal? opcode "sub")
			(define rd (bitvector->natural (list-ref instr 1)))
			(define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
			(define rs2 (gprs-get-x m (bitvector->natural (list-ref instr 3))))
			(gprs-set-x! m rd (bvsub rs1 rs2))
			(set-pc! m (+ pc 2))]

		; B Format
		[(equal? opcode "beq")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (equal? rs1 rs2)
				(set-pc! m (+ pc (/ offset 2)))
				(set-pc! m (+ pc 2)))]
		[(equal? opcode "bne")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (not (equal? rs1 rs2))
				(set-pc! m (+ pc (/ offset 2)))
				(set-pc! m (+ pc 2)))]
		[(equal? opcode "blt")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (bvslt rs1 rs2)
				(set-pc! m (+ pc (/ offset 2)))
				(set-pc! m (+ pc 2)))]
		[(equal? opcode "bge")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (bvsge rs1 rs2)
				(set-pc! m (+ pc (/ offset 2)))
				(set-pc! m (+ pc 2)))]
		[(equal? opcode "bltu")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (bvult rs1 rs2)
				(set-pc! m (+ pc (/ offset 2)))
				(set-pc! m (+ pc 2)))]
		[(equal? opcode "bgeu")
			(define rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define rs2 (gprs-get-x m (list-ref-nat instr 2)))
			; append upper imm and lower imm into imm
			(define offset (list-ref-nat instr 3))
			; since each unit in ram is 2 bytes, we divide by 2 to advance the PC
			(assert (equal? (modulo offset 2) 0)) ; check that we are working with blocks of 2
			(if (bvuge rs1 rs2)
				(set-pc! m (+ pc offset))
					(set-pc! m (+ pc 2)))]

		; U Format
		[(equal? opcode "auipc")
			(define rd (list-ref-nat instr 1))
			(define imm (zero-extend (list-ref instr 2) (bitvector 64)))
			(gprs-set-x! m rd (bvadd (bv pc 64) imm))
			(set-pc! m (+ pc 2))]

	))
(provide execute)
