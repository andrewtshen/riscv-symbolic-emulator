#lang rosette/safe

(require (only-in racket/base error))
(require "instr.rkt")

; Decode all of the binary instructions to a list similar to 
; objdump output so that it is easier to parse.

(define (decode-R b_instr)
	(define op null)
	(define rd (extract 11 7 b_instr))
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define rs2 (extract 24 20 b_instr))
	(define funct7 (extract 31 25 b_instr))
	(cond
		[(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0000000 7)))
			(set! op "add")]
		[(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0100000 7)))
			(set! op "sub")]
		[else (error "No such R op")])
	(list op rd rs1 rs2))

(define (decode-I b_instr)
	(define op null)
	(define opcode (extract 6 0 b_instr))
	(define rd (extract 11 7 b_instr))
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define imm (extract 31 20 b_instr))
	(cond
		[(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0010011 7)))
			(set! op "addi")]
		[(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lb")]
		[(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lh")]
		[(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lw")]
		[(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "ld")]
		[(and (bveq funct3 (bv #b100 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lbu")]
		[(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lhu")]
		[(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b0000011 7)))
			(set! op "lwu")]
		[else (error "No such I op")])
	(list op rd rs1 imm))

(define (decode-B b_instr)
	(define op null)
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define rs2 (extract 24 20 b_instr))
	; append upper imm and lower imm into imm
	(define imm (concat 
		(extract 31 31 b_instr) 
		(extract 7 7 b_instr)
		(extract 30 25 b_instr)
		(extract 11 8 b_instr)))
	(cond
		[(bveq funct3 (bv #b000 3))
			(set! op "beq")]
		[(bveq funct3 (bv #b001 3))
			(set! op "bne")]
		[(bveq funct3 (bv #b100 3))
			(set! op "blt")]
		[(bveq funct3 (bv #b101 3))
			(set! op "bge")]
		[(bveq funct3 (bv #b110 3))
			(set! op "bltu")]
		[(bveq funct3 (bv #b111 3))
			(set! op "bgeu")]
		[else
			(error "No such B op")])
		(list op rs1 rs2 imm))

(define (decode-U b_instr)
	(define op null)
	(define opcode (extract 6 0 b_instr))
	; append upper imm and lower imm into imm
	(define rd (extract 11 7 b_instr))
	(define imm (extract 31 12 b_instr))
	(cond
		[(bveq opcode (bv #b0110111 7))
			(set! op "lui")]
		[(bveq opcode (bv #b0010111 7))
			(set! op "auipc")]
		[else
			(error "No such U op")])
	(list op rd imm))

(define (decode-S b_instr)
	(define op null)
	(define opcode (extract 6 0 b_instr))
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define rs2 (extract 24 20 b_instr))
	(define imm (concat
		(extract 31 25 b_instr)
		(extract 11 7 b_instr)))
	(cond
		[(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sb")]
		[(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sh")]
		[(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sw")]
		[(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sd")]
		[else
			(error "No such S op")])
	(list op rs1 rs2 imm))

(define (decode-csr b_csr)
	(define csr null)
	(cond
		[(bveq b_csr (bv #x000 12))
			(set! csr "ustatus")]
		[(bveq b_csr (bv #x004 12))
			(set! csr "uie")]
		[(bveq b_csr (bv #x005 12))
			(set! csr "utevc")]
		[(bveq b_csr (bv #x040 12))
			(set! csr "uscratch")]
		[(bveq b_csr (bv #x041 12))
			(set! csr "uepc")]
		[(bveq b_csr (bv #x042 12))
			(set! csr "ucause")]
		[(bveq b_csr (bv #x043 12))
			(set! csr "ubadaddr")]
		[(bveq b_csr (bv #x044 12))
			(set! csr "uip")]
		[(bveq b_csr (bv #x300 12))
			(set! csr "mstatus")]
		[(bveq b_csr (bv #x301 12))
			(set! csr "misa")]
		[(bveq b_csr (bv #x302 12))
			(set! csr "medeleg")]
		[(bveq b_csr (bv #x303 12))
			(set! csr "mideleg")]
		[(bveq b_csr (bv #x304 12))
			(set! csr "mie")]
		[(bveq b_csr (bv #x305 12))
			(set! csr "mtevc")]
		[(bveq b_csr (bv #x340 12))
			(set! csr "mscratch")]
		[(bveq b_csr (bv #x341 12))
			(set! csr "mepc")]
		[(bveq b_csr (bv #x342 12))
			(set! csr "mcause")]
		[(bveq b_csr (bv #x343 12))
			(set! csr "mbadaddr")]
		[(bveq b_csr (bv #x344 12))
			(set! csr "mip")]
		[else
			error "No such CSR"])
	csr)

(define (decode-SPECIAL b_instr)
	(define op null)
	(define is_csr #f)
	(define opcode (extract 6 0 b_instr))
	(define rd (extract 11 7 b_instr))
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define csr (extract 31 20 b_instr))
	(cond
		[(bveq b_instr (bv #b00110000001000000000000001110011 32))
			(set! op "mret")]
		[(bveq b_instr (bv #b00000000001000000000000001110011 32))
			(set! op "uret")]
		[(bveq b_instr (bv #b00000000000000000000000001110011 32))
			(set! op "ecall")]
		[(bveq b_instr (bv #b00000000000100000000000001110011 32))
			(set! op "ebreak")]
		[(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrw")
			(set! is_csr #t)]
		[(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrs")
			(set! is_csr #t)]
		[(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrc")
			(set! is_csr #t)]
		[(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrwi")
			(set! is_csr #t)]
		[(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrsi")
			(set! is_csr #t)]
		[(and (bveq funct3 (bv #b111 3)) (bveq opcode (bv #b1110011 7)))
			(set! op "csrrci")
			(set! is_csr #t)]
		[else
			(error "No such SPECIAL op")])

	(printf "OPCODE ~a" opcode)
	; (list op rd rs1 csr))
	(if is_csr
		(list op rd rs1 (decode-csr csr))
		(list op)))

; decode a 32 bit vector instruction
(define (decode b_instr)
	; (printf "decoding: ~a~n" b_instr)
	(define instr null)
	(define opcode (extract 6 0 b_instr))
	(define fmt (get-fmt opcode))
	(printf "FMT: ~a " fmt)

	(cond
		[(equal? fmt "R")
			(set! instr (decode-R b_instr))]
		[(equal? fmt "I")
			(set! instr (decode-I b_instr))]
		[(equal? fmt "B")
			(set! instr (decode-B b_instr))]
		[(equal? fmt "U")
			(set! instr (decode-U b_instr))]
		[(equal? fmt "S")
			(set! instr (decode-S b_instr))]
		[(equal? fmt "SPECIAL")
			(set! instr (decode-SPECIAL b_instr))]
		[else
			(error "No match for FMT")])
	instr)
(provide decode)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define instr (decode b_instr))
; (printf "~a~n" instr)