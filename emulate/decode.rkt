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
		[else (error "no such op exists")])
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
		[else (error "no such op exists")])
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
			(set! op "bgeu")])
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
			(set! op "auipc")])
	(list op rd imm))

(define (decode-S b_instr)
	(define op null)
	(define opcode (extract 6 0 b_instr))
	(define funct3 (extract 14 12 b_instr))
	(define rs1 (extract 19 15 b_instr))
	(define rs2 (extract 24 20 b_instr))
	(define imm (concat
		(extract 31 31 b_instr)
		(extract 7 7 b_instr)
		(extract 30 25 b_instr)
		(extract 11 8 b_instr)))
	(cond
		[(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sb")]
		[(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sh")]
		[(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sw")]
		[(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0100011 7)))
			(set! op "sd")])
	(list op rs1 rs2 imm))

(define (decode-RET b_instr)
	(define op null)
	(cond
		[(bveq b_instr (bv #b00110000001000000000000001110011 32))
			(set! op "mret")]
		[(bveq b_instr (bv #b00000000001000000000000001110011 32))
			(set! op "uret")])
	(list op))

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
		[(equal? fmt "RET")
			(set! instr (decode-RET b_instr))])
	instr)
(provide decode)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define instr (decode b_instr))
; (printf "~a~n" instr)