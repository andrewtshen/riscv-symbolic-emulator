#lang rosette

(require
	"init.rkt"
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
	; (printf "instr: ~a~n" instr)
	; (printf "opcode: ~a~n" opcode)

	(cond
		; SPECIAL Format
		[(eq? opcode 'ecall)
			; (printf " --> ecall ~n")
			; TODO: real ecall implementation
			(illegal-instr m)]
		[(eq? opcode 'ebreak)
			; (printf " --> ebreak ~n")
			; TODO: ebreak instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'uret)
			; (printf " --> uret ~n")
			; TODO: uret instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mret)
			; (printf " --> mret ~n")
			(define mstatus (get-csr m 'mstatus))
			(define MPP (extract 12 11 mstatus))
			; this is always user mode
			; TODO: fix this and set mstatus to concrete value
			; (set-machine-mode! m (bitvector->natural MPP))
			(set-machine-mode! m 0)
			(set-pc! m (bvsub (get-csr m 'mepc) base_address))
			instr]
		[(eq? opcode 'dret)
			; (printf " --> dret ~n")
			; TODO: dret instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'sfence_vma)
			; (printf " --> sfence_vma ~n")
			; TODO: sfence_vma instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'wfi)
			; (printf " --> wfi ~n")
			; TODO: wfi instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'csrrw)
			; (printf " --> csrrw ~n")
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
			; (printf " --> csrrs ~n")
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
			; (printf " --> csrrc ~n")
			; TODO: csrrc instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'csrrwi)
			; (printf " --> csrrwi ~n")
			; TODO: csrrwi instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'csrrsi)
			; (printf " --> csrrsi ~n")
			; TODO: csrrsi instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'csrrci)
			; (printf " --> csrrci ~n")
			; TODO: csrrci instruction not implemented yet
			(illegal-instr m)]

		; I Format
		[(eq? opcode 'addi)
			; (printf " --> addi ~n")
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
			; (printf " --> slli ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvshl v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'srli)
			; (printf " --> srli ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvlshr v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'srai)
			; (printf " --> srai ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (zero-extend (list-ref instr 3) (bitvector 64)))
			(define shifted (bvashr v_rs1 imm))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'slti)
			; (printf " --> slti ~n")
			; TODO: slti instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'sltiu)
			; (printf " --> sltiu ~n")
			; TODO: sltiu instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'xori)
			; (printf " --> xori ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvxor v_rs1 imm))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'ori)
			; (printf " --> ori ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvor v_rs1 imm))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'andi)
			; (printf " --> andi ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(gprs-set-x! m rd (bvand v_rs1 imm))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'addiw)
			; (printf " --> addiw ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define 32bit_sum (extract 31 0 (bvadd v_rs1 imm)))
			(gprs-set-x! m rd (sign-extend 32bit_sum (bitvector 64)))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'slliw)
			; (printf " --> slliw ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvshl v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(when (not (bveq (extract 5 5 imm) (bv 0 1)))
				(illegal-instr m))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'srliw)
			; (printf " --> srliw ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvlshr v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(when (not (bveq (extract 5 5 imm) (bv 0 1)))
				(illegal-instr m))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'sraiw)
			; (printf " --> sraiw ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (extract 31 0 (gprs-get-x m (list-ref-nat instr 2))))
			(define imm (zero-extend (list-ref instr 3) (bitvector 32)))
			(define shifted (sign-extend (bvashr v_rs1 imm) (bitvector 64)))
			(gprs-set-x! m rd shifted)
			(when (not (bveq (extract 5 5 imm) (bv 0 1)))
					(illegal-instr m))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lb)
			; (printf " --> lb ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 1)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lh)
			; (printf " --> lh ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 2)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lw)
			; (printf " --> lw ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 4)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'ld)
			; (printf " --> ld ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 8)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lbu)
			; (printf " --> lbu ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 1)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lhu)
			; (printf " --> lhu ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 2)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'lwu)
			; (printf " --> lwu ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbytes 4)
			; stronger case that covers all possible values that val can take
			(define val (sign-extend (machine-ram-read m adj_addr nbytes) (bitvector 64)))
			(gprs-set-x! m rd val)
				(set-pc! m (bvadd pc (bv 4 64)))
				instr]
		[(eq? opcode 'jalr)
			; (printf " --> jalr ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			; addr is bitvector 64 correct address
			(define addr (bvand (bvadd v_rs1 imm) (bvnot (bv 1 64))))
			; adj_addr is adjusted for offset
			(define adj_addr (bvsub addr base_address))

			(define save (bvadd pc (bv 4 64)))
			(cond
				[(not (equal? rd 0))
					(gprs-set-x! m rd save)])
			(set-pc! m adj_addr)
			instr]

		; R Format
		[(eq? opcode 'add)
			; (printf " --> add ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvadd v_rs1 v_rs2))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'sub)
			; (printf " --> sub ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvsub v_rs1 v_rs2))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'sll)
			; (printf " --> sll ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvshl v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'slt)
			; (printf " --> slt ~n")
				; TODO: slt instruction not implemented yet
				(illegal-instr m)]
		[(eq? opcode 'sltu)
			; (printf " --> sltu ~n")
			; TODO: sltu instruction not implemented yet
				(illegal-instr m)]
		[(eq? opcode 'xor)
			; (printf " --> xor ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvxor v_rs1 v_rs2))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'srl)
			; (printf " --> srl ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvlshr v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'sra)
			; (printf " --> sra ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(define shifted (bvashr v_rs1 v_rs2))
			(gprs-set-x! m rd shifted)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'or)
			; (printf " --> or ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvor v_rs1 v_rs2))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'and)
			; (printf " --> and ~n")
			(define rd (list-ref-nat instr 1))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 2)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 3)))
			(gprs-set-x! m rd (bvand v_rs1 v_rs2))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'andw)
			; (printf " --> andw ~n")
			; TODO: andw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'subw)
			; (printf " --> subw ~n")
			; TODO: subw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'sllw)
			; (printf " --> sllw ~n")
			; TODO: sllw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'srlw)
			; (printf " --> srlw ~n")
			; TODO: srlw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'sraw)
			; (printf " --> sraw ~n")
			; TODO: sraw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mul)
			; (printf " --> mul ~n")
			; TODO: mul instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mulh)
			; (printf " --> mulh ~n")
			; TODO: mulh instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mulhsu)
			; (printf " --> mulhsu ~n")
			; TODO: mulhsu instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mulhu)
			; (printf " --> mulhu ~n")
			; TODO: mulhu instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'div)
			; (printf " --> div ~n")
			; TODO: div instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'divu)
			; (printf " --> divu ~n")
			; TODO: divu instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'rem)
			; (printf " --> rem ~n")
			; TODO: rem instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'remu)
			; (printf " --> remu ~n")
			; TODO: remu instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'mulw)
			; (printf " --> mulw ~n")
			; TODO: mulw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'divw)
			; (printf " --> divw ~n")
			; TODO: divw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'divuw)
			; (printf " --> divuw ~n")
			; TODO: divuw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'remw)
			; (printf " --> remw ~n")
			; TODO: remw instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'remuw)
			; (printf " --> remuw ~n")
			; TODO: remuw instruction not implemented yet
			(illegal-instr m)]

		; B Format
		[(eq? opcode 'beq)
			; (printf " --> beq ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref instr 3))
			(if (equal? v_rs1 v_rs2)
				(set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
				(set-pc! m (bvadd pc (bv 4 64))))
			instr]
		[(eq? opcode 'bne)
			; (printf " --> bne ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref instr 3))
			(if (not (equal? v_rs1 v_rs2))
				(set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
				(set-pc! m (bvadd pc (bv 4 64))))
			instr]
		[(eq? opcode 'blt)
			; (printf " --> blt ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref instr 3))
			(if (bvslt v_rs1 v_rs2)
				(set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
				(set-pc! m (bvadd pc (bv 4 64))))
			instr]
		[(eq? opcode 'bge)
			; (printf " --> bge ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref instr 3))
			(if (bvsge v_rs1 v_rs2)
				(set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
				(set-pc! m (bvadd pc (bv 4 64))))
			instr]
		[(eq? opcode 'bltu)
			; (printf " --> bltu ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (list-ref instr 3))
			(if (bvult v_rs1 v_rs2)
				(set-pc! m (bvadd pc (bvmul (sign-extend imm (bitvector 64)) (bv 2 64))))
				(set-pc! m (bvadd pc (bv 4 64))))
			instr]
		[(eq? opcode 'bgeu)
			; (printf " --> bgeu ~n")
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
			; (printf " --> lui ~n")
			(define rd (list-ref-nat instr 1))
			; extend immediate by 12 bits
			(define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
			(gprs-set-x! m rd imm)
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]
		[(eq? opcode 'auipc)
			; (printf " --> auipc ~n")
			(define rd (list-ref-nat instr 1))
			; extend immediate by 12 bits, then zero-extend to 64 bits
			(define imm (zero-extend (concat (list-ref instr 2) (bv 0 12)) (bitvector 64)))
			(gprs-set-x! m rd (bvadd pc base_address imm))
			(set-pc! m (bvadd pc (bv 4 64)))
			instr]

		; S Format
		[(eq? opcode 'sb)
			; (printf " --> sb ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbits 8)
			(define success (machine-ram-write! m adj_addr v_rs2 nbits))
			(cond
				[(not success)
					(illegal-instr m)]
				[else
					(set-pc! m (bvadd pc (bv 4 64)))
					instr])]
		[(eq? opcode 'sh)
			; (printf " --> sh ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbits 16)
			(define success (machine-ram-write! m adj_addr v_rs2 nbits))
			(cond
				[(not success)
					(illegal-instr m)]
				[else
					(set-pc! m (bvadd pc (bv 4 64)))
					instr])]
		[(eq? opcode 'sw)
			; (printf " --> sw ~n")
			(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbits 32)
			(define success (machine-ram-write! m adj_addr v_rs2 nbits))
			(cond
				[(not success)
					(illegal-instr m)]
				[else
					(set-pc! m (bvadd pc (bv 4 64)))
					instr])]
		[(eq? opcode 'sd)
			; (printf " --> sd ~n")
			(define v_rs1 (gprs-get-x m (list-ref-nat instr 1)))
			(define v_rs2 (gprs-get-x m (list-ref-nat instr 2)))
			(define imm (sign-extend (list-ref instr 3) (bitvector 64)))
			(define addr (bvadd v_rs1 imm))
			(define adj_addr (bvsub addr base_address))
			(define nbits 64)
			(define success (machine-ram-write! m adj_addr v_rs2 nbits))
			(cond
				[(not success)
					(illegal-instr m)]
				[else
					(set-pc! m (bvadd pc (bv 4 64)))
					instr])]
		; J Format
		[(eq? opcode 'jal)
			; (printf " --> jal ~n")
			(define rd (list-ref-nat instr 1))
			(define imm (sign-extend (concat (list-ref instr 2) (bv 0 1)) (bitvector 64)))
			; adjust for base_address
			(define save_addr (bvadd (bvadd pc (bv 4 64)) base_address))
			; imm is the offset from pc, so we don't need to do anything with base_address
			(define jump_addr (bvadd imm pc))
			(cond
				[(not (equal? rd 0))
					(gprs-set-x! m rd save_addr)])
			(set-pc! m jump_addr)
			instr]

		; FENCE Format
		[(eq? opcode 'FENCE)
			; (printf " --> FENCE ~n")
			; TODO: FENCE instruction not implemented yet
			(illegal-instr m)]
		[(eq? opcode 'FENCE_I)
			; (printf " --> FENCE_I ~n")
			; TODO: FENCE_I instruction not implemented yet
			(illegal-instr m)]
		[else
			(illegal-instr m)])
)
(provide execute)
