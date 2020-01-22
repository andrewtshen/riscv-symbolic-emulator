#lang rosette/safe

(require (only-in racket/base error))

; Return the instruction format for each of the opcodes.

(define (get-fmt opcode)
	(define fmt null)
	(cond
		[(equal? opcode (bv #b1100011 7)) ; BEQ BNE BLT BGE BLTU BGEU
			(set! fmt "B")]
		[(equal? opcode (bv #b1100111 7)) ; JALR
			(set! fmt "I")]
		[(equal? opcode (bv #b1101111 7)) ; JAL
			(set! fmt "J")]
		[(equal? opcode (bv #b0110111 7)) ; LUI
			(set! fmt "U")]
		[(equal? opcode (bv #b0010111 7)) ; AUIPC
			(set! fmt "U")]
		[(equal? opcode (bv #b0010011 7)) ; ADDI SLLI SLTI SLTIU XORI SRLI SRAI ORI ANDI
			(set! fmt "I")]
		[(equal? opcode (bv #b0110011 7)) ; ADD SUB SLL SLT SLTU XOR SRL SRA OR AND
			(set! fmt "R")]
		[(equal? opcode (bv #b0011011 7)) ; ADDIW SLLIW SRLIW SRAIW
			(set! fmt "I")]
		[(equal? opcode (bv #b0111011 7)) ; ADDW SUBW SLLW SRLW SRAW
			(set! fmt "R")]
		[(equal? opcode (bv #b0000011 7)) ; LB LH LW LD LBU LHU LWU
			(set! fmt "I")]
		[(equal? opcode (bv #b0100011 7)) ; SB SH SW SD
			(set! fmt "S")]
		[(equal? opcode (bv #b0001111 7)) ; FENCE FENCE_I
			(set! fmt "FENCE")]
		[(equal? opcode (bv #b0110011 7)) ; MUL MULH MULHSU MULHU DIV DIVU REM REMU
			(set! fmt "R")]
		[(equal? opcode (bv #b0111011 7)) ; MULW DIVW DIVUW REMW REMUW
			(set! fmt "R")]
		[(equal? opcode (bv #b0101111 7)) ; LR_W SC_W LR_D SC_D
			(set! fmt "TODO FMT")]
		[(equal? opcode (bv #b1110011 7)) ; ECALL EBREAK URET MRET DRET SFENCE_VMA WFI 
										  ; CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI
			(set! fmt "SPECIAL")]
		[(equal? opcode (bv #b0010011 7)) ; SLLI_RV32 SRLI_RV32 SRAI_RV32
			(set! fmt "TODO FMT")]
		[(equal? opcode (bv #b1110011 7)) ; RDCYCLE RDTIME RDINSTRET RDCYCLEH RDTIMEH RDINSTRETH
			(set! fmt "TODO FMT")]
		[else
			(printf "OPCODE: ~a~n" opcode)
			(error "NO SUCH FMT")])
	fmt)
(provide get-fmt)

; example get-fmt
; (define fmt (get-fmt (bv #b1111011 7)))
; (printf "~a~n" fmt)
