#lang rosette/safe

(require
	"machine.rkt")

; Return the instruction format for each of the opcodes.

(define (get-fmt m opcode)
	(cond
		; [(equal? opcode (bv #b1100011 7)) ; BEQ BNE BLT BGE BLTU BGEU
		; 	'B]
		[(equal? opcode (bv #b1100111 7)) ; JALR
			'I]
		; [(equal? opcode (bv #b1101111 7)) ; JAL
		; 	'J]
		; [(equal? opcode (bv #b0110111 7)) ; LUI
		; 	'U]
		; [(equal? opcode (bv #b0010111 7)) ; AUIPC
		; 	'U]
		; [(equal? opcode (bv #b0010011 7)) ; ADDI SLLI SLTI SLTIU XORI SRLI SRAI ORI ANDI
		; 	'I]
		; [(equal? opcode (bv #b0110011 7)) ; ADD SUB SLL SLT SLTU XOR SRL SRA OR AND
		; 	'R]
		; [(equal? opcode (bv #b0011011 7)) ; ADDIW SLLIW SRLIW SRAIW
		; 	'I]
		; [(equal? opcode (bv #b0111011 7)) ; ADDW SUBW SLLW SRLW SRAW
		; 	'R]
		; [(equal? opcode (bv #b0000011 7)) ; LB LH LW LD LBU LHU LWU
		; 	'I]
		; [(equal? opcode (bv #b0100011 7)) ; SB SH SW SD
		; 	'S]
		; [(equal? opcode (bv #b0001111 7)) ; FENCE FENCE_I
		; 	'FENCE]
		; [(equal? opcode (bv #b0110011 7)) ; MUL MULH MULHSU MULHU DIV DIVU REM REMU
		; 	'R]
		; [(equal? opcode (bv #b0111011 7)) ; MULW DIVW DIVUW REMW REMUW
		; 	'R]
		; [(equal? opcode (bv #b0101111 7)) ; LR_W SC_W LR_D SC_D
		; 	'TODOFMT]
		; [(equal? opcode (bv #b1110011 7)) ; ECALL EBREAK URET MRET DRET SFENCE_VMA WFI CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI
		; 	'SPECIAL]
		; [(equal? opcode (bv #b0010011 7)) ; SLLI_RV32 SRLI_RV32 SRAI_RV32
		; 	'TODOFMT]
		; [(equal? opcode (bv #b1110011 7)) ; RDCYCLE RDTIME RDINSTRET RDCYCLEH RDTIMEH RDINSTRETH
		; 	'TODOFMT]
		[else
			(illegal-instr m)]))
(provide get-fmt)

; example get-fmt
; (define fmt (get-fmt m (bv #b1111011 7)))
; (printf ~a~n 'fmt)
