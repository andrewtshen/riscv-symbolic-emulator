#lang rosette/safe

; Return the instruction format for each of the opcodes.

(define (get-fmt opcode)
  (cond
    [(equal? opcode (bv #b1100011 7)) 'B] 	     ; BEQ BNE BLT BGE BLTU BGEU
    [(equal? opcode (bv #b1100111 7)) 'I] 	     ; JALR
    [(equal? opcode (bv #b1101111 7)) 'J] 	     ; JAL
    [(equal? opcode (bv #b0110111 7)) 'U] 	     ; LUI
    [(equal? opcode (bv #b0010111 7)) 'U] 	     ; AUIPC
    [(equal? opcode (bv #b0010011 7)) 'I] 	     ; ADDI SLLI SLTI SLTIU XORI SRLI SRAI ORI ANDI
    [(equal? opcode (bv #b0110011 7)) 'R] 	     ; ADD SUB SLL SLT SLTU XOR SRL SRA OR AND
    [(equal? opcode (bv #b0011011 7)) 'I] 	     ; ADDIW SLLIW SRLIW SRAIW
    [(equal? opcode (bv #b0111011 7)) 'R] 	     ; ADDW SUBW SLLW SRLW SRAW
    [(equal? opcode (bv #b0000011 7)) 'I] 	     ; LB LH LW LD LBU LHU LWU
    [(equal? opcode (bv #b0100011 7)) 'S] 	     ; SB SH SW SD
    [(equal? opcode (bv #b0001111 7)) 'FENCE] 	 ; FENCE FENCE_I
    [(equal? opcode (bv #b0110011 7)) 'R] 	     ; MUL MULH MULHSU MULHU DIV DIVU REM REMU
    [(equal? opcode (bv #b0111011 7)) 'R] 	     ; MULW DIVW DIVUW REMW REMUW
    [(equal? opcode (bv #b0101111 7)) 'TODOFMT]  ; LR_W SC_W LR_D SC_D
    [(equal? opcode (bv #b1110011 7)) 'SYSTEM]   ; ECALL EBREAK URET MRET DRET SFENCE_VMA WFI CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI
    [(equal? opcode (bv #b0010011 7)) 'TODOFMT]  ; SLLI_RV32 SRLI_RV32 SRAI_RV32
    [(equal? opcode (bv #b1110011 7)) 'TODOFMT]  ; RDCYCLE RDTIME RDINSTRET RDCYCLEH RDTIMEH RDINSTRETH
    [else null]))
(provide get-fmt)

; Example get-fmt
; (define fmt (get-fmt (bv #b1111011 7)))
; (printf ~a~n 'fmt)
