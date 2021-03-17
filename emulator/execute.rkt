#lang rosette/safe

(require
  "fmt.rkt"
  "machine.rkt"
  "parameters.rkt"
  "instr.rkt")

; Decode and execute all of the binary instructions and instruction as list

(define (execute-R m b_instr)
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define rs2 (extract 24 20 b_instr))
  (define funct7 (extract 31 25 b_instr))
  (cond
    [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0000000 7)))
     (add-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0100000 7)))
     (sub-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b001 3)) (bveq funct7 (bv #b0000000 7)))
     (sll-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b010 3)) (bveq funct7 (bv #b0000000 7)))
     (slt-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b011 3)) (bveq funct7 (bv #b0000000 7)))
     (sltu-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b100 3)) (bveq funct7 (bv #b0000000 7)))
     (xor-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b101 3)) (bveq funct7 (bv #b0000000 7)))
     (srl-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b101 3)) (bveq funct7 (bv #b0100000 7)))
     (sra-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b110 3)) (bveq funct7 (bv #b0000000 7)))
     (or-instr m rd rs1 rs2)]
    [(and (bveq funct3 (bv #b111 3)) (bveq funct7 (bv #b0000000 7)))
     (and-instr m rd rs1 rs2)]
    [else
     ; (printf "No such R FMT ~n")
     'illegal-instruction]))

(define (execute-I m b_instr)
  ; TODO Could group by opcode first and then check for funct3
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define imm (extract 31 20 b_instr))
  (define shift_type (extract 30 30 b_instr))
  (cond
    ; Move some instructions to the top for potentially faster speed (less branches to check)
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0000011 7)))
     (lb-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0000011 7)))
     (lh-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0000011 7)))
     (lw-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0000011 7)))
     (ld-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b100 3)) (bveq opcode (bv #b0000011 7)))
     (lbu-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0000011 7)))
     (lhu-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b0000011 7)))
     (lwu-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b1100111 7)))
     (jalr-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0010011 7)))
     (addi-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0010011 7)))
     (slti-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0010011 7)))
     (sltiu-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b100 3)) (bveq opcode (bv #b0010011 7)))
     (xori-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b0010011 7)))
     (ori-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b111 3)) (bveq opcode (bv #b0010011 7)))
     (andi-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0010011 7)))
     ; TODO: Check if supposed to be 24?
     (slli-instr m rd rs1 (extract 24 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b0 1)))
     (srli-instr m rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b1 1)))
     (srai-instr m rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0011011 7)))
     ; TODO: Check if supposed to be normal imm
     (addiw-instr m rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0011011 7)))
     (slliw-instr m rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b0 1)))
     (srliw-instr m rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b1 1)))
     (sraiw-instr m rd rs1 (extract 25 20 b_instr))]
    [else
     ; (printf "No such I FMT ~n")
     'illegal-instruction]))

(define (execute-B m b_instr)
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
     (beq-instr m rs1 rs2 imm)]
    [(bveq funct3 (bv #b001 3))
     (bne-instr m rs1 rs2 imm)]
    [(bveq funct3 (bv #b100 3))
     (blt-instr m rs1 rs2 imm)]
    [(bveq funct3 (bv #b101 3))
     (bge-instr m rs1 rs2 imm)]
    [(bveq funct3 (bv #b110 3))
     (bltu-instr m rs1 rs2 imm)]
    [(bveq funct3 (bv #b111 3))
     (bgeu-instr m rs1 rs2 imm)]
    [else
     ; (printf "No such B FMT ~n")
     'illegal-instruction]))

(define (execute-U m b_instr)
  (define opcode (extract 6 0 b_instr))
  ; append upper imm and lower imm into imm
  (define rd (extract 11 7 b_instr))
  (define imm (extract 31 12 b_instr))
  (cond
    [(bveq opcode (bv #b0110111 7))
     (lui-instr m rd imm)]
    [(bveq opcode (bv #b0010111 7))
     (auipc-instr m rd imm)]
    [else
     ; (printf "No such U FMT ~n")
     'illegal-instruction]))

(define (execute-S m b_instr)
  (define opcode (extract 6 0 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define rs2 (extract 24 20 b_instr))
  (define imm (concat
                (extract 31 25 b_instr)
                (extract 11 7 b_instr)))
  (cond
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0100011 7)))
     (sb-instr m rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0100011 7)))
     (sh-instr m rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0100011 7)))
     (sw-instr m rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0100011 7)))
     (sd-instr m rs1 rs2 imm)]
    [else
     ; (printf "No such S FMT ~n")
     'illegal-instruction]))

(define (execute-J m b_instr)
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define imm (concat
                (extract 31 31 b_instr)
                (extract 19 12 b_instr)
                (extract 20 20 b_instr)
                (extract 30 21 b_instr)))
  (cond
    [(bveq opcode (bv #b1101111 7))
     (jal-instr m rd imm)]
    [else
     ; (printf "No such J FMT ~n")
     'illegal-instruction]))

(define (execute-SYSTEM m b_instr)
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (cond
    [(bveq funct3 (bv #b000 3))
     (define funct12 (extract 31 20 b_instr))
     (cond
       [(bveq funct12 (bv #b001100000010 12))
        (mret-instr m)]
       [(bveq funct12 (bv #b000000000010 12))
        (uret-instr m)]
       [(bveq funct12 (bv #b000000000000 12))
        (ecall-instr m)]
       [(bveq funct12 (bv #b000000000001 12))
        (ebreak-instr m)]
       [else 
        ; (printf "No such SYSTEM FMT ~n")
        'illegal-instruction])]
    [else
     (define csr (extract 31 20 b_instr))
     (define sym_csr (decode-csr csr))
     (cond
       [(bveq funct3 (bv #b001 3))
        (csrrw-instr m rd rs1 sym_csr)]
       [(bveq funct3 (bv #b010 3))
        (csrrs-instr m rd rs1 sym_csr)]
       [(bveq funct3 (bv #b011 3))
        (csrrc-instr m rd rs1 sym_csr)]
       [(bveq funct3 (bv #b101 3))
        (csrrwi-instr m rd rs1 sym_csr)]
       [(bveq funct3 (bv #b110 3))
        (csrrsi-instr m rd rs1 sym_csr)]
       [(bveq funct3 (bv #b111 3))
        (csrrci-instr m rd rs1 sym_csr)]
       [else
        ; (printf "No such SYSTEM FMT ~n")
        'illegal-instruction])]))

; Execute a 32 bit instruction
(define (execute m b_instr)
  (define opcode (extract 6 0 b_instr))
  (define fmt (get-fmt opcode))
  (cond
    [(eq? fmt 'R) (execute-R m b_instr)]
    [(eq? fmt 'I) (execute-I m b_instr)]
    [(eq? fmt 'B) (execute-B m b_instr)]
    [(eq? fmt 'U) (execute-U m b_instr)]
    [(eq? fmt 'S) (execute-S m b_instr)]
    [(eq? fmt 'J) (execute-J m b_instr)]
    [(eq? fmt 'SYSTEM) (execute-SYSTEM m b_instr)]
    [else
     ; (printf "No such FMT ~n")
     'illegal-instruction]))
(provide execute)

(define (decode-csr b_csr)
  ; (printf "CSR: ~a~n" b_csr)
  (cond
    [(bveq b_csr (bv #x000 12)) 'ustatus]
    [(bveq b_csr (bv #x004 12)) 'uie]
    [(bveq b_csr (bv #x005 12)) 'utevc]
    [(bveq b_csr (bv #x040 12)) 'uscratch]
    [(bveq b_csr (bv #x041 12)) 'uepc]
    [(bveq b_csr (bv #x042 12)) 'ucause]
    [(bveq b_csr (bv #x043 12)) 'ubadaddr]
    [(bveq b_csr (bv #x044 12)) 'uip]
    [(bveq b_csr (bv #x300 12)) 'mstatus]
    [(bveq b_csr (bv #x301 12)) 'misa]
    [(bveq b_csr (bv #x302 12)) 'medeleg]
    [(bveq b_csr (bv #x303 12)) 'mideleg]
    [(bveq b_csr (bv #x304 12)) 'mie]
    [(bveq b_csr (bv #x305 12)) 'mtvec]
    [(bveq b_csr (bv #x340 12)) 'mscratch]
    [(bveq b_csr (bv #x341 12)) 'mepc]
    [(bveq b_csr (bv #x342 12)) 'mcause]
    [(bveq b_csr (bv #x343 12)) 'mbadaddr]
    [(bveq b_csr (bv #x344 12)) 'mip]
    [(bveq b_csr (bv #x3A0 12)) 'pmpcfg0]
    [(bveq b_csr (bv #x3A1 12)) 'pmpcfg1]
    [(bveq b_csr (bv #x3A2 12)) 'pmpcfg2]
    [(bveq b_csr (bv #x3A3 12)) 'pmpcfg3]
    [(bveq b_csr (bv #x3B0 12)) 'pmpaddr0]
    [(bveq b_csr (bv #x3B1 12)) 'pmpaddr1]
    [(bveq b_csr (bv #x3B2 12)) 'pmpaddr2]
    [(bveq b_csr (bv #x3B3 12)) 'pmpaddr3]
    [(bveq b_csr (bv #x3B4 12)) 'pmpaddr4]
    [(bveq b_csr (bv #x3B5 12)) 'pmpaddr5]
    [(bveq b_csr (bv #x3B6 12)) 'pmpaddr6]
    [(bveq b_csr (bv #x3B7 12)) 'pmpaddr7]
    [(bveq b_csr (bv #x3B8 12)) 'pmpaddr8]
    [(bveq b_csr (bv #x3B9 12)) 'pmpaddr9]
    [(bveq b_csr (bv #x3BA 12)) 'pmpaddr10]
    [(bveq b_csr (bv #x3BB 12)) 'pmpaddr11]
    [(bveq b_csr (bv #x3BC 12)) 'pmpaddr12]
    [(bveq b_csr (bv #x3BD 12)) 'pmpaddr13]
    [(bveq b_csr (bv #x3BE 12)) 'pmpaddr14]
    [(bveq b_csr (bv #x3BF 12)) 'pmpaddr15]
    [else
     ; (printf "No such CSR FMT ~n")
     'illegal-instruction]))


; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (execute b_instr))
; (printf "~a~n" instr)
