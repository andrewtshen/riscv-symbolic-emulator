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
     (slli-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b0 1)))
     (srli-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b1 1)))
     (srai-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0011011 7)))
     (slliw-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b0 1)))
     (srliw-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b1 1)))
     (sraiw-instr m rd rs1 (extract 5 0 imm))]
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0011011 7)))
     (addiw-instr m rd rs1 imm)]
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
     (cond
       [(bveq funct3 (bv #b001 3))
        (csrrw-instr m rd rs1 csr)]
       [(bveq funct3 (bv #b010 3))
        (csrrs-instr m rd rs1 csr)]
       [(bveq funct3 (bv #b011 3))
        (csrrc-instr m rd rs1 csr)]
       [(bveq funct3 (bv #b101 3))
        (csrrwi-instr m rd rs1 csr)]
       [(bveq funct3 (bv #b110 3))
        (csrrsi-instr m rd rs1 csr)]
       [(bveq funct3 (bv #b111 3))
        (csrrci-instr m rd rs1 csr)]
       [else
        ; (printf "No such SYSTEM FMT ~n")
        'illegal-instruction])]))

(define (execute-FENCE m b_instr)
  (define funct3 (extract 14 12 b_instr))
  ; TODO: Fill in details for these instructions (currently act as NOP)
  (cond
    [(bveq funct3 (bv #b000 3))
     (FENCE-instr m)]
    [(bveq funct3 (bv #b001 3))
     (FENCE_I-instr m)]))

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
    [(eq? fmt 'FENCE)
     (execute-FENCE m b_instr)]
    [else
     ; (printf "No such FMT ~n")
     'illegal-instruction]))
(provide execute)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (execute b_instr))
; (printf "~a~n" instr)
