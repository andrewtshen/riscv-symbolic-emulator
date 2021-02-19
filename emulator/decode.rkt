#lang rosette/safe

(require
  "fmt.rkt"
  "machine.rkt"
  "parameters.rkt")

; Decode all of the binary instructions to a list similar to 
; objdump output so that it is easier to parse.

(define (decode-R b_instr)
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define rs2 (extract 24 20 b_instr))
  (define funct7 (extract 31 25 b_instr))
  (cond
    [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'add rd rs1 rs2)]
    [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0100000 7)))
     (list 'sub rd rs1 rs2)]
    [(and (bveq funct3 (bv #b001 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'sll rd rs1 rs2)]
    [(and (bveq funct3 (bv #b010 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'slt rd rs1 rs2)]
    [(and (bveq funct3 (bv #b011 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'sltu rd rs1 rs2)]
    [(and (bveq funct3 (bv #b100 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'xor rd rs1 rs2)]
    [(and (bveq funct3 (bv #b101 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'srl rd rs1 rs2)]
    [(and (bveq funct3 (bv #b101 3)) (bveq funct7 (bv #b0100000 7)))
     (list 'sra rd rs1 rs2)]
    [(and (bveq funct3 (bv #b110 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'or rd rs1 rs2)]
    [(and (bveq funct3 (bv #b111 3)) (bveq funct7 (bv #b0000000 7)))
     (list 'and rd rs1 rs2)]
    [else
     ; (printf "No such R FMT ~n")
     null]))

(define (decode-I b_instr)
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
     (list 'lb rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0000011 7)))
     (list 'lh rd rs1 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0000011 7)))
     (list 'lw rd rs1 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0000011 7)))
     (list 'ld rd rs1 imm)]
    [(and (bveq funct3 (bv #b100 3)) (bveq opcode (bv #b0000011 7)))
     (list 'lbu rd rs1 imm)]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0000011 7)))
     (list 'lhu rd rs1 imm)]
    [(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b0000011 7)))
     (list 'lwu rd rs1 imm)]
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b1100111 7)))
     (list 'jalr rd rs1 imm)]
    
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0010011 7)))
     (list 'addi rd rs1 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0010011 7)))
     (list 'slti rd rs1 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0010011 7)))
     (list 'sltiu rd rs1 imm)]
    [(and (bveq funct3 (bv #b100 3)) (bveq opcode (bv #b0010011 7)))
     (list 'xori rd rs1 imm)]
    [(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b0010011 7)))
     (list 'ori rd rs1 imm)]
    [(and (bveq funct3 (bv #b111 3)) (bveq opcode (bv #b0010011 7)))
     (list 'andi rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0010011 7)))
     ; TODO: Check if supposed to be 24?
     (list 'slli rd rs1 (extract 24 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b0 1)))
     (list 'srli rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0010011 7)) (bveq shift_type (bv #b1 1)))
     (list 'srai rd rs1 (extract 25 20 b_instr))] 
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0011011 7)))
     ; TODO: Check if supposed to be normal imm
     (list 'addiw rd rs1 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0011011 7)))
     (list 'slliw rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b0 1)))
     (list 'srliw rd rs1 (extract 25 20 b_instr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b0011011 7)) (bveq shift_type (bv #b1 1)))
     (list 'sraiw rd rs1 (extract 25 20 b_instr))]
    [else
     ; (printf "No such I FMT ~n")
     null]))

(define (decode-B b_instr)
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
     (list 'beq rs1 rs2 imm)]
    [(bveq funct3 (bv #b001 3))
     (list 'bne rs1 rs2 imm)]
    [(bveq funct3 (bv #b100 3))
     (list 'blt rs1 rs2 imm)]
    [(bveq funct3 (bv #b101 3))
     (list 'bge rs1 rs2 imm)]
    [(bveq funct3 (bv #b110 3))
     (list 'bltu rs1 rs2 imm)]
    [(bveq funct3 (bv #b111 3))
     (list 'bgeu rs1 rs2 imm)]
    [else
     ; (printf "No such B FMT ~n")
     null]))

(define (decode-U b_instr)
  (define opcode (extract 6 0 b_instr))
  ; append upper imm and lower imm into imm
  (define rd (extract 11 7 b_instr))
  (define imm (extract 31 12 b_instr))
  (cond
    [(bveq opcode (bv #b0110111 7))
     (list 'lui rd imm)]
    [(bveq opcode (bv #b0010111 7))
     (list 'auipc rd imm)]
    [else
     ; (printf "No such U FMT ~n")
     null]))

(define (decode-S b_instr)
  (define opcode (extract 6 0 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define rs2 (extract 24 20 b_instr))
  (define imm (concat
                (extract 31 25 b_instr)
                (extract 11 7 b_instr)))
  (cond
    [(and (bveq funct3 (bv #b000 3)) (bveq opcode (bv #b0100011 7)))
     (list 'sb rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b0100011 7)))
     (list 'sh rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b0100011 7)))
     (list 'sw rs1 rs2 imm)]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b0100011 7)))
     (list 'sd rs1 rs2 imm)]
    [else
     ; (printf "No such S FMT ~n")
     null]))

(define (decode-J b_instr)
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define imm (concat
                (extract 31 31 b_instr)
                (extract 19 12 b_instr)
                (extract 20 20 b_instr)
                (extract 30 21 b_instr)))
  (cond
    [(bveq opcode (bv #b1101111 7))
     (list 'jal rd imm)]
    [else
     ; (printf "No such J FMT ~n")
     null]))

(define (decode-SYSTEM b_instr)
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (cond
    [(bveq funct3 (bv #b000 3))
     (define funct12 (extract 31 20 b_instr))
     (cond
       [(bveq funct12 (bv #b001100000010 12))
        (list 'mret)]
       [(bveq funct12 (bv #b000000000010 12))
        (list 'uret)]
       [(bveq funct12 (bv #b000000000000 12))
        (list 'ecall)]
       [(bveq funct12 (bv #b000000000001 12))
        (list 'ebreak)]
       [else null])]
    [else
     (define csr (extract 31 20 b_instr))
     (define sym_csr (decode-csr csr))
     (cond
       [(null? sym_csr) null]
       [(bveq funct3 (bv #b001 3))
        (list 'csrrw rd rs1 sym_csr)]
       [(bveq funct3 (bv #b010 3))
        (list 'csrrs rd rs1 sym_csr)]
       [(bveq funct3 (bv #b011 3))
        (list 'csrrc rd rs1 sym_csr)]
       [(bveq funct3 (bv #b101 3))
        (list 'csrrwi rd rs1 sym_csr)]
       [(bveq funct3 (bv #b110 3))
        (list 'csrrsi rd rs1 sym_csr)]
       [(bveq funct3 (bv #b111 3))
        (list 'csrrci rd rs1 sym_csr)]
       [else null])]))

; decode a 32 bit vector instruction
(define (decode b_instr)
  (define opcode (extract 6 0 b_instr))
  (define fmt (get-fmt opcode))
  (cond
    [(eq? fmt 'R) (decode-R b_instr)]
    [(eq? fmt 'I) (decode-I b_instr)]
    [(eq? fmt 'B) (decode-B b_instr)]
    [(eq? fmt 'U) (decode-U b_instr)]
    [(eq? fmt 'S) (decode-S b_instr)]
    [(eq? fmt 'J) (decode-J b_instr)]
    [(eq? fmt 'SYSTEM) (decode-SYSTEM b_instr)]
    [else null]))
(provide decode)

(define (decode-csr b_csr)
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
     null]))

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (decode b_instr))
; (printf "~a~n" instr)
