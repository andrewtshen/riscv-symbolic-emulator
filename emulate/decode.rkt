#lang rosette

(require data/bit-vector)

; decode a 32 bit vector instruction
(define (decode b_instr)
    (displayln b_instr)
    (define instr null)
    (define opcode (extract 6 0 b_instr))
    (printf "~v~n" opcode)

    ; go back and match each of the opcodes to each of the expressions,
    ; ask if there is a nice way to do this
    (cond
        ; R format
        [(bveq opcode (bv #b0110011 7))
            ; use funct3 and funct7 to determine what operation it is
            (define op null)
            (define funct3 (extract 14 12 b_instr))
            (define funct7 (extract 31 25 b_instr))
            (define rs2 (extract 24 20 b_instr))
            (define rs1 (extract 19 15 b_instr))
            (define rd (extract 11 7 b_instr))
            (printf "> R format~n")
            (cond
                [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0000000 7)))
                    (set! op "add")]
                [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0100000 7)))
                    (set! op "sub")])
            (set! instr (list op rd rs1 rs2))
            ]
        ; I format

        ; S format

        ; B format
        [(bveq opcode (bv #b1100011 7))
            (define op null)
            (define funct3 (extract 14 12 b_instr))
            (define rs2 (extract 24 20 b_instr))
            (define rs1 (extract 19 15 b_instr))
            (define upp_imm (extract 31 25 b_instr))
            (define low_imm (extract 11 7 b_instr))
            ; append upper imm and lower imm into imm
            (define imm (string->bit-vector 
                (string-append
                    (bit-vector->string upp_imm)
                    (bit-vector->string low_imm))))
            (printf "> SB format~n")
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
                )
            (set! instr (list op rs1 rs2 imm))
            ]

        ; U format
        [(bveq opcode (bv #b0110111 7))
            (printf "> U (LUI) format~n")]
        [(bveq opcode (bv #b0010111 7))
            (printf "> U (AUIPC) format~n")]
        ; UJ 
        [(bveq opcode(bv #b1101111 7))
            (printf "UJ (JAL) format~n")]
        [else (error "format does not match any known formats")])
    instr)

(define b_instr (bv #b00000000011100110000001010110011 32))
(define instr (decode b_instr))
(displayln instr)

; TODO for tomorrow, figure out how to join the rest of the
; rs2, rs1 and rd together to create the ientire instruction set,
; try to complete other instructions to make it work with more things.