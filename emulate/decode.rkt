#lang rosette/safe

(require (only-in racket/base error))

; decode a 32 bit vector instruction
(define (decode b_instr)
    (printf "decoding: ~a~n" b_instr)
    (define instr null)
    (define opcode (extract 6 0 b_instr))
    (printf "opcode: ~v~n" opcode)

    ; go back and match each of the opcodes to each of the expressions,
    ; ask if there is a nice way to do this
    (cond
        ; R format
        [(bveq opcode (bv #b0110011 7))
            (printf "> R format~n")
            (define op null)
            (define rd (extract 11 7 b_instr))
            (define funct3 (extract 14 12 b_instr))
            (define rs1 (extract 19 15 b_instr))
            (define funct7 (extract 31 25 b_instr))
            (define rs2 (extract 24 20 b_instr))
            (cond
                [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0000000 7)))
                    (set! op "add")]
                [(and (bveq funct3 (bv #b000 3)) (bveq funct7 (bv #b0100000 7)))
                    (set! op "sub")]
                [else (error "no such op exists")])
            (set! instr (list op rd rs1 rs2))]
        ; I format
        [(bveq opcode (bv #b0010011 7))
            (printf "> I format~n")
            (define op null)
            (define rd (extract 11 7 b_instr))
            (define funct3 (extract 14 12 b_instr))
            (define rs1 (extract 19 15 b_instr))
            (define imm (extract 31 20 b_instr))
            (cond
                [(bveq funct3 (bv #b000 3))
                    (set! op "addi")]
                [else (error "no such op exists")])
            (set! instr (list op rd rs1 imm))]
        ; S format

        ; SB format
        [(bveq opcode (bv #b1100011 7))
            (printf "> SB format~n")
            (define op null)
            (define funct3 (extract 14 12 b_instr))
            (define rs1 (extract 19 15 b_instr))
            (define rs2 (extract 24 20 b_instr))
            ; append upper imm and lower imm into imm
            (define imm (concat (extract 11 7 b_instr) (extract 31 25 b_instr)))
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
            (set! instr (list op rs1 rs2 imm))]

        ; U format
        [(bveq opcode (bv #b0110111 7))
            (printf "> U (LUI) format~n")
            (define op "lui")
            (define rd (extract 11 7 b_instr))
            (define imm (extract 31 12 b_instr))
            (set! instr (list op rd imm))
            ]
        [(bveq opcode (bv #b0010111 7))
            (printf "> U (AUIPC) format~n")]
        ; UJ 
        [(bveq opcode (bv #b1101111 7))
            (printf "UJ (JAL) format~n")]

        ; Speical Case for MRETs
        [(bveq b_instr (bv #b00110000001000000000000001110011 32))
            (printf "MRET~n")
            (set! instr (list "mret"))]
        [else (error "format does not match any known formats")])
    instr)
(provide decode)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define instr (decode b_instr))
; (printf "~a~n" instr)