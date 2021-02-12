#lang racket

(require
  "pmp.rkt"
  "fmt.rkt"
  "parameters.rkt")
(require (only-in rosette/safe
  bv bitvector bveq bvadd bvsub bvand bvor extract concat bitvector->natural))

;; Concrete PMP Check
; PMP test address ranging from saddr to eaddr 
(define (concrete-pmp-check pmp mode saddr eaddr)
  (define legal #t)
  (if (pmp-none-impl? pmp) legal
    (let
      ([legal null]
       [pmpcfg0 (pmp-pmpcfgi pmp 0)]
       [pmpcfg2 (pmp-pmpcfgi pmp 1)])
      ; Iterate through each pmpaddr and break at first matching
      (for ([i (in-range 16)])
        #:break (not (equal? legal null))
        (let*
          ([setting (if (< i 8)
                      (get-pmpicfg-setting pmpcfg0 i)
                      (get-pmpicfg-setting pmpcfg2 (- i 8)))]
           [R (pmpcfg-setting-R setting)]
           [W (pmpcfg-setting-W setting)]
           [X (pmpcfg-setting-X setting)]
           [A (pmpcfg-setting-A setting)]
           [L (pmpcfg-setting-L setting)])
          ; For now we only implement A = 3 (NAPOT)
          (define bounds 
            (cond
              [(bveq A (bv 0 2))
                 ; Unimplemented, so just return no access
                 (list #f #f)]
              [(bveq A (bv 3 2))
                (let*
                  ([pmpaddr (pmp-pmpaddri pmp i)]
                   [pmp_start (pmpaddr-startaddr pmpaddr)]
                   [pmp_end (pmpaddr-endaddr pmpaddr)]
                   ; Test the proper bounds, #t means allow access, #f means disallow access
                   [slegal (bv-between saddr pmp_start pmp_end)]
                   [elegal (bv-between eaddr pmp_start pmp_end)])
                  (list slegal elegal))]
              [else
                (list #f #f)]))

         (define slegal (list-ref bounds 0))    
         (define elegal (list-ref bounds 1)) 
         ; Check saddr and eaddr match the pmpaddri range
         (if (and slegal elegal)
             ; Check if pmpaddri is locked
             (if (not (pmp-is-locked? setting))
                 ; Check machine mode
                 (cond
                   [(equal? mode 1) (set! legal #t)]
                   [(equal? mode 0)
                    ; TODO: actually check what the access type is
                    (set! legal (and (bveq R (bv 1 1)) (bveq W (bv 1 1)) (bveq X (bv 1 1))))]
                   [else
                    ; TODO: implement other mode support
                    (set! legal #f)])
                 ; TODO: Implement locked variant of access, for now just return false (no access)
                 (set! legal #f))
             ; from earlier checks there must have been at least 1 pmpaddr active
             (when (equal? i 15) (set! legal #f)))))
      legal)))
(provide concrete-pmp-check)


;; Concrete Decode Byte Instruction

(define (concrete-decode-R b_instr)
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

(define (concrete-decode-I b_instr)
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

(define (concrete-decode-B b_instr)
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

(define (concrete-decode-U b_instr)
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

(define (concrete-decode-S b_instr)
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

(define (concrete-decode-J b_instr)
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

(define (concrete-decode-csr b_csr)
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

(define (concrete-decode-SPECIAL b_instr)
  (define is_csr #f)
  (define opcode (extract 6 0 b_instr))
  (define rd (extract 11 7 b_instr))
  (define funct3 (extract 14 12 b_instr))
  (define rs1 (extract 19 15 b_instr))
  (define csr (extract 31 20 b_instr))
  (cond
    [(bveq b_instr (bv #b00110000001000000000000001110011 32))
      (list 'mret)]
    [(bveq b_instr (bv #b00000000001000000000000001110011 32))
      (list 'uret)]
    [(bveq b_instr (bv #b00000000000000000000000001110011 32))
      (list 'ecall)]
    [(bveq b_instr (bv #b00000000000100000000000001110011 32))
      (list 'ebreak)]
    [(and (bveq funct3 (bv #b001 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrw rd rs1 sym_csr))]
    [(and (bveq funct3 (bv #b010 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrs rd rs1 sym_csr))]
    [(and (bveq funct3 (bv #b011 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrc rd rs1 sym_csr))]
    [(and (bveq funct3 (bv #b101 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrwi rd rs1 sym_csr))]
    [(and (bveq funct3 (bv #b110 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrsi rd rs1 sym_csr))]
    [(and (bveq funct3 (bv #b111 3)) (bveq opcode (bv #b1110011 7)))
      (define sym_csr (concrete-decode-csr csr))
      (if (eq? sym_csr null)
        null
        (list 'csrrci rd rs1 sym_csr))]
    [else
      ; (printf "No such SPECIAL FMT ~n")
      null]))

; concrete-decode a 32 bit vector instruction
(define (concrete-decode b_instr)
  (define opcode (extract 6 0 b_instr))
  (define fmt (concrete-get-fmt opcode))
  (cond
    [(eq? fmt 'R)
      (concrete-decode-R b_instr)]
    [(eq? fmt 'I)
      (concrete-decode-I b_instr)]
    [(eq? fmt 'B)
      (concrete-decode-B b_instr)]
    [(eq? fmt 'U)
      (concrete-decode-U b_instr)]
    [(eq? fmt 'S)
      (concrete-decode-S b_instr)]
    [(eq? fmt 'J)
      (concrete-decode-J b_instr)]
    [(eq? fmt 'SPECIAL)
      (concrete-decode-SPECIAL b_instr)]
    [else null]))
(provide concrete-decode)

(define (concrete-get-fmt opcode)
  (cond
    [(equal? opcode (bv #b1100011 7)) 'B]   ; BEQ BNE BLT BGE BLTU BGEU
    [(equal? opcode (bv #b1100111 7)) 'I]   ; JALR
    [(equal? opcode (bv #b1101111 7)) 'J]   ; JAL
    [(equal? opcode (bv #b0110111 7)) 'U]   ; LUI
    [(equal? opcode (bv #b0010111 7)) 'U]   ; AUIPC
    [(equal? opcode (bv #b0010011 7)) 'I]   ; ADDI SLLI SLTI SLTIU XORI SRLI SRAI ORI ANDI
    [(equal? opcode (bv #b0110011 7)) 'R]   ; ADD SUB SLL SLT SLTU XOR SRL SRA OR AND
    [(equal? opcode (bv #b0011011 7)) 'I]   ; ADDIW SLLIW SRLIW SRAIW
    [(equal? opcode (bv #b0111011 7)) 'R]   ; ADDW SUBW SLLW SRLW SRAW
    [(equal? opcode (bv #b0000011 7)) 'I]   ; LB LH LW LD LBU LHU LWU
    [(equal? opcode (bv #b0100011 7)) 'S]   ; SB SH SW SD
    [(equal? opcode (bv #b0001111 7)) 'FENCE]   ; FENCE FENCE_I
    [(equal? opcode (bv #b0110011 7)) 'R]   ; MUL MULH MULHSU MULHU DIV DIVU REM REMU
    [(equal? opcode (bv #b0111011 7)) 'R]   ; MULW DIVW DIVUW REMW REMUW
    [(equal? opcode (bv #b0101111 7)) 'TODOFMT] ; LR_W SC_W LR_D SC_D
    [(equal? opcode (bv #b1110011 7)) 'SPECIAL] ; ECALL EBREAK URET MRET DRET SFENCE_VMA WFI CSRRW CSRRS CSRRC CSRRWI CSRRSI CSRRCI
    [(equal? opcode (bv #b0010011 7)) 'TODOFMT] ; SLLI_RV32 SRLI_RV32 SRAI_RV32
    [(equal? opcode (bv #b1110011 7)) 'TODOFMT] ; RDCYCLE RDTIME RDINSTRET RDCYCLEH RDTIMEH RDINSTRETH
    [else null]))
(provide concrete-get-fmt)

;; Read/Write from Bytearray
(define (concrete-bytearray-read ba addr nbytes)
  (define bytes
    (for/list ([i (in-range nbytes)])
      ; adjust address for bitvector size (ramsize-log2) and index
      (define adj_addr (extract (sub1 (ramsize-log2)) 0 (bvadd addr (bv i 64))))
      (concrete-memory-read ba adj_addr)))
  ; little endian
  (apply concat (reverse bytes)))
(provide concrete-bytearray-read)

(define (concrete-memory-read mem addr)
  (if (use-fnmem)
      (mem addr)
      (vector-ref mem (bitvector->natural addr))))
(provide concrete-memory-read)
