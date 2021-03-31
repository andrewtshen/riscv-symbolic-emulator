#lang rosette/safe

(require
  "fmt.rkt"
  "machine.rkt"
  "parameters.rkt"
  "instr.rkt"
  "c-instr.rkt"
  "gprs.rkt")

; Decode and execute all of the binary instructions and instruction as list

(define (adj-reg reg)
  (bvadd (zero-extend reg (bitvector 5)) (bv 8 5)))

; Execute a 32 bit compressed instruction
(define (c-execute m b_instr)
  (define opcode (extract 1 0 b_instr))
  (define funct3 (extract 15 13 b_instr))

  ; // Compressed instructions have 3-bit field for popular registers, which correspond to
  ; // registers x8 to x15.
  (cond
    [(bveq opcode (bv 0 2))
     (cond
       [(bveq funct3 (bv 0 3))
        ; c.addi4spn
        ; Expands to addi rd, x2, nzuimm, where rd=rd'+8.
        (define adj-rd (adj-reg (extract 4 2 b_instr)))
        (define nzuimm 
          (zero-extend
            (concat
              (extract 10 7 b_instr)
              (extract 12 11 b_instr)
              (extract 5 5 b_instr)
              (extract 6 6 b_instr)
              (bv #b00 2))
            (bitvector 64)))
        (cond
          [(not (bveq nzuimm (bv 0 64)))
           (c.addi-instr m adj-rd X2 nzuimm)]
          [else
           'illegal-instruction])]
       [(bveq funct3 (bv 1 3))
        ; c.fld
        ; Expands to fld rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        ; TODO: Implement Regular FLD First

        ; let rd = ((inst >> 2) & 0x7) + 8;
        ; let rs1 = ((inst >> 7) & 0x7) + 8;
        ; // offset[5:3|7:6] = isnt[12:10|6:5]
        ; let offset = ((inst << 1) & 0xc0) // imm[7:6]
        ;     | ((inst >> 7) & 0x38); // imm[5:3]
        ; let val = f64::from_bits(
        ;     self.read(self.xregs.read(rs1).wrapping_add(offset), DOUBLEWORD)?,
        ; );
        ; self.fregs.write(rd, val);
        'illegal-instruction]
       [(bveq funct3 (bv 2 3))
        ; c.lw
        ; Expands to lw rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        (define adj-rd (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        (define imm
          (zero-extend
            (concat
              (extract 5 5 b_instr)
              (extract 12 10 b_instr)
              (extract 6 6 b_instr)
              (bv #b00 2))
          (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) adj-rs1)))
        (c.lw-instr m adj-rd offset)]
       [(bveq funct3 (bv 3 3))
        ; c.ld
        ; Expands to ld rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        (define adj-rd (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        (define imm
          (zero-extend 
            (concat
              (extract 6 5 b_instr)
              (extract 12 10 b_instr)
              (bv #b000 3))
            (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) adj-rs1)))
        (c.ld-instr m adj-rd offset)]
       [(bveq funct3 (bv #b100 3))
        ; // Reserved.
        ; panic!("reserved");
        ; TODO: Figure out what to do here
        'illegal-instruction]
       [(bveq funct3 (bv #b101 3))
        ; c.fsd
        ; Expands to fsd rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        ; TODO: Implement regular FSD

        ; let rs2 = ((inst >> 2) & 0x7) + 8;
        ; let rs1 = ((inst >> 7) & 0x7) + 8;
        ; // offset[5:3|7:6] = isnt[12:10|6:5]
        ; let offset = ((inst << 1) & 0xc0) // imm[7:6]
        ;     | ((inst >> 7) & 0x38); // imm[5:3]
        ; let addr = self.xregs.read(rs1).wrapping_add(offset);
        ; self.write(addr, self.fregs.read(rs2).to_bits() as u64, DOUBLEWORD)?;
        'illegal-instruction]
       [(bveq funct3 (bv #b110 3))
        ; c.sw
        ; Expands to sw rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        (define adj-rs2 (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        (define imm
          (zero-extend
            (concat
              (extract 5 5 b_instr)
              (extract 12 10 b_instr)
              (extract 6 6 b_instr)
              (bv #b00 2))
          (bitvector 64)))
        ; let addr = self.xregs.read(rs1).wrapping_add(offset);
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) adj-rs1)))
        (c.sw-instr m adj-rs2 offset)]
       [(bveq funct3 (bv #b111 3))
        ; c.sd
        ; Expands to sd rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        (define adj-rs2 (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        (define imm
          (zero-extend
            (concat
              (extract 6 5 b_instr)
              (extract 12 10 b_instr)
              (bv #b000 3))
          (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) adj-rs1)))
        (c.sd-instr m adj-rs2 offset)]
       [else
        'illegal-instruction])]
    [(bveq opcode (bv 1 2))
     (cond
       [(bveq funct3 (bv #b000 3))
        ; c.addi
        ; Expands to addi rd, rd, nzimm.
        (define rd (extract 11 7 b_instr))
        (define nzimm 
          (concat
            (extract 12 12 b_instr)
            (extract 6 2 b_instr)))
        (cond
          [(not (bveq nzimm (bv 0 6)))
           (c.addi-instr m rd rd nzimm)]
          [else
           ; TODO: what to actually return?
           (c.nop-instr m)])]
       [(bveq funct3 (bv #b001 3))
        ; c.addiw
        ; Expands to addiw rd, rd, imm
        ; "The immediate can be zero for C.ADDIW, where this corresponds to sext.w rd"
        (define rd (extract 11 7 b_instr))
        (define imm 
          (concat
            (extract 12 12 b_instr)
            (extract 6 2 b_instr)))
        (c.addiw-instr m rd rd imm)]
       [(bveq funct3 (bv #b010 3))
        ; c.li
        ; Expands to addi rd, x0, imm.
        (define rd (extract 11 7 b_instr))
        (define imm 
          (concat
            (extract 12 12 b_instr)
            (extract 6 2 b_instr)))
        (c.addi-instr m rd X0 imm)]
       [(bveq funct3 (bv #b011 3))
        (define rd (extract 11 7 b_instr))
        (cond
          [(bveq rd X0)
           ; TODO: What to actually return?
           'illegal-instruction]
          [(bveq rd X2)
           ; c.addi16sp
           ; Expands to addi x2, x2, nzimm
           (define nzimm
             (concat (extract 12 12 b_instr)
                     (extract 4 3 b_instr)
                     (extract 5 5 b_instr)
                     (extract 2 2 b_instr)
                     (extract 6 6 b_instr)
                     (bv #b0000 4)))
           (cond
            [(not (bveq nzimm (bv 0 10)))
             (c.addi-instr m X2 X2 nzimm)]
            [else
             ; TODO: what to actually return?
             'illegal-instruction])]
          [else
           ; c.lui
           ; Expands to lui rd, nzimm.
           (define nzimm
             (concat
               (extract 12 12 b_instr)
               (extract 6 2 b_instr)))
           (cond
            [(not (bveq nzimm (bv 0 6)))
             (c.lui-instr m rd nzimm)]
            [else
             ; TODO: what to actually return?
             'illegal-instruction])])]
       [(bveq funct3 (bv #b100 3))
        (define adj-rd (adj-reg (extract 9 7 b_instr)))
        (define funct2 (extract 11 10 b_instr))
        (cond
          [(bveq funct2 (bv #b00 2))
           ; c.srli
           ; Expands to srli rd, rd, shamt, where rd=rd'+8.
           (define shamt
             (concat
               (extract 12 12 b_instr)
               (extract 6 2 b_instr)))
           (cond
            [(not (bveq shamt (bv 0 6)))
             (c.srli-instr m adj-rd adj-rd shamt)]
            [else
             ; TODO: what to actually return?
             'illegal-instruction])]
          [(bveq funct2 (bv #b01 2))
           ; c.srai
           ; Expands to srai rd, rd, shamt, where rd=rd'+8.
           (define shamt
             (concat
               (extract 12 12 b_instr)
               (extract 6 2 b_instr)))
           (cond
            [(not (bveq shamt (bv 0 6)))
             (c.srai-instr m adj-rd adj-rd shamt)]
            [else
             ; TODO: what to actually return?
             'illegal-instruction])]
          [(bveq funct2 (bv #b10 2))
          ; c.andi
          ; Expands to andi rd, rd, imm, where rd=rd'+8.
          (define shamt
             (concat
               (extract 12 12 b_instr)
               (extract 6 2 b_instr)))
          (c.andi-instr m adj-rd adj-rd shamt)]
          [(bveq funct2 (bv #b11 2))
           (define imm2 (extract 6 5 b_instr))
           (define imm1 (extract 12 12 b_instr))
           (define adj-rs2 (adj-reg (extract 4 2 b_instr)))
           (cond
             [(and (bveq imm1 (bv #b0 1)) (bveq imm2 (bv #b00 2)))
              ; c.sub
              ; Expands to sub rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.sub-instr m adj-rd adj-rd adj-rs2)]
             [(and (bveq imm1 (bv #b0 1)) (bveq imm2 (bv #b01 2)))
              ; // c.xor
              ; // Expands to xor rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.xor-instr m adj-rd adj-rd adj-rs2)]
             [(and (bveq imm1 (bv #b0 1)) (bveq imm2 (bv #b10 2)))
              ; // c.or
              ; // Expands to or rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.or-instr m adj-rd adj-rd adj-rs2)]
             [(and (bveq imm1 (bv #b0 1)) (bveq imm2 (bv #b11 2)))
              ; // c.and
              ; // Expands to and rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.and-instr m adj-rd adj-rd adj-rs2)]
             [(and (bveq imm1 (bv #b1 1)) (bveq imm2 (bv #b00 2)))
              ; // c.subw
              ; // Expands to subw rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.subw-instr m adj-rd adj-rd adj-rs2)]
             [(and (bveq imm1 (bv #b1 1)) (bveq imm2 (bv #b01 2)))
              ; // c.addw
              ; // Expands to addw rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              (c.addw-instr m adj-rd adj-rd adj-rs2)]
             [else
              'illegal-instruction])]
          [else
           'illegal-instruction])]
       [(bveq funct3 (bv #b101 3))
        ; c.j
        ; Expands to jal x0, offset.
        (define offset
          (sign-extend
            (concat
              (extract 12 12 b_instr)
              (extract 8 8 b_instr)
              (extract 10 9 b_instr)
              (extract 6 6 b_instr)
              (extract 7 7 b_instr)
              (extract 2 2 b_instr)
              (extract 11 11 b_instr)
              (extract 5 3 b_instr)
              (bv #b0 1))
            (bitvector 64)))
        (c.jal-instr m X0 offset)]
       [(bveq funct3 (bv #b110 3))
        ; // c.beqz
        ; // Expands to beq rs1, x0, offset, rs1=rs1'+8.
        (define adj-rs1 (adj-reg (extract 11 7 b_instr)))
        (define offset
          (sign-extend
            (concat
              (extract 12 12 b_instr)
              (extract 6 5 b_instr)
              (extract 2 2 b_instr)
              (extract 11 10 b_instr)
              (extract 4 3 b_instr)
              (bv #b0 1))
            (bitvector 64)))
        (c.beq-instr m adj-rs1 X0 offset)]
       [(bveq funct3 (bv #b111 3))
        ; // c.bnez
        ; // Expands to bne rs1, x0, offset, rs1=rs1'+8.
        (define adj-rs1 (adj-reg (extract 11 7 b_instr)))
        (define offset
          (sign-extend
            (concat
              (extract 12 12 b_instr)
              (extract 6 5 b_instr)
              (extract 2 2 b_instr)
              (extract 11 10 b_instr)
              (extract 4 3 b_instr)
              (bv #b0 1))
            (bitvector 64)))
        (c.bne-instr m adj-rs1 X0 offset)]
       [else
        'illegal-instruction])]
    [(bveq opcode (bv 2 2))
     (cond
       [(bveq funct3 (bv #b000 3))
        ; c.slli
        ; Expands to slli rd, rd, shamt.
        (define rd (extract 11 7 b_instr))
        (define shamt
          (concat
            (extract 12 12 b_instr)
            (extract 6 2 b_instr)))
        (cond
          [(not (bveq shamt (bv 0 6)))
           (c.slli-instr m rd rd shamt)]
          [else
           ; TODO: Find out what to do here?
           'illegal-instruction])]
       [(bveq funct3 (bv #b001 3))
        ; // c.fldsp
        ; // Expands to fld rd, offset(x2).
        ; inst_count!(self, "c.fldsp");

        ; let rd = (inst >> 7) & 0x1f;
        ; // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        ; let offset = ((inst << 4) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x20) // offset[5]
        ;     | ((inst >> 2) & 0x18); // offset[4:3]
        ; let val =
        ;     f64::from_bits(self.read(self.xregs.read(2) + offset, DOUBLEWORD)?);
        ; self.fregs.write(rd, val);
        'illegal-instruction]
       [(bveq funct3 (bv #b010 3))
        ; c.lwsp
        ; Expands to lw rd, offset(x2).
        (define rd (extract 11 7 b_instr))
        (define imm
          (zero-extend
            (concat
              (extract 3 2 b_instr)
              (extract 12 12 b_instr)
              (extract 6 4 b_instr)
              (bv #b00 2))
            (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) X2)))
        (c.lw-instr m rd offset)]
       [(bveq funct3 (bv #b011 3))
        ; c.ldsp
        ; Expands to ld rd, offset(x2).
        (define rd (extract 11 7 b_instr))
        (define imm
          (zero-extend
            (concat
              (extract 4 2 b_instr)
              (extract 12 12 b_instr)
              (extract 6 5 b_instr)
              (bv #b000 3))
            (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) X2)))
        (c.ld-instr m rd offset)]
       [(bveq funct3 (bv #b100 3))
        (define funct1 (extract 12 12 b_instr))
        (define funct5 (extract 6 2 b_instr))
        (cond
          [(and (bveq funct1 (bv #b0 1)) (bveq funct5 (bv #b00000 5)))
           ; c.jr
           ; Expands to jalr x0, rs1, 0.
           (define rs1 (extract 11 7 b_instr))
           (c.jalr-instr m X0 rs1 (bv 0 64))]
          [(bveq funct1 (bv #b0 1))
           ; c.mv
           ; Expands to add rd, x0, rs2.
           (define rd (extract 11 7 b_instr))
           (define rs2 (extract 6 2 b_instr))
           (c.add-instr m rd X0 rs2)]
          [(and (bveq funct1 (bv #b1 1)) (bveq funct5 (bv #b00000 5)))
           (define rd (extract 11 7 b_instr))
           (if (bveq rd (bv 0 5))
               ; c.ebreak
               ; Expands to ebreak.
               'breakpoint-exception
               (begin
                 ; c.jalr
                 ; Expands to jalr x1, rs1, 0
                 (define rs1 (extract 11 7 b_instr))
                 (c.jalr-instr m X1 rs1 (bv 0 64))))]
          [(bveq funct1 (bv #b1 1))
           ; c.add
           ; Expands to add rd, rd, rs2
           (define rd (extract 11 7 b_instr))
           (define rs2 (extract 6 2 b_instr))
           (c.add-instr m rd rd rs2)]
          [else
           'illegal-instruction])]
       [(bveq funct3 (bv #b101 3))
        ; // c.fsdsp
        ; // Expands to fsd rs2, offset(x2).
        ; inst_count!(self, "c.fsdsp");

        ; let rs2 = (inst >> 2) & 0x1f;
        ; // offset[5:3|8:6] = isnt[12:10|9:7]
        ; let offset = ((inst >> 1) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x38); // offset[5:3]
        ; let addr = self.xregs.read(2).wrapping_add(offset);
        ; self.write(addr, self.fregs.read(rs2).to_bits(), DOUBLEWORD)?;
        'illegal-instruction]
       [(bveq funct3 (bv #b110 3))
        ; c.swsp
        ; Expands to sw rs2, offset(x2).
        (define rs2 (extract 6 2 b_instr))
        (define imm
          (zero-extend
            (concat
              (extract 8 7 b_instr)
              (extract 12 9 b_instr)
              (bv #b00 2))
            (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) X2)))
        (c.sw-instr m rs2 offset)]
       [(bveq funct3 (bv #b111 3))
        ; // c.sdsp
        ; // Expands to sd rs2, offset(x2).
        (define rs2 (extract 6 2 b_instr))
        (define imm
          (zero-extend
            (concat
              (extract 9 7 b_instr)
              (extract 12 10 b_instr)
              (bv #b000 3))
            (bitvector 64)))
        (define offset (bvadd imm (get-gprs-i (machine-gprs m) X2)))
        (c.sd-instr m rs2 offset)]
       [else
        'illegal-instruction])]
    [else
     'illegal-instruction]))
(provide c-execute)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (execute b_instr))
; (printf "~a~n" instr)
